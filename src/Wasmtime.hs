{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Wasmtime
  ( Engine,
    Store,
    Module,
    Func,
    Instance,
    WasmtimeException (..),
    newEngine,
    newStore,
    compileWatModule,
    deserializeModule,
    newHostFunc0,
    instantiate,
    getFunc,
    call0,
    call2I32,
  )
where

import Control.Exception (Exception, SomeException, bracket, displayException, mask_, throwIO, try)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as ByteString
import Data.Int (Int32)
import Foreign.C.String (peekCStringLen, withCStringLen)
import Foreign.C.Types (CBool (..))
import Foreign.ForeignPtr
  ( ForeignPtr,
    mallocForeignPtrBytes,
    newForeignPtr,
    touchForeignPtr,
    withForeignPtr,
  )
import qualified Foreign.Concurrent as Concurrent
import Foreign.Marshal.Alloc (alloca, allocaBytesAligned)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullFunPtr, nullPtr, plusPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (peek, poke)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)
import qualified Wasmtime.Raw as Raw

newtype Engine = Engine (ForeignPtr Raw.WasmEngine)

data Store = Store
  { storePointer :: ForeignPtr Raw.WasmtimeStore,
    storeContext :: Ptr Raw.WasmtimeContext,
    storeEngine :: Engine
  }

data Module = Module
  { modulePointer :: ForeignPtr Raw.WasmtimeModule,
    moduleEngine :: Engine
  }

data Func = Func
  { funcPointer :: ForeignPtr Raw.WasmtimeExtern,
    funcStore :: Store
  }

data Instance = Instance
  { instancePointer :: ForeignPtr Raw.WasmtimeInstance,
    instanceStore :: Store,
    instanceModule :: Module
  }

newtype WasmtimeException = WasmtimeException String
  deriving stock (Show, Generic)
  deriving anyclass (Exception)

newEngine :: IO Engine
newEngine = do
  pointer <- Raw.wasmEngineNew
  when (pointer == nullPtr) $ throwIO (WasmtimeException "failed to create Wasmtime engine")
  Engine <$> newForeignPtr Raw.wasmEngineDeleteFinalizer pointer

newStore :: Engine -> IO Store
newStore engine@(Engine enginePointer) =
  withForeignPtr enginePointer $ \engineRaw -> do
    pointer <- Raw.wasmtimeStoreNew engineRaw nullPtr nullFunPtr
    when (pointer == nullPtr) $ throwIO (WasmtimeException "failed to create Wasmtime store")
    context <- Raw.wasmtimeStoreContext pointer
    foreignPointer <-
      Concurrent.newForeignPtr pointer $ do
        Raw.wasmtimeStoreDelete pointer
        touchForeignPtr enginePointer
    pure Store {storePointer = foreignPointer, storeContext = context, storeEngine = engine}

-- | Parse WebAssembly text and compile it into a module.
compileWatModule :: Engine -> ByteString -> IO Module
compileWatModule engine@(Engine enginePointer) wat =
  ByteString.unsafeUseAsCStringLen wat $ \(watBytes, watSize) ->
    allocaBytesAligned Raw.byteVecBytes Raw.byteVecAlignment $ \wasm -> do
      checkError =<< Raw.wasmtimeWat2Wasm watBytes (fromIntegral watSize) wasm
      bracket
        (pure ())
        (const (Raw.wasmByteVecDelete wasm))
        $ \() ->
          alloca $ \moduleOutput ->
            withForeignPtr enginePointer $ \engineRaw -> do
              wasmBytes <- Raw.byteVecData wasm
              wasmSize <- Raw.byteVecSize wasm
              checkError =<<
                Raw.wasmtimeModuleNew
                  engineRaw
                  (castPtr wasmBytes)
                  wasmSize
                  moduleOutput
              pointer <- peek moduleOutput
              foreignPointer <- newForeignPtr Raw.wasmtimeModuleDeleteFinalizer pointer
              pure Module {modulePointer = foreignPointer, moduleEngine = engine}

-- | Load a trusted module previously serialized by the matching Wasmtime
-- version and target. Wasmtime serialized modules must not be accepted from
-- untrusted sources.
deserializeModule :: Engine -> ByteString -> IO Module
deserializeModule engine@(Engine enginePointer) serialized =
  ByteString.unsafeUseAsCStringLen serialized $ \(bytes, size) ->
    alloca $ \moduleOutput ->
      withForeignPtr enginePointer $ \engineRaw -> do
        checkError =<<
          Raw.wasmtimeModuleDeserialize
            engineRaw
            (castPtr bytes)
            (fromIntegral size)
            moduleOutput
        pointer <- peek moduleOutput
        foreignPointer <- newForeignPtr Raw.wasmtimeModuleDeleteFinalizer pointer
        pure Module {modulePointer = foreignPointer, moduleEngine = engine}

newHostFunc0 :: Store -> IO () -> IO Func
newHostFunc0 store action = mask_ $ do
  stable <- newStablePtr action
  foreignPointer <- mallocForeignPtrBytes Raw.externBytes
  withForeignPtr foreignPointer $ \external ->
    allocaBytesAligned Raw.funcBytes Raw.funcAlignment $ \function ->
      bracket Raw.wasmFuncTypeNew0_0 Raw.wasmFuncTypeDelete $ \functionType -> do
        Raw.wasmtimeFuncNew
          (storeContext store)
          functionType
          hostCallbackPointer
          (castStablePtrToPtr stable)
          stableFinalizerPointer
          function
        Raw.setExternKindFunc external
        copyBytes (Raw.externFunc external) function Raw.funcBytes
  touchForeignPtr (storePointer store)
  pure Func {funcPointer = foreignPointer, funcStore = store}

instantiate :: Store -> Module -> [Func] -> IO Instance
instantiate store wasmModule imports = do
  ensureSameEngine store wasmModule
  ensureSameStore store imports
  foreignPointer <- mallocForeignPtrBytes Raw.instanceBytes
  withForeignPtr foreignPointer $ \instanceRaw ->
    withForeignPtr (modulePointer wasmModule) $ \moduleRaw ->
      withExternArray imports $ \externs count ->
        alloca $ \trapOutput -> do
          poke trapOutput nullPtr
          errorPointer <-
            Raw.wasmtimeInstanceNew
              (storeContext store)
              moduleRaw
              externs
              (fromIntegral count)
              instanceRaw
              trapOutput
          checkErrorOrTrap errorPointer =<< peek trapOutput
  touchForeignPtr (storePointer store)
  pure
    Instance
      { instancePointer = foreignPointer,
        instanceStore = store,
        instanceModule = wasmModule
      }

getFunc :: Store -> Instance -> String -> IO Func
getFunc store wasmInstance name = do
  unless (sameStore store (instanceStore wasmInstance)) $
    throwIO (WasmtimeException "instance belongs to a different store")
  foreignPointer <- mallocForeignPtrBytes Raw.externBytes
  withForeignPtr (instancePointer wasmInstance) $ \instanceRaw ->
    withForeignPtr foreignPointer $ \external ->
      withCStringLen name $ \(namePointer, nameLength) -> do
        CBool found <-
          Raw.wasmtimeInstanceExportGet
            (storeContext store)
            instanceRaw
            namePointer
            (fromIntegral nameLength)
            external
        unless (found /= 0) $
          throwIO (WasmtimeException ("function export not found: " ++ name))
        kind <- Raw.externKind external
        unless (kind == Raw.externKindFunc) $
          throwIO (WasmtimeException ("export is not a function: " ++ name))
  touchForeignPtr (storePointer store)
  pure Func {funcPointer = foreignPointer, funcStore = store}

call0 :: Store -> Func -> IO ()
call0 store function = do
  unless (sameStore store (funcStore function)) $
    throwIO (WasmtimeException "function belongs to a different store")
  withForeignPtr (funcPointer function) $ \external ->
    alloca $ \trapOutput -> do
      poke trapOutput nullPtr
      errorPointer <-
        Raw.wasmtimeFuncCall
          (storeContext store)
          (Raw.externFunc external)
          nullPtr
          0
          nullPtr
          0
          trapOutput
      checkErrorOrTrap errorPointer =<< peek trapOutput
  touchForeignPtr (storePointer store)

-- | Call a function with two WebAssembly @i32@ arguments and one @i32@ result.
call2I32 :: Store -> Func -> Int32 -> Int32 -> IO Int32
call2I32 store function left right = do
  unless (sameStore store (funcStore function)) $
    throwIO (WasmtimeException "function belongs to a different store")
  result <-
    withForeignPtr (funcPointer function) $ \external ->
      allocaBytesAligned (2 * Raw.valBytes) Raw.valAlignment $ \arguments ->
        allocaBytesAligned Raw.valBytes Raw.valAlignment $ \results ->
          alloca $ \trapOutput -> do
            Raw.setValI32 arguments left
            Raw.setValI32 (arguments `plusPtr` Raw.valBytes) right
            poke trapOutput nullPtr
            errorPointer <-
              Raw.wasmtimeFuncCall
                (storeContext store)
                (Raw.externFunc external)
                arguments
                2
                results
                1
                trapOutput
            checkErrorOrTrap errorPointer =<< peek trapOutput
            kind <- Raw.valKind results
            unless (kind == Raw.valKindI32) $
              throwIO (WasmtimeException "function result is not i32")
            Raw.valI32 results
  touchForeignPtr (storePointer store)
  pure result

withExternArray :: [Func] -> (Ptr Raw.WasmtimeExtern -> Int -> IO a) -> IO a
withExternArray functions action =
  allocaBytesAligned (length functions * Raw.externBytes) Raw.externAlignment $ \output -> do
    copy 0 functions output
    result <- action output (length functions)
    mapM_ (touchForeignPtr . funcPointer) functions
    pure result
  where
    copy _ [] _ = pure ()
    copy index (function : rest) output =
      withForeignPtr (funcPointer function) $ \source -> do
        copyBytes (output `plusBytes` (index * Raw.externBytes)) source Raw.externBytes
        copy (index + 1) rest output

    plusBytes pointer offset = castPtr (castPtr pointer `plusPtr` offset)

ensureSameEngine :: Store -> Module -> IO ()
ensureSameEngine store wasmModule =
  unless (sameEngine (storeEngine store) (moduleEngine wasmModule)) $
    throwIO (WasmtimeException "module and store use different engines")

ensureSameStore :: Store -> [Func] -> IO ()
ensureSameStore store =
  mapM_ $ \function ->
    unless (sameStore store (funcStore function)) $
      throwIO (WasmtimeException "imported function belongs to a different store")

sameEngine :: Engine -> Engine -> Bool
sameEngine (Engine left) (Engine right) = left == right

sameStore :: Store -> Store -> Bool
sameStore left right = storePointer left == storePointer right

checkError :: Ptr Raw.WasmtimeError -> IO ()
checkError pointer = unless (pointer == nullPtr) (throwError pointer)

checkErrorOrTrap :: Ptr Raw.WasmtimeError -> Ptr Raw.WasmTrap -> IO ()
checkErrorOrTrap errorPointer trapPointer
  | errorPointer /= nullPtr = throwError errorPointer
  | trapPointer /= nullPtr = throwTrap trapPointer
  | otherwise = pure ()

throwError :: Ptr Raw.WasmtimeError -> IO a
throwError pointer = do
  message <- readMessage (Raw.wasmtimeErrorMessage pointer)
  Raw.wasmtimeErrorDelete pointer
  throwIO (WasmtimeException message)

throwTrap :: Ptr Raw.WasmTrap -> IO a
throwTrap pointer = do
  message <- readMessage (Raw.wasmTrapMessage pointer)
  Raw.wasmTrapDelete pointer
  throwIO (WasmtimeException message)

readMessage :: (Ptr Raw.WasmByteVec -> IO ()) -> IO String
readMessage fill =
  allocaBytesAligned Raw.byteVecBytes Raw.byteVecAlignment $ \message ->
    bracket
      (fill message)
      (const (Raw.wasmByteVecDelete message))
      $ \() -> do
        size <- Raw.byteVecSize message
        bytes <- Raw.byteVecData message
        peekCStringLen (bytes, fromIntegral size)

hostCallback :: Raw.FuncCallback
hostCallback environment _caller _arguments _argumentCount _results _resultCount = do
  action <- deRefStablePtr (castPtrToStablePtr environment :: StablePtr (IO ()))
  outcome <- try action
  case outcome of
    Right () -> pure nullPtr
    Left exception -> exceptionTrap exception

exceptionTrap :: SomeException -> IO (Ptr Raw.WasmTrap)
exceptionTrap exception =
  withCStringLen (displayException exception) $ \(message, lengthInBytes) ->
    Raw.wasmtimeTrapNew message (fromIntegral lengthInBytes)

hostCallbackPointer :: FunPtr Raw.FuncCallback
hostCallbackPointer = unsafePerformIO (Raw.wrapFuncCallback hostCallback)
{-# NOINLINE hostCallbackPointer #-}

stableFinalizer :: Raw.Finalizer
stableFinalizer environment =
  freeStablePtr (castPtrToStablePtr environment :: StablePtr (IO ()))

stableFinalizerPointer :: FunPtr Raw.Finalizer
stableFinalizerPointer = unsafePerformIO (Raw.wrapFinalizer stableFinalizer)
{-# NOINLINE stableFinalizerPointer #-}
