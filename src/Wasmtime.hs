{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | High-level Haskell bindings to Wasmtime's C API.
--
-- Native resources are finalized automatically. Wasmtime errors, WebAssembly
-- traps, and invalid relationships between Haskell handles are reported as
-- @WasmtimeException@.
module Wasmtime
  ( Engine,
    Store,
    Module,
    Func,
    Memory,
    Instance,
    WasmtimeException (..),
    newEngine,
    newStore,
    compileWatModule,
    deserializeModule,
    newHostFunc0,
    instantiate,
    getFunc,
    getMemory,
    newMemory,
    memorySize,
    memoryDataSize,
    readMemoryByte,
    writeMemoryByte,
    growMemory,
    call0,
    call0I32,
    call1I32,
    call2I32,
    call2I32Unit,
  )
where

import Control.Exception (Exception, SomeException, bracket, displayException, mask_, throwIO, try)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as ByteString
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Word (Word64, Word8)
import Foreign.C.String (peekCStringLen, withCStringLen)
import Foreign.C.Types (CBool (..), CSize)
import Foreign.Concurrent qualified as Concurrent
import Foreign.ForeignPtr
  ( ForeignPtr,
    mallocForeignPtrBytes,
    newForeignPtr,
    touchForeignPtr,
    withForeignPtr,
  )
import Foreign.Marshal.Alloc (alloca, allocaBytesAligned)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullFunPtr, nullPtr, plusPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (peek, peekElemOff, poke, pokeElemOff)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)
import Wasmtime.Raw qualified as Raw

-- | A WebAssembly compilation environment and its configuration.
newtype Engine = Engine (ForeignPtr Raw.WasmEngine)

-- | The runtime state owned by one engine.
data Store = Store
  { storePointer :: ForeignPtr Raw.WasmtimeStore,
    storeContext :: Ptr Raw.WasmtimeContext,
    storeEngine :: Engine
  }

-- | A compiled WebAssembly module.
data Module = Module
  { modulePointer :: ForeignPtr Raw.WasmtimeModule,
    moduleEngine :: Engine
  }

-- | A WebAssembly or host-defined function belonging to a store.
data Func = Func
  { funcPointer :: ForeignPtr Raw.WasmtimeExtern,
    funcStore :: Store
  }

-- | A WebAssembly linear memory belonging to a store.
data Memory = Memory
  { memoryPointer :: ForeignPtr Raw.WasmtimeMemory,
    memoryStore :: Store
  }

-- | An instantiated WebAssembly module belonging to a store.
data Instance = Instance
  { instancePointer :: ForeignPtr Raw.WasmtimeInstance,
    instanceStore :: Store,
    instanceModule :: Module
  }

-- | An error reported by Wasmtime, a WebAssembly trap, or invalid use of
-- handles from different engines or stores.
newtype WasmtimeException = WasmtimeException String
  deriving stock (Show, Generic)
  deriving anyclass (Exception)

-- | Create a new engine with the default configuration.
--
-- The engine is released automatically when it is no longer reachable.
--
-- Throws @WasmtimeException@ if the engine cannot be allocated.
newEngine :: IO Engine
newEngine = do
  pointer <- Raw.wasmEngineNew
  when (pointer == nullPtr) $ throwIO (WasmtimeException "failed to create Wasmtime engine")
  Engine <$> newForeignPtr Raw.wasmEngineDeleteFinalizer pointer

-- | Create a fresh store within the specified engine.
--
-- The store has no host data attached to it. It is released automatically when
-- it is no longer reachable and keeps its engine alive for its lifetime.
--
-- Throws @WasmtimeException@ if the store cannot be allocated.
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

-- | Convert WebAssembly text format to binary and compile it into a module.
--
-- The input is parsed and validated according to the engine's configuration.
-- Neither the engine nor the input bytes are consumed, and the compiled module
-- is released automatically when it is no longer reachable.
--
-- Throws @WasmtimeException@ if the text cannot be parsed or the resulting
-- WebAssembly module cannot be compiled.
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
              checkError
                =<< Raw.wasmtimeModuleNew
                  engineRaw
                  (castPtr wasmBytes)
                  wasmSize
                  moduleOutput
              pointer <- peek moduleOutput
              foreignPointer <- newForeignPtr Raw.wasmtimeModuleDeleteFinalizer pointer
              pure Module {modulePointer = foreignPointer, moduleEngine = engine}

-- | Build a module from serialized compiled-module data.
--
-- Only pass data previously produced by a compatible Wasmtime version and
-- target. Serialized modules are not safe to accept from untrusted sources.
-- Neither the engine nor the input bytes are consumed, and the deserialized
-- module is released automatically when it is no longer reachable.
--
-- Throws @WasmtimeException@ if the data cannot be deserialized.
deserializeModule :: Engine -> ByteString -> IO Module
deserializeModule engine@(Engine enginePointer) serialized =
  ByteString.unsafeUseAsCStringLen serialized $ \(bytes, size) ->
    alloca $ \moduleOutput ->
      withForeignPtr enginePointer $ \engineRaw -> do
        checkError
          =<< Raw.wasmtimeModuleDeserialize
            engineRaw
            (castPtr bytes)
            (fromIntegral size)
            moduleOutput
        pointer <- peek moduleOutput
        foreignPointer <- newForeignPtr Raw.wasmtimeModuleDeleteFinalizer pointer
        pure Module {modulePointer = foreignPointer, moduleEngine = engine}

-- | Create a host-defined function with no parameters or results.
--
-- The function belongs to the specified store and can be supplied as an import
-- to 'instantiate'. The action is invoked each time WebAssembly calls the
-- function. If the action raises an exception, the exception is converted into
-- a WebAssembly trap; a host call that encounters it raises
-- @WasmtimeException@.
--
-- The callback's lifetime is managed automatically with the Wasmtime store.
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

-- | Instantiate a WebAssembly module with the provided imports.
--
-- Imports must be listed in the same order as the module's imports and the list
-- must have exactly the expected length. Every imported function must belong to
-- the specified store, and the module must use the store's engine. Instantiation
-- also executes the module's start function, if it has one.
--
-- Throws @WasmtimeException@ for an engine or store mismatch, a linking error,
-- or a WebAssembly trap during instantiation.
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

-- | Get a function export by name from an instance.
--
-- The instance must belong to the specified store. The returned function also
-- belongs to that store.
--
-- Throws @WasmtimeException@ if the instance belongs to a different store, the
-- export does not exist, or the export is not a function.
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

-- | Get a memory export by name from an instance.
--
-- Throws @WasmtimeException@ if the instance belongs to a different store, the
-- export does not exist, or the export is not a memory.
getMemory :: Store -> Instance -> String -> IO Memory
getMemory store wasmInstance name = do
  unless (sameStore store (instanceStore wasmInstance)) $
    throwIO (WasmtimeException "instance belongs to a different store")
  foreignPointer <- mallocForeignPtrBytes Raw.memoryBytes
  withForeignPtr (instancePointer wasmInstance) $ \instanceRaw ->
    withForeignPtr foreignPointer $ \memory ->
      allocaBytesAligned Raw.externBytes Raw.externAlignment $ \external ->
        withCStringLen name $ \(namePointer, nameLength) -> do
          CBool found <-
            Raw.wasmtimeInstanceExportGet
              (storeContext store)
              instanceRaw
              namePointer
              (fromIntegral nameLength)
              external
          unless (found /= 0) $
            throwIO (WasmtimeException ("memory export not found: " ++ name))
          kind <- Raw.externKind external
          unless (kind == Raw.externKindMemory) $
            throwIO (WasmtimeException ("export is not a memory: " ++ name))
          copyBytes memory (Raw.externMemory external) Raw.memoryBytes
  touchForeignPtr (storePointer store)
  pure Memory {memoryPointer = foreignPointer, memoryStore = store}

-- | Create a stand-alone 32-bit WebAssembly memory.
--
-- The minimum and optional maximum are expressed in WebAssembly pages.
-- Throws @WasmtimeException@ if the limits are invalid or the memory cannot be
-- allocated.
newMemory :: Store -> Word64 -> Maybe Word64 -> IO Memory
newMemory store minimumPages maximumPages =
  alloca $ \memoryTypeOutput -> do
    checkError
      =<< Raw.wasmtimeMemoryTypeNew
        minimumPages
        (maybe 0 (const 1) maximumPages)
        (fromMaybe 0 maximumPages)
        0
        0
        16
        memoryTypeOutput
    memoryType <- peek memoryTypeOutput
    bracket (pure memoryType) Raw.wasmMemoryTypeDelete $ \memoryTypeRaw -> do
      foreignPointer <- mallocForeignPtrBytes Raw.memoryBytes
      withForeignPtr foreignPointer $ \memory -> do
        errorPointer <- Raw.wasmtimeMemoryNew (storeContext store) memoryTypeRaw memory
        checkError errorPointer
      touchForeignPtr (storePointer store)
      pure Memory {memoryPointer = foreignPointer, memoryStore = store}

-- | Return the current size of a memory in WebAssembly pages.
memorySize :: Store -> Memory -> IO Word64
memorySize store memory = withMemory store memory (Raw.wasmtimeMemorySize (storeContext store))

-- | Return the current size of a memory in bytes.
memoryDataSize :: Store -> Memory -> IO Int
memoryDataSize store memory =
  fromIntegral <$> withMemory store memory (Raw.wasmtimeMemoryDataSize (storeContext store))

-- | Read one byte from linear memory.
--
-- Throws @WasmtimeException@ when the offset is out of bounds.
readMemoryByte :: Store -> Memory -> Int -> IO Word8
readMemoryByte store memory offset =
  withMemoryData store memory offset $ \bytes -> peekElemOff bytes offset

-- | Write one byte to linear memory.
--
-- Throws @WasmtimeException@ when the offset is out of bounds.
writeMemoryByte :: Store -> Memory -> Int -> Word8 -> IO ()
writeMemoryByte store memory offset value =
  withMemoryData store memory offset $ \bytes -> pokeElemOff bytes offset value

-- | Grow a memory by the specified number of WebAssembly pages and return its
-- previous size.
--
-- Throws @WasmtimeException@ if the memory cannot grow by the requested amount.
growMemory :: Store -> Memory -> Word64 -> IO Word64
growMemory store memory delta =
  withMemory store memory $ \memoryRaw ->
    alloca $ \previousSize -> do
      checkError
        =<< Raw.wasmtimeMemoryGrow (storeContext store) memoryRaw delta previousSize
      peek previousSize

-- | Call a WebAssembly function with no arguments and no results.
--
-- The function must belong to the specified store and have the corresponding
-- WebAssembly signature.
--
-- Throws @WasmtimeException@ if the store or function signature is wrong, if
-- Wasmtime reports another call error, or if execution traps.
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

-- | Call a WebAssembly function with no arguments and one @i32@ result.
call0I32 :: Store -> Func -> IO Int32
call0I32 store function = callI32 store function []

-- | Call a WebAssembly function with one @i32@ argument and one @i32@ result.
call1I32 :: Store -> Func -> Int32 -> IO Int32
call1I32 store function argument = callI32 store function [argument]

-- | Call a function with two WebAssembly @i32@ arguments and one @i32@ result.
--
-- The function must belong to the specified store and have the corresponding
-- WebAssembly signature.
--
-- Throws @WasmtimeException@ if the store or function signature is wrong, if
-- Wasmtime reports another call error, or if execution traps.
call2I32 :: Store -> Func -> Int32 -> Int32 -> IO Int32
call2I32 store function left right = callI32 store function [left, right]

-- | Call a WebAssembly function with two @i32@ arguments and no results.
call2I32Unit :: Store -> Func -> Int32 -> Int32 -> IO ()
call2I32Unit store function left right = do
  unless (sameStore store (funcStore function)) $
    throwIO (WasmtimeException "function belongs to a different store")
  withForeignPtr (funcPointer function) $ \external ->
    withI32Arguments [left, right] $ \arguments argumentCount ->
      alloca $ \trapOutput -> do
        poke trapOutput nullPtr
        errorPointer <-
          Raw.wasmtimeFuncCall
            (storeContext store)
            (Raw.externFunc external)
            arguments
            argumentCount
            nullPtr
            0
            trapOutput
        checkErrorOrTrap errorPointer =<< peek trapOutput
  touchForeignPtr (storePointer store)

callI32 :: Store -> Func -> [Int32] -> IO Int32
callI32 store function arguments = do
  unless (sameStore store (funcStore function)) $
    throwIO (WasmtimeException "function belongs to a different store")
  result <-
    withForeignPtr (funcPointer function) $ \external ->
      withI32Arguments arguments $ \argumentPointer argumentCount ->
        allocaBytesAligned Raw.valBytes Raw.valAlignment $ \results ->
          alloca $ \trapOutput -> do
            poke trapOutput nullPtr
            errorPointer <-
              Raw.wasmtimeFuncCall
                (storeContext store)
                (Raw.externFunc external)
                argumentPointer
                argumentCount
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

withI32Arguments :: [Int32] -> (Ptr Raw.WasmtimeVal -> CSize -> IO a) -> IO a
withI32Arguments [] action = action nullPtr 0
withI32Arguments arguments action =
  allocaBytesAligned (length arguments * Raw.valBytes) Raw.valAlignment $ \output -> do
    mapM_
      (\(index, value) -> Raw.setValI32 (output `plusPtr` (index * Raw.valBytes)) value)
      (zip [0 ..] arguments)
    action output (fromIntegral (length arguments))

withMemory :: Store -> Memory -> (Ptr Raw.WasmtimeMemory -> IO a) -> IO a
withMemory store memory action = do
  unless (sameStore store (memoryStore memory)) $
    throwIO (WasmtimeException "memory belongs to a different store")
  result <- withForeignPtr (memoryPointer memory) action
  touchForeignPtr (storePointer store)
  pure result

withMemoryData :: Store -> Memory -> Int -> (Ptr Word8 -> IO a) -> IO a
withMemoryData store memory offset action =
  withMemory store memory $ \memoryRaw -> do
    size <- fromIntegral <$> Raw.wasmtimeMemoryDataSize (storeContext store) memoryRaw
    when (offset < 0 || offset >= size) $
      throwIO (WasmtimeException ("memory offset out of bounds: " ++ show offset))
    bytes <- Raw.wasmtimeMemoryData (storeContext store) memoryRaw
    action bytes

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
