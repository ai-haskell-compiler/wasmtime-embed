{-# LANGUAGE CApiFFI #-}

module Wasmtime.Raw where

#include <wasmtime.h>

import Data.Int (Int32)
import Data.Word (Word8, Word64)
import Foreign.C.Types (CBool (..), CChar, CSize (..))
import Foreign.Ptr (FunPtr, Ptr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)

data WasmEngine
data WasmFuncType
data WasmTrap
data WasmtimeCaller
data WasmtimeContext
data WasmtimeError
data WasmtimeModule
data WasmtimeStore
data WasmtimeFunc
data WasmtimeInstance
data WasmtimeExtern
data WasmtimeMemory
data WasmtimeVal

data WasmByteVec
data WasmMemoryType

byteVecSize :: Ptr WasmByteVec -> IO CSize
byteVecSize pointer = peekByteOff pointer #{offset wasm_byte_vec_t, size}

byteVecData :: Ptr WasmByteVec -> IO (Ptr CChar)
byteVecData pointer = peekByteOff pointer #{offset wasm_byte_vec_t, data}

byteVecBytes :: Int
byteVecBytes = #{size wasm_byte_vec_t}

byteVecAlignment :: Int
byteVecAlignment = #{alignment wasm_byte_vec_t}

funcBytes :: Int
funcBytes = #{size wasmtime_func_t}

funcAlignment :: Int
funcAlignment = #{alignment wasmtime_func_t}

instanceBytes :: Int
instanceBytes = #{size wasmtime_instance_t}

instanceAlignment :: Int
instanceAlignment = #{alignment wasmtime_instance_t}

externBytes :: Int
externBytes = #{size wasmtime_extern_t}

externAlignment :: Int
externAlignment = #{alignment wasmtime_extern_t}

externKind :: Ptr WasmtimeExtern -> IO Word8
externKind pointer = peekByteOff pointer #{offset wasmtime_extern_t, kind}

setExternKindFunc :: Ptr WasmtimeExtern -> IO ()
setExternKindFunc pointer =
  pokeByteOff pointer #{offset wasmtime_extern_t, kind} (#{const WASMTIME_EXTERN_FUNC} :: Word8)

externFunc :: Ptr WasmtimeExtern -> Ptr WasmtimeFunc
externFunc pointer = pointer `plusPtr` #{offset wasmtime_extern_t, of.func}

externMemory :: Ptr WasmtimeExtern -> Ptr WasmtimeMemory
externMemory pointer = pointer `plusPtr` #{offset wasmtime_extern_t, of.memory}

externKindFunc :: Word8
externKindFunc = #{const WASMTIME_EXTERN_FUNC}

externKindMemory :: Word8
externKindMemory = #{const WASMTIME_EXTERN_MEMORY}

memoryBytes :: Int
memoryBytes = #{size wasmtime_memory_t}

memoryAlignment :: Int
memoryAlignment = #{alignment wasmtime_memory_t}

valBytes :: Int
valBytes = #{size wasmtime_val_t}

valAlignment :: Int
valAlignment = #{alignment wasmtime_val_t}

valKind :: Ptr WasmtimeVal -> IO Word8
valKind pointer = peekByteOff pointer #{offset wasmtime_val_t, kind}

setValI32 :: Ptr WasmtimeVal -> Int32 -> IO ()
setValI32 pointer value = do
  pokeByteOff pointer #{offset wasmtime_val_t, kind} (#{const WASMTIME_I32} :: Word8)
  pokeByteOff pointer #{offset wasmtime_val_t, of.i32} value

valI32 :: Ptr WasmtimeVal -> IO Int32
valI32 pointer = peekByteOff pointer #{offset wasmtime_val_t, of.i32}

valKindI32 :: Word8
valKindI32 = #{const WASMTIME_I32}

type Finalizer = Ptr () -> IO ()

type FuncCallback =
  Ptr () ->
  Ptr WasmtimeCaller ->
  Ptr WasmtimeVal ->
  CSize ->
  Ptr WasmtimeVal ->
  CSize ->
  IO (Ptr WasmTrap)

foreign import ccall "wrapper"
  wrapFuncCallback :: FuncCallback -> IO (FunPtr FuncCallback)

foreign import ccall "wrapper"
  wrapFinalizer :: Finalizer -> IO (FunPtr Finalizer)

foreign import ccall unsafe "wasm_engine_new"
  wasmEngineNew :: IO (Ptr WasmEngine)

foreign import ccall unsafe "&wasm_engine_delete"
  wasmEngineDeleteFinalizer :: FunPtr (Ptr WasmEngine -> IO ())

foreign import ccall unsafe "wasmtime_store_new"
  wasmtimeStoreNew :: Ptr WasmEngine -> Ptr () -> FunPtr Finalizer -> IO (Ptr WasmtimeStore)

foreign import ccall unsafe "wasmtime_store_delete"
  wasmtimeStoreDelete :: Ptr WasmtimeStore -> IO ()

foreign import ccall unsafe "wasmtime_store_context"
  wasmtimeStoreContext :: Ptr WasmtimeStore -> IO (Ptr WasmtimeContext)

foreign import ccall unsafe "wasm_byte_vec_delete"
  wasmByteVecDelete :: Ptr WasmByteVec -> IO ()

foreign import ccall safe "wasmtime_module_deserialize"
  wasmtimeModuleDeserialize ::
    Ptr WasmEngine -> Ptr Word8 -> CSize -> Ptr (Ptr WasmtimeModule) -> IO (Ptr WasmtimeError)

foreign import ccall safe "wasmtime_module_serialize"
  wasmtimeModuleSerialize ::
    Ptr WasmtimeModule -> Ptr WasmByteVec -> IO (Ptr WasmtimeError)

foreign import ccall safe "wasmtime_module_new"
  wasmtimeModuleNew ::
    Ptr WasmEngine -> Ptr Word8 -> CSize -> Ptr (Ptr WasmtimeModule) -> IO (Ptr WasmtimeError)

foreign import ccall safe "wasmtime_wat2wasm"
  wasmtimeWat2Wasm :: Ptr CChar -> CSize -> Ptr WasmByteVec -> IO (Ptr WasmtimeError)

foreign import ccall unsafe "&wasmtime_module_delete"
  wasmtimeModuleDeleteFinalizer :: FunPtr (Ptr WasmtimeModule -> IO ())

foreign import capi unsafe "wasmtime.h wasm_functype_new_0_0"
  wasmFuncTypeNew0_0 :: IO (Ptr WasmFuncType)

foreign import ccall unsafe "wasm_functype_delete"
  wasmFuncTypeDelete :: Ptr WasmFuncType -> IO ()

foreign import ccall safe "wasmtime_func_new"
  wasmtimeFuncNew ::
    Ptr WasmtimeContext ->
    Ptr WasmFuncType ->
    FunPtr FuncCallback ->
    Ptr () ->
    FunPtr Finalizer ->
    Ptr WasmtimeFunc ->
    IO ()

foreign import ccall safe "wasmtime_instance_new"
  wasmtimeInstanceNew ::
    Ptr WasmtimeContext ->
    Ptr WasmtimeModule ->
    Ptr WasmtimeExtern ->
    CSize ->
    Ptr WasmtimeInstance ->
    Ptr (Ptr WasmTrap) ->
    IO (Ptr WasmtimeError)

foreign import ccall safe "wasmtime_instance_export_get"
  wasmtimeInstanceExportGet ::
    Ptr WasmtimeContext ->
    Ptr WasmtimeInstance ->
    Ptr CChar ->
    CSize ->
    Ptr WasmtimeExtern ->
    IO CBool

foreign import ccall safe "wasmtime_func_call"
  wasmtimeFuncCall ::
    Ptr WasmtimeContext ->
    Ptr WasmtimeFunc ->
    Ptr WasmtimeVal ->
    CSize ->
    Ptr WasmtimeVal ->
    CSize ->
    Ptr (Ptr WasmTrap) ->
    IO (Ptr WasmtimeError)

foreign import ccall safe "wasmtime_memorytype_new"
  wasmtimeMemoryTypeNew ::
    Word64 -> CBool -> Word64 -> CBool -> CBool -> Word8 -> Ptr (Ptr WasmMemoryType) -> IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasm_memorytype_delete"
  wasmMemoryTypeDelete :: Ptr WasmMemoryType -> IO ()

foreign import ccall safe "wasmtime_memory_new"
  wasmtimeMemoryNew ::
    Ptr WasmtimeContext -> Ptr WasmMemoryType -> Ptr WasmtimeMemory -> IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_memory_data"
  wasmtimeMemoryData :: Ptr WasmtimeContext -> Ptr WasmtimeMemory -> IO (Ptr Word8)

foreign import ccall unsafe "wasmtime_memory_data_size"
  wasmtimeMemoryDataSize :: Ptr WasmtimeContext -> Ptr WasmtimeMemory -> IO CSize

foreign import ccall unsafe "wasmtime_memory_size"
  wasmtimeMemorySize :: Ptr WasmtimeContext -> Ptr WasmtimeMemory -> IO Word64

foreign import ccall safe "wasmtime_memory_grow"
  wasmtimeMemoryGrow ::
    Ptr WasmtimeContext -> Ptr WasmtimeMemory -> Word64 -> Ptr Word64 -> IO (Ptr WasmtimeError)

foreign import ccall unsafe "wasmtime_error_message"
  wasmtimeErrorMessage :: Ptr WasmtimeError -> Ptr WasmByteVec -> IO ()

foreign import ccall unsafe "wasmtime_error_delete"
  wasmtimeErrorDelete :: Ptr WasmtimeError -> IO ()

foreign import ccall unsafe "wasm_trap_message"
  wasmTrapMessage :: Ptr WasmTrap -> Ptr WasmByteVec -> IO ()

foreign import ccall unsafe "wasm_trap_delete"
  wasmTrapDelete :: Ptr WasmTrap -> IO ()

foreign import ccall unsafe "wasmtime_trap_new"
  wasmtimeTrapNew :: Ptr CChar -> CSize -> IO (Ptr WasmTrap)
