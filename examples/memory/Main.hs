module Main where

import Control.Exception (try)
import Control.Monad (unless, void)
import Data.ByteString qualified as ByteString
import Paths_wasmtime_embed_examples (getDataFileName)
import Wasmtime

main :: IO ()
main = do
  engine <- newEngine
  store <- newStore engine
  modulePath <- getDataFileName "memory/memory.wat"
  wat <- ByteString.readFile modulePath
  wasmModule <- compileWatModule engine wat
  wasmInstance <- instantiate store wasmModule []

  memory <- getMemory store wasmInstance "memory"
  size <- getFunc store wasmInstance "size"
  load <- getFunc store wasmInstance "load"
  storeByte <- getFunc store wasmInstance "store"

  putStrLn "Checking memory..."
  memorySize store memory >>= checkEqual 2
  memoryDataSize store memory >>= checkEqual 0x20000
  readMemoryByte store memory 0 >>= checkEqual 0
  readMemoryByte store memory 0x1000 >>= checkEqual 1
  readMemoryByte store memory 0x1003 >>= checkEqual 4
  call0I32 store size >>= checkEqual 2
  call1I32 store load 0 >>= checkEqual 0
  call1I32 store load 0x1000 >>= checkEqual 1
  call1I32 store load 0x1003 >>= checkEqual 4
  call1I32 store load 0x1ffff >>= checkEqual 0
  expectFailure (call1I32 store load 0x20000)

  putStrLn "Mutating memory..."
  writeMemoryByte store memory 0x1003 5
  call2I32Unit store storeByte 0x1002 6
  expectFailure (call2I32Unit store storeByte 0x20000 0)
  readMemoryByte store memory 0x1002 >>= checkEqual 6
  readMemoryByte store memory 0x1003 >>= checkEqual 5
  call1I32 store load 0x1002 >>= checkEqual 6
  call1I32 store load 0x1003 >>= checkEqual 5

  putStrLn "Growing memory..."
  growMemory store memory 1 >>= checkEqual 2
  memorySize store memory >>= checkEqual 3
  memoryDataSize store memory >>= checkEqual 0x30000
  call1I32 store load 0x20000 >>= checkEqual 0
  call2I32Unit store storeByte 0x20000 0
  expectFailure (call1I32 store load 0x30000)
  expectFailure (call2I32Unit store storeByte 0x30000 0)
  expectFailure (growMemory store memory 1)
  growMemory store memory 0 >>= checkEqual 3

  putStrLn "Creating stand-alone memory..."
  secondMemory <- newMemory store 5 (Just 5)
  memorySize store secondMemory >>= checkEqual 5
  expectFailure (growMemory store secondMemory 1)
  growMemory store secondMemory 0 >>= checkEqual 5

checkEqual :: (Eq a, Show a) => a -> a -> IO ()
checkEqual expected actual =
  unless (actual == expected) $
    fail ("expected " ++ show expected ++ ", got " ++ show actual)

expectFailure :: IO a -> IO ()
expectFailure action = do
  outcome <- try (void action)
  case outcome of
    Left (WasmtimeException _) -> pure ()
    Right () -> fail "expected WasmtimeException"
