{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (try)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Wasmtime

helloWat :: ByteString
helloWat =
  """
  (module
    (func $hello (import "" "hello"))
    (func (export "run") (call $hello))
  )
  """

linking1Wat :: ByteString
linking1Wat =
  """
  (module
    (import "linking2" "notify" (func $notify))
    (func (export "run") (call $notify))
  )
  """

linking2Wat :: ByteString
linking2Wat =
  """
  (module
    (import "" "notify" (func $notify))
    (export "notify" (func $notify))
  )
  """

multiWat :: ByteString
multiWat =
  """
  (module
    (func $f (import "" "f") (param i32 i64) (result i64 i32))
    (func (export "g") (param i32 i64) (result i64 i32)
      (call $f (local.get 0) (local.get 1)))
    (func (export "round_trip_many")
      (param i64 i64 i64 i64 i64 i64 i64 i64 i64 i64)
      (result i64 i64 i64 i64 i64 i64 i64 i64 i64 i64)
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      local.get 4
      local.get 5
      local.get 6
      local.get 7
      local.get 8
      local.get 9))
  """

main :: IO ()
main = do
  compilationEngine <- newEngine
  compiledModule <- compileWatModule compilationEngine helloWat
  serialized <- serializeModule compiledModule

  engine <- newEngine
  store <- newStore engine
  wasmModule <- deserializeModule engine serialized
  called <- newIORef False
  hello <- newHostFunc0 store (writeIORef called True)
  wasmInstance <- instantiate store wasmModule [hello]
  run <- getFunc store wasmInstance "run"
  call0 store run
  callbackRan <- readIORef called
  if callbackRan then pure () else fail "host callback did not run"

  failing <- newHostFunc0 store (fail "callback exploded")
  failingInstance <- instantiate store wasmModule [failing]
  failingRun <- getFunc store failingInstance "run"
  outcome <- try (call0 store failingRun) :: IO (Either WasmtimeException ())
  case outcome of
    Left (WasmtimeException message)
      | "callback exploded" `isInfixOf` message -> pure ()
    Left exception -> fail ("unexpected trap: " ++ show exception)
    Right () -> fail "callback exception did not become a trap"

  linkedCallRan <- newIORef False
  notify <- newHostFunc0 store (writeIORef linkedCallRan True)
  linking2Module <- compileWatModule engine linking2Wat
  linking2Instance <- instantiate store linking2Module [notify]
  linker <- newLinker engine
  defineInstance linker store "linking2" linking2Instance
  linking1Module <- compileWatModule engine linking1Wat
  linking1Instance <- instantiateWithLinker linker store linking1Module
  linkedRun <- getFunc store linking1Instance "run"
  call0 store linkedRun
  didRun <- readIORef linkedCallRan
  if didRun then pure () else fail "linked host callback did not run"

  multiModule <- compileWatModule engine multiWat
  callback <- newHostFunc store [I32Type, I64Type] [I64Type, I32Type] incrementAndFlip
  multiInstance <- instantiate store multiModule [callback]
  g <- getFunc store multiInstance "g"
  call store g [I32 1, I64 3] >>= assertEqual [I64 4, I32 2]
  roundTripMany <- getFunc store multiInstance "round_trip_many"
  let many = map I64 [0 .. 9]
  call store roundTripMany many >>= assertEqual many

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual expected actual
  | actual == expected = pure ()
  | otherwise = fail ("expected " ++ show expected ++ ", got " ++ show actual)

incrementAndFlip :: [Val] -> IO [Val]
incrementAndFlip [I32 a, I64 b] = pure [I64 (b + 1), I32 (a + 1)]
incrementAndFlip _ = fail "unexpected callback arguments"
