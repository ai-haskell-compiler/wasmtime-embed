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
