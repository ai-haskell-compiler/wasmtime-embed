module Main (main) where

import Control.Exception (try)
import qualified Data.ByteString as ByteString
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Wasmtime

main :: IO ()
main = do
  engine <- newEngine
  store <- newStore engine
  serialized <- ByteString.readFile "test/fixtures/hello.cwasm"
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
