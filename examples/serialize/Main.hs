module Main where

import Data.ByteString qualified as ByteString
import Paths_wasmtime_embed_examples (getDataFileName)
import Wasmtime

serialize :: IO ByteString.ByteString
serialize = do
  putStrLn "Initializing..."
  engine <- newEngine
  putStrLn "Compiling module..."
  modulePath <- getDataFileName "hello/hello.wat"
  wat <- ByteString.readFile modulePath
  wasmModule <- compileWatModule engine wat
  serialized <- serializeModule wasmModule
  putStrLn "Serialized."
  pure serialized

deserialize :: ByteString.ByteString -> IO ()
deserialize serialized = do
  putStrLn "Initializing..."
  engine <- newEngine
  store <- newStore engine
  putStrLn "Deserializing module..."
  wasmModule <- deserializeModule engine serialized
  putStrLn "Creating callback..."
  hello <- newHostFunc0 store $ do
    putStrLn "Calling back..."
    putStrLn "> Hello World!"
  putStrLn "Instantiating module..."
  wasmInstance <- instantiate store wasmModule [hello]
  putStrLn "Extracting export..."
  run <- getFunc store wasmInstance "run"
  putStrLn "Calling export..."
  call0 store run
  putStrLn "All finished!"

main :: IO ()
main = serialize >>= deserialize
