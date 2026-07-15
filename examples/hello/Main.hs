module Main where

import qualified Data.ByteString as ByteString
import Paths_wasmtime_embed_examples (getDataFileName)
import Wasmtime

main :: IO ()
main = do
  putStrLn "Initializing..."
  engine <- newEngine
  store <- newStore engine
  putStrLn "Compiling module..."
  modulePath <- getDataFileName "hello/hello.wat"
  wat <- ByteString.readFile modulePath
  wasmModule <- compileWatModule engine wat
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
