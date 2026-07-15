module Main where

import qualified Data.ByteString as ByteString
import Paths_wasmtime_embed_examples (getDataFileName)
import Wasmtime

main :: IO ()
main = do
  engine <- newEngine
  store <- newStore engine
  linking1 <- compileExample engine "linking/linking1.wat"
  linking2 <- compileExample engine "linking/linking2.wat"

  logMessage <- newHostFunc0 store (putStrLn "Hello, world!")
  linking2Instance <- instantiate store linking2 [logMessage]

  linker <- newLinker engine
  defineInstance linker store "linking2" linking2Instance
  linking1Instance <- instantiateWithLinker linker store linking1
  run <- getFunc store linking1Instance "run"
  call0 store run

compileExample :: Engine -> FilePath -> IO Module
compileExample engine relativePath = do
  path <- getDataFileName relativePath
  compileWatModule engine =<< ByteString.readFile path
