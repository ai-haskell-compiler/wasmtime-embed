module Main where

import qualified Data.ByteString as ByteString
import Paths_wasmtime_embed_examples (getDataFileName)
import Wasmtime

main :: IO ()
main = do
  engine <- newEngine
  store <- newStore engine
  modulePath <- getDataFileName "gcd/gcd.wat"
  wat <- ByteString.readFile modulePath
  wasmModule <- compileWatModule engine wat
  wasmInstance <- instantiate store wasmModule []
  gcdFunction <- getFunc store wasmInstance "gcd"
  result <- call2I32 store gcdFunction 6 27
  putStrLn ("gcd(6, 27) = " ++ show result)
