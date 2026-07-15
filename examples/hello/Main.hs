{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Paths_wasmtime_embed_examples (getDataFileName)
import Wasmtime

-- | Source corresponding to the bundled, ahead-of-time compiled module.
helloWat :: ByteString
helloWat =
  """
  (module
    (func $hello (import "" "hello"))
    (func (export "run") (call $hello))
  )
  """

main :: IO ()
main = do
  putStrLn "Initializing..."
  engine <- newEngine
  store <- newStore engine
  putStrLn "Loading precompiled module..."
  modulePath <- getDataFileName "hello/hello.cwasm"
  serialized <- ByteString.readFile modulePath
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
