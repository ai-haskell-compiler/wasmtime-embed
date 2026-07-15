{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Paths_wasmtime_embed_examples (getDataFileName)
import Wasmtime

-- | Source corresponding to the bundled, ahead-of-time compiled module.
gcdWat :: ByteString
gcdWat =
  """
  (module
    (func $gcd (param i32 i32) (result i32)
      (local i32)
      block
        block
          local.get 0
          br_if 0
          local.get 1
          local.set 2
          br 1
        end
        loop
          local.get 1
          local.get 0
          local.tee 2
          i32.rem_u
          local.set 0
          local.get 2
          local.set 1
          local.get 0
          br_if 0
        end
      end
      local.get 2
    )
    (export "gcd" (func $gcd))
  )
  """

main :: IO ()
main = do
  engine <- newEngine
  store <- newStore engine
  modulePath <- getDataFileName "gcd/gcd.cwasm"
  serialized <- ByteString.readFile modulePath
  wasmModule <- deserializeModule engine serialized
  wasmInstance <- instantiate store wasmModule []
  gcdFunction <- getFunc store wasmInstance "gcd"
  result <- call2I32 store gcdFunction 6 27
  putStrLn ("gcd(6, 27) = " ++ show result)
