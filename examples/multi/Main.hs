module Main where

import Data.ByteString qualified as ByteString
import Paths_wasmtime_embed_examples (getDataFileName)
import Wasmtime

main :: IO ()
main = do
  putStrLn "Initializing..."
  engine <- newEngine
  store <- newStore engine

  putStrLn "Compiling module..."
  modulePath <- getDataFileName "multi/multi.wat"
  wat <- ByteString.readFile modulePath
  wasmModule <- compileWatModule engine wat

  putStrLn "Creating callback..."
  callback <- newHostFunc store [I32Type, I64Type] [I64Type, I32Type] incrementAndFlip

  putStrLn "Instantiating module..."
  wasmInstance <- instantiate store wasmModule [callback]

  putStrLn "Extracting export..."
  g <- getFunc store wasmInstance "g"

  putStrLn "Calling export \"g\"..."
  result <- call store g [I32 1, I64 3]
  putStrLn "Printing result..."
  putStrLn ("> " ++ unwords (map showVal result))
  checkEqual [I64 4, I32 2] result

  putStrLn "Calling export \"round_trip_many\"..."
  roundTripMany <- getFunc store wasmInstance "round_trip_many"
  let many = map I64 [0 .. 9]
  manyResult <- call store roundTripMany many
  putStrLn "Printing result..."
  print manyResult
  checkEqual many manyResult

showVal :: Val -> String
showVal (I32 value) = show value
showVal (I64 value) = show value

incrementAndFlip :: [Val] -> IO [Val]
incrementAndFlip [I32 a, I64 b] = pure [I64 (b + 1), I32 (a + 1)]
incrementAndFlip _ = fail "unexpected callback arguments"

checkEqual :: (Eq a, Show a) => a -> a -> IO ()
checkEqual expected actual
  | actual == expected = pure ()
  | otherwise = fail ("expected " ++ show expected ++ ", got " ++ show actual)
