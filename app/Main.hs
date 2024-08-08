module Main where

import State (runTwoRandoms)
import Regex (compile, match)
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)
import SafeDiv

main :: IO ()
main = do
  putStrLn (show (divBy 10 [1, 2, 3, 10]))
