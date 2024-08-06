module Main where

import State (runTwoRandoms)
import Regex (compile, match)

main :: IO ()
main = do
  regex <- compile (pack "quick brown") []
  matches <- match regex (pack "quick brown box") []
  putStrLn (show matches)
