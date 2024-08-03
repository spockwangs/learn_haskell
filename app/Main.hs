module Main where

import State (runTwoRandoms)

main :: IO ()
main = do
  result <- runTwoRandoms
  putStrLn (show result)
