module Main where

import RunProcess

main :: IO ()
main = do
  runIO ("ls", ["."])
