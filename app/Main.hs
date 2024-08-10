module Main where

import RunProcess

main :: IO ()
main = do
  runIO $ ("ls", ["."]) -|- ("grep", ["^[a-z]\\+[$"]) -|- ("tr", ["a-z", "A-Z"])
