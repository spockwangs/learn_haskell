module Main where

import State (runTwoRandoms)
import Regex (compile, match)
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)

main :: IO ()
main = do
  let regexPattern = (pack "quick brown")
      str = (pack "quick brown box")
      matches = matchStr regexPattern str
  case matches of
    Left error -> putStrLn (show error)
    Right result -> putStrLn (show result)
  where
    matchStr :: ByteString -> ByteString -> Either String [ByteString]
    matchStr pat s = do
      regex <- compile pat []
      morphEither $ match regex s []
        where
          morphEither :: Maybe a -> Either String a
          morphEither Nothing = Left "Nothing"
          morphEither (Just x) = Right x
