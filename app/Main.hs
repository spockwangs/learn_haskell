module Main where

import State (runTwoRandoms)
import Regex (compile, match)
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)

main :: IO ()
main = do
  let regexPattern = (pack "quick brown")
      str = (pack "quick brow box")
      matches = matchStr regexPattern str
  case matches of
    Left err -> putStrLn (show err)
    Right result -> putStrLn (show result)
  where
    matchStr :: ByteString -> ByteString -> Either String [ByteString]
    matchStr pat s = do
      regex <- compile pat []
      match regex s [] `orDie` "not matched"
        where
          orDie :: Maybe a -> String -> Either String a
          Nothing `orDie` err = Left err
          (Just x) `orDie` _ = Right x
