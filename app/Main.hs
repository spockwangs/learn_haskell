module Main where

import Regex (compile, match)
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString (ByteString)

main :: IO ()
main = do
  let regexPattern = (pack "quick brown[")
      str = (pack "quick brow box")
      matches = matchStr regexPattern str
  case matches of
    Left err -> putStrLn ("error: " ++ err)
    Right result -> putStrLn (show result)
  where
    matchStr :: ByteString -> ByteString -> Either String [ByteString]
    matchStr pat s = do
      regex <- compile pat []
      match regex s [] `orDie` ("`" ++ (unpack pat) ++ "` does not match `" ++ (unpack s) ++ "`")
        where
          orDie :: Maybe a -> String -> Either String a
          Nothing `orDie` err = Left err
          (Just x) `orDie` _ = Right x
