module Options (Options, 
                serverRoot, configPath, inServerRoot, 
                parseOptions) where

import Data.List (isPrefixOf)
import System.Console.GetOpt


type Options = [CmdLineOpt]

data CmdLineOpt
  = O_ConfigFile FilePath
  | O_ServerRoot FilePath


options :: [OptDescr CmdLineOpt]
options = [
  Option ['f'] ["config"] (ReqArg O_ConfigFile "filename") 
    ("default: " ++ show defaultConfigFile),
  Option ['d'] ["server-root"]  (ReqArg O_ServerRoot "directory")
    ("default: " ++ show defaultServerRoot)
  ]

usage :: String
usage = "usage: hws [option...]"

defaultConfigFile :: FilePath
defaultConfigFile = "conf/httpd.conf"

defaultServerRoot :: FilePath
defaultServerRoot = "."

serverRoot :: Options -> FilePath
serverRoot opts =
    case [ s | O_ServerRoot s <- opts] of
      [] -> defaultServerRoot
      s  -> last s

configPath :: Options -> FilePath
configPath args = inServerRoot args (configFile args)

configFile :: Options -> FilePath
configFile args = 
    case [ s | O_ConfigFile s <- args] of
      [] -> defaultConfigFile
      s  -> last s

inServerRoot :: Options -> FilePath -> FilePath
inServerRoot opts f | "/" `isPrefixOf` f = f
                    | otherwise = serverRoot opts ++ '/':f

-- returns error message or options
parseOptions :: [String] -> Either String Options
parseOptions args =
    case getOpt Permute options args of
      (flags, [], []) -> Right flags
      (_, _, errs) -> Left (concat errs ++ "\n" ++ usageInfo usage options)
