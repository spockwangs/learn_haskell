module LogLevel (LogLevel(..)) where

import Data.Maybe (fromMaybe)

data LogLevel = LogDebug
              | LogInfo
              | LogNotice
              | LogWarn
              | LogError
              | LogCrit
              | LogAlert
              | LogEmerg 
                deriving (Eq,Ord,Enum,Bounded)

logLevelNames :: [(LogLevel,String)]
logLevelNames = [
                 (LogDebug, "debug"),
                 (LogInfo,  "info"),
                 (LogNotice,"notice"),
                 (LogWarn,  "warn"),
                 (LogError, "error"),
                 (LogCrit,  "crit"),
                 (LogAlert, "alert"),
                 (LogEmerg, "emerg")
                ]

instance Show LogLevel where
    show l = fromMaybe (error $ "logLevelNames is incomplete") $ 
             lookup l logLevelNames

instance Read LogLevel where
    readsPrec _ s = [ (l,"") | (l,n) <- logLevelNames, n == s ]
