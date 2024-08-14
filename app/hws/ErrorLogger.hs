-- -----------------------------------------------------------------------------
-- Copyright 2002, Simon Marlow.
-- Copyright 2006, Bjorn Bringert.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
-- 
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
-- 
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
--  * Neither the name of the copyright holder(s) nor the names of
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- -----------------------------------------------------------------------------

module ErrorLogger (
                    ErrorLoggerHandle,
                    startErrorLogger, 
                    stopErrorLogger, 
                    logErrorMessage
                   ) where

import Logger
import LogLevel
import Util

import System.IO
import System.Time

data ErrorLoggerHandle = ErrorLoggerHandle 
    { 
     errorLogger ::LoggerHandle ErrorLoggerMessage,
     errorMinLevel :: LogLevel
    }

data ErrorLoggerMessage = ErrorLoggerMessage
    {
     errorTime   :: ClockTime,
     errorString :: String
    }


startErrorLogger :: FilePath -> LogLevel -> IO ErrorLoggerHandle
startErrorLogger file level = 
    do l <- startLogger format file
       let h = ErrorLoggerHandle {
                                  errorLogger = l,
                                  errorMinLevel = level
                                 }
       logErrorMessage h LogWarn $ "Starting error logger with log level " 
                                    ++ show level ++ "..."
       return h
  where format m = formatTimeSensibly (toUTCTime (errorTime m))
                   ++ "  " ++ errorString m

stopErrorLogger :: ErrorLoggerHandle -> IO ()
stopErrorLogger l = do logErrorMessage l LogWarn "Stopping error logger..."
                       stopLogger (errorLogger l)

logErrorMessage :: ErrorLoggerHandle -> LogLevel -> String -> IO ()
logErrorMessage l level s 
    | level < errorMinLevel l = return ()
    | otherwise = do time <- getClockTime
                     logMessage (errorLogger l) (ErrorLoggerMessage time s)
