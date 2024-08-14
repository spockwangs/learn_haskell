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

module Logger (
               LoggerHandle,                    
               startLogger,
               stopLogger,
               logMessage
              ) where

import Util

import Control.Exception as Exception
import Control.Concurrent
import System.Directory
import System.IO


data LoggerHandle a = LoggerHandle 
    {
     loggerHandleChan     :: Chan (LoggerCommand a),
     loggerHandleThreadId :: ThreadId
    }

data Logger a = Logger
    {
     loggerChan     :: Chan (LoggerCommand a),
     loggerFormat   :: (a -> String),
     loggerFile     :: FilePath
    }

data LoggerCommand a = StopLogger
                     | LogMessage a

startLogger :: (a -> String) -- ^ Message formatting function
            -> FilePath      -- ^ log file path
            -> IO (LoggerHandle a)
startLogger format file = 
    do chan <- newChan
       createDirectoryIfMissing True (dirname file)
       let l = Logger {
                       loggerChan = chan,
                       loggerFormat = format,
                       loggerFile = file
                      }
       t <- forkIO (runLogger l
                    `Exception.catch` 
                    (\e -> hPutStrLn stderr 
                           ("Error starting logger: " ++ show (e :: SomeException))))
       return $ LoggerHandle { 
                              loggerHandleChan = chan,
                              loggerHandleThreadId = t 
                             }

stopLogger :: LoggerHandle a -> IO ()
stopLogger l = writeChan (loggerHandleChan l) StopLogger

logMessage :: LoggerHandle a -> a -> IO ()
logMessage l x = writeChan (loggerHandleChan l) (LogMessage x)

-- Internals

runLogger :: Logger a -> IO ()
runLogger l = runLogger1 l
                `Exception.catch` 
              (\e -> do hPutStrLn stderr ("Logger died: " ++ show (e :: SomeException))
                        runLogger l)

runLogger1 :: Logger a -> IO ()
runLogger1 l = 
    Exception.bracket 
      (openLogFile (loggerFile l))
      (\hdl -> hClose hdl)
      (\hdl -> handleLogCommands l hdl)
  where
    openLogFile :: FilePath -> IO Handle
    openLogFile f = 
        openFile f AppendMode 
            `Exception.catch` 
        (\e -> do hPutStrLn stderr ("Failed to open log file: " ++ show (e :: SomeException))
                  Exception.throw e)

handleLogCommands :: Logger a -> Handle -> IO ()
handleLogCommands l hdl =
    do comm <- readChan (loggerChan l)
       case comm of
         StopLogger ->    do return ()
         LogMessage x  -> do let str = (loggerFormat l) x
                             writeLogLine hdl str
                             handleLogCommands l hdl
  where
    writeLogLine :: Handle -> String -> IO ()
    writeLogLine hdl str = do hPutStrLn hdl str
                              hFlush hdl
