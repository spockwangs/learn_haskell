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

module AccessLogger (
                     AccessLoggerHandle,
                     AccessLogRequest(..),
                     startAccessLogger,
                     stopAccessLogger,
                     mkAccessLogRequest,
                     logAccessLogRequest
                    ) where

import Logger
import Headers
import Response
import ServerRequest
import Util

import Network.BSD (HostEntry, hostName)
import System.Time
import System.IO.Unsafe (unsafePerformIO)
import Network.Socket (HostAddress)

type AccessLoggerHandle = LoggerHandle AccessLogRequest

data AccessLogRequest = AccessLogRequest 
    { 
     log_request        :: ServerRequest,
     log_response       :: Response,
     log_server_host    :: String,
     log_time           :: ClockTime,
     log_delay          :: TimeDiff
    }


startAccessLogger :: String -> FilePath -> IO AccessLoggerHandle
startAccessLogger format file = startLogger f file
  where f = mkLogLine format

mkLogLine :: String -> AccessLogRequest -> String
mkLogLine "" _ = ""
mkLogLine ('%':'{':rest) r = 
    case span (/= '}') rest of
      (str, '}':c:rest1) -> expand (Just str) c r ++ mkLogLine rest1 r
      _                  -> '%':'{':mkLogLine rest r
mkLogLine ('%':c:rest) r = expand Nothing c r ++ mkLogLine rest r
mkLogLine (c:rest) r = c : mkLogLine rest r

expand :: Maybe String -> Char -> AccessLogRequest -> String
expand arg c info = 
          case c of
            'b' -> let len = responseBodyLength (respBody resp)
                   in if len == 0 then "-" else show len
            'f' -> serverFilename sreq

            -- %h is the hostname if hostnameLookups is on, otherwise the 
            -- IP address.
            'h' -> maybe addr hostName (clientName sreq)
            'a' -> addr
            'l' -> "-" -- FIXME: does anyone use identd these days?
            'r' -> show req
            -- ToDo: 'p' -> canonical port number of server
            's' -> show (respCode resp)
            't' -> formatTimeSensibly (toUTCTime (log_time info))
            'T' -> timeDiffToString (log_delay info)
            'v' -> log_server_host info
            'u' -> "-" -- FIXME: implement HTTP auth

            'i' -> header req arg
            'o' -> header resp arg

            -- ToDo: other stuff
            _ -> ['%',c]
  where
   resp = log_response info
   sreq = log_request info
   req  = clientRequest sreq
   host = clientName (log_request info)
   header _ Nothing  = ""
   header x (Just n) = unwords (lookupHeaders (mkHeaderName n) x)
   addr = inet_ntoa (clientAddress sreq)

stopAccessLogger :: AccessLoggerHandle -> IO ()
stopAccessLogger l = stopLogger l

mkAccessLogRequest :: ServerRequest -> Response -> String -> TimeDiff -> IO AccessLogRequest
mkAccessLogRequest req resp host delay =
    do time <- getClockTime
       return $ AccessLogRequest 
                  { 
                   log_request     = req,
                   log_response    = resp,
                   log_server_host = host,
                   log_time        = time,
                   log_delay       = delay
                  }

logAccessLogRequest :: AccessLoggerHandle -> AccessLogRequest -> IO ()
logAccessLogRequest l r = logMessage l r

