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

module Main (main) where

import AccessLogger
import ConfigParser
import Config
import ErrorLogger
import Headers
import MimeTypes
import Options
import Parse
import Request
import Response
import ServerRequest
import ServerState
import StaticModules
import Util

import Control.Concurrent
import Control.Exception as Exception
import Control.Monad
import Data.Maybe
import Network.BSD
import Network.Socket hiding (listen)
import qualified Network.Socket as Socket
import Network.URI
import System.Environment (getArgs)
import System.IO
import System.IO.Error
import System.Posix
import Text.ParserCombinators.Parsec
import Data.IP (IPv4, toHostAddress)
import Debug.Trace (trace, traceIO)

{- -----------------------------------------------------------------------------
ToDo:

- MAJOR:

- deal with http version numbers
- timeouts (partly done)
- languages
- per-directory permissions (ala apache)
- directory indexing
- error logging levels
- virtual hosts, per-directory config options.
- languages (content-language, accept-language)
- multipart/byteranges

- MINOR:

- access logging (various bits left)
- implement user & group setting
- log time to serve request
- terminate & restart signal (like Apache's SIGHUP)
- don't die if the new configuration file contains errors after a restart
- reading config file may block, unsafe if we receive another SIGHUP
- common up headers with same name (eg. accept).
- implement if-modified-since (need to parse time)

- when we get a request for http://foo.com/bar, where 'bar' is a
  directory and contains an index.html, we need to send back a
  redirect for http://foo.com/bar/ (i.e. add the final slash),
  otherwise relative links from index.html will be relative to
  http://foo.com/ instead of http://foo.com/bar/.  eg. look at
  http://www.haskell.org/happy/.

- MAYBE:

- throttling if too many open connections (config: MaxClients)

-}


-----------------------------------------------------------------------------
-- Top-level server

main :: IO ()
main =
    do args <- getArgs
       case parseOptions args of
         Left err   -> die err
         Right opts -> main2 opts

main2 :: Options -> IO ()
main2 opts =
    do main_thread <- myThreadId
       installHandler sigPIPE Ignore Nothing
       installHandler sigHUP (Catch (hupHandler main_thread)) Nothing
       mask $ \restore -> do
         readConfig restore opts

hupHandler :: ThreadId -> IO ()
hupHandler main_thread
  = throwTo main_thread (ErrorCall "**restart**")

sigsToBlock :: SignalSet
sigsToBlock = addSignal sigHUP emptySignalSet

-- Async exceptions should be blocked on entry to readConfig (so that
-- multiple SIGHUPs close together can't kill us).  Make sure that
-- there aren't any interruptible operations until we've blocked signals.
readConfig :: (IO () -> IO ()) -> Options -> IO ()
readConfig restore opts = do
    blockSignals sigsToBlock
    r <- parseConfig (configPath opts)
    case r of
      Left err -> die $ unlines ["Failed to parse configuration file",
                                 show err]
      Right b  -> do
        let conf = b defaultConfig
        st <- initServerState opts conf
        topServer restore st

rereadConfig :: (IO () -> IO ()) -> ServerState -> IO ()
rereadConfig restore st =
    do mapM_ stopAccessLogger (serverAccessLoggers st)
       stopErrorLogger (serverErrorLogger st)
       readConfig restore (serverOptions st)

initServerState :: Options -> Config -> IO ServerState
initServerState opts conf =
    do
       mimeTypes
           <- initMimeTypes (inServerRoot opts (typesConfig conf))
       errorLogger
           <- startErrorLogger (inServerRoot opts (errorLogFile conf)) (logLevel conf)
       accessLoggers
          <- sequence [startAccessLogger format (inServerRoot opts file)
                       | (file,format) <- customLogs conf]

       let st = ServerState {
             serverOptions = opts,
             serverConfig = conf,
             serverHostName = "localhost",
             serverPort = error "serverPort not set yet",
             serverMimeTypes = mimeTypes,
             serverErrorLogger = errorLogger,
             serverAccessLoggers = accessLoggers,
             serverModules = []
             }
       foldM loadModule st staticModules

loadModule :: ServerState -> ModuleDesc -> IO ServerState
loadModule st md =
    (do logInfo st $ "Loading module " ++ moduleName md ++ "..."
        m <- moduleLoad md
        moduleLoadConfig m st
        return $ st { serverModules = serverModules st ++ [m] })
    `Exception.catch`
    \e -> do logError st $ unlines ["Error loading module " ++ moduleName md,
                                    show (e :: SomeException)]
             return st

-- We catch exceptions from the main server thread, and restart the
-- server.  If we receive a restart signal (from a SIGHUP), then we
-- re-read the configuration file.
topServer :: (IO () -> IO ()) -> ServerState -> IO ()
topServer restore st
  = (do unblockSignals sigsToBlock
        restore startServers)
    `Exception.catch`
    (\ e -> do logError st ("server: " ++ show (e :: SomeException))
               topServer restore st)
  where startServers =
          do ts <- servers st
             (wait `Exception.catch`
               (\e -> case e of
                        ErrorCall "**restart**" ->
                          do mapM_ killThread ts
                             rereadConfig restore st
                        _ -> Exception.throw e))

servers :: ServerState -> IO [ThreadId]
servers st = do
  let addrs = map mkAddr (listen (serverConfig st))
  mapM (\ (st', addr) -> forkIO (server st' addr)) addrs
  where
    mkAddr (maddr, port) =
      let addr = case maddr of
                   Nothing -> 0
                   Just ip -> toHostAddress (read ip :: IPv4)
      in
        (st { serverPort = port }, SockAddrInet (fromIntegral port) addr)


-- open the server socket and start accepting connections
server :: ServerState -> SockAddr -> IO ()
server st addr = do
  logInfo st $ "Starting server thread on " ++ show addr
  proto <- getProtocolNumber "tcp"
  Exception.bracket
     (socket AF_INET Stream proto)
     (\sock -> close sock)
     (\sock -> do setSocketOption sock ReuseAddr 1
                  ok <- catchSomeIOErrors isAlreadyInUseError
                        (bind sock addr >> return True)
                        (\e -> do logError st ("server: " ++ show e)
                                  hPutStrLn stderr $ show e
                                  return False)
                  when ok $ do Socket.listen sock maxListenQueue
                               acceptConnections st sock
    )

-- accept connections, and fork off a new thread to handle each one
acceptConnections :: ServerState -> Socket -> IO ()
acceptConnections st sock = do
  debug st "Calling accept..."
  (h, SockAddrInet port haddr) <- Util.accept sock
  let ip = inet_ntoa haddr
  debug st $ "Got connection from " ++ ip ++ ":" ++ show port
  forkIO ( (talk st h haddr  `finally`  (hClose h))
            `Exception.catch`
          (\e -> debug st ("servlet died: "  ++ show (e :: SomeException)))
        )
  acceptConnections st sock

talk :: ServerState -> Handle -> HostAddress -> IO ()
talk st h haddr = do
  debug st "Started"
  hSetBuffering h LineBuffering
  run st True h haddr
  debug st "Done"

run :: ServerState -> Bool -> Handle -> HostAddress -> IO ()
run st first h haddr = do
    let conf = serverConfig st
    -- read a request up to the first empty line.  If we
    -- don't get a request within the alloted time, issue
    -- a "Request Time-out" response and close the connection.
    let time_allowed | first     = requestTimeout conf
                     | otherwise = keepAliveTimeout conf

    debug st "Waiting for request..."
    req <- catchIOError (
             do ok <- hWaitForInput h (time_allowed * 1000)
                if ok then liftM Just (getUntilEmptyLine h)
                  -- only send a "request timed out" response if this
                  -- was the first request on the socket.  Subsequent
                  -- requests time-out and close the socket silently.
                  -- ToDo: if we get a partial request, still emit the
                  -- the timeout response.
                      else do debug st $ "Request timeout (after " ++ show time_allowed ++ " s)"
                              when first (response st h (requestTimeOutResponse conf))
                              return Nothing
                              )
           (\e ->
                if isEOFError e
                     then debug st "EOF from client" >> return Nothing
                     else do logError st ("request: " ++ show e)
                             return Nothing )

    case req of { Nothing -> return ();  Just r -> do
    case parse pRequestHeaders "Request" r of

         -- close the connection after a badly formatted request
         Left err -> do
              debug st (show err)
              response st h (badRequestResponse conf)
              return ()

         Right req_no_body  -> do
              req <- getBody h req_no_body
              debug st $ show req
              resp <- request st req haddr
              response st h resp

              -- Persistent Connections
              --
              -- We close the connection if
              --   (a) client specified "connection: close"
              --   (b) client is pre-HTTP/1.1, and didn't
              --       specify "connection: keep-alive"

              let connection_headers = getConnection (reqHeaders req)
              if ConnectionClose `elem` connection_headers
                 || (reqHTTPVer req < http1_1
                     && ConnectionKeepAlive `notElem` connection_headers)
                   then return ()
                   else run st False h haddr
   }


getBody :: Handle -> Request -> IO Request
getBody h req = do b <- readBody
                   return $ req { reqBody = b}
  where
    -- FIXME: handled chunked input
    readBody = case getContentLength req of
                 Nothing  -> return ""
                 -- FIXME: what if input is huge?
                 Just len -> hGetChars h (fromIntegral len)

-----------------------------------------------------------------------------
-- Dealing with requests

request :: ServerState -> Request -> HostAddress -> IO Response
request st req haddr
  = do (sreq, merr) <- serverRequest st req haddr
       resp <- case merr of
                 Nothing  -> do sreq' <- tweakRequest st sreq
                                debug st $ "Handling request..."
                                handleRequest st sreq'
                 Just err -> return err
       debug st (showResponseLine resp)
       logAccess st sreq resp (error "noTimeDiff"){-FIXME-}
       return resp

serverRequest :: ServerState -> Request -> HostAddress -> IO (ServerRequest, Maybe Response)
serverRequest st req haddr
  = ( do remoteName <- maybeLookupHostname conf haddr
         let sreq1 = sreq { clientName = remoteName }
         e_host <- getServerHostName st req
         case e_host of
           Left resp -> return (sreq1, Just resp)
           Right host ->
               do let sreq2 = sreq1 { requestHostName = host }
                  e_path <- requestAbsPath st req
                  case e_path of
                    Left resp -> return (sreq2, Just resp)
                    Right path ->
                        do let sreq3 = sreq2 { serverURIPath = path }
                           e_file <- translatePath st path
                           case e_file of
                             Left resp -> return (sreq3, Just resp)
                             Right file ->
                                 do let sreq4 = sreq3 { serverFilename = file }
                                    return (sreq4, Nothing)
    )
      `Exception.catch`
    ( \e -> do
         logError st ("request: " ++ show (e :: SomeException))
         return (sreq, Just (internalServerErrorResponse conf))
    )
  where conf = serverConfig st
        sreq = ServerRequest {
                              clientRequest  = req,
                              clientAddress  = haddr,
                              clientName     = Nothing,
                              requestHostName = serverHostName st,
                              serverURIPath  = "-",
                              serverFilename = "-"
                             }



maybeLookupHostname :: Config -> HostAddress -> IO (Maybe HostEntry)
maybeLookupHostname conf haddr =
    if hostnameLookups conf
      then catchIOError (liftM Just (getHostByAddr AF_INET haddr))
                (\_ -> return Nothing)
      else return Nothing

-- make sure we've got a host field
-- if the request version is >= HTTP/1.1
getServerHostName :: ServerState -> Request -> IO (Either Response String)
getServerHostName st req
    = case getHost req of
        Nothing | reqHTTPVer req < http1_1
                    -> return $ Right (serverHostName st)
                | otherwise
                    -> return $ Left (badRequestResponse conf)
        Just (host,_)
            | isServerHost host
                -> return $ Right host
            | otherwise
                -> do logError st ("Unknown host: " ++ show host)
                      return $ Left $ notFoundResponse conf
  where conf = serverConfig st
        isServerHost host = host `elem` (serverName conf:serverAlias conf)


-- | Get the absolute path from the request.
--   TODO: do something about virtual hosts?
requestAbsPath :: ServerState -> Request -> IO (Either Response String)
requestAbsPath st req = return $ Right $ uriPath $ reqURI req


-- Path translation

translatePath :: ServerState -> String -> IO (Either Response FilePath)
translatePath st path =
  do m_file <- tryModules st (\m -> moduleTranslatePath m st path)
     case m_file of
       Just file -> return $ Right file
       Nothing   -> defaultTranslatePath st path

defaultTranslatePath :: ServerState -> String -> IO (Either Response FilePath)
defaultTranslatePath st path =
    case path of
      '/':_ -> return $ Right $ documentRoot conf ++ path
      _     -> return $ Left $ notFoundResponse conf
  where conf = serverConfig st

-- Request tweaking

tweakRequest :: ServerState -> ServerRequest -> IO ServerRequest
tweakRequest st = foldModules st (\m r -> moduleTweakRequest m st r)

-- Request handling

handleRequest :: ServerState -> ServerRequest -> IO Response
handleRequest st req =
    do m_resp <- tryModules st (\m -> moduleHandleRequest m st req)
       case m_resp of
         Just resp -> return resp
         Nothing   -> defaultHandleRequest st req

defaultHandleRequest :: ServerState -> ServerRequest -> IO Response
defaultHandleRequest st req = return (notFoundResponse (serverConfig st))

-- Sending response


response :: ServerState
         -> Handle
         -> Response
         -> IO ()

response _ h (Response { respCode = code,
                         respDesc = desc,
                         respHeaders = headers,
                         respCoding =  tes,
                         respBody =  body,
                         respSendBody = send_body }) =
  do hPutStrCrLf h (statusLine code desc)
     hPutHeader h serverHeader

     -- Date Header: required on all messages
     date <- dateHeader
     hPutHeader h date

     mapM_ (hPutHeader h) (listHeaders headers)

     -- Output a Content-Length when the message body isn't
     -- encoded.  If it *is* encoded, then the last transfer
     -- coding must be "chunked", according to RFC2616 sec 3.6.  This
     -- allows the client to determine the message-length.
     let content_length = responseBodyLength body

     when (hasBody body && null tes)
       (hPutHeader h (contentLengthHeader content_length))

     mapM_ (hPutHeader h . transferCodingHeader) tes

     hPutStrCrLf h ""
     -- ToDo: implement transfer codings

     if send_body
       then sendBody h body
       else return ()

hPutHeader :: Handle -> Header -> IO ()
hPutHeader h = hPutStrCrLf h . show
