-- Copyright 2006, Bjorn Bringert.
module Module.CGI (desc,
                   mkCGIEnv, mkCGIResponse
                  ) where

import Config
import Headers
import Parse
import Request
import Response
import ServerRequest
import ServerState
import Util

import Control.Concurrent
import Control.Exception as Exception
import Control.Monad
import Data.Char (toUpper)
import Data.List
import Network.BSD (hostName)
import Network.URI
import System.IO
import System.IO.Error
import System.Posix
import System.Process (runInteractiveProcess, waitForProcess)
import Text.ParserCombinators.Parsec


desc :: ModuleDesc
desc = emptyModuleDesc {
                        moduleName = "cgi",
                        moduleLoad = return funs
                       }

funs :: Module
funs = emptyModule {
                    moduleHandleRequest = cgiHandleRequest
                   }

cgiHandleRequest :: ServerState -> ServerRequest -> IO (Maybe Response)
cgiHandleRequest st sreq =
    do m_prog <- findProg st (serverFilename sreq)
       case m_prog of
         Nothing -> do debug st $ "CGI: not handling " ++ serverFilename sreq
                       return Nothing
         Just (path_prog, path_info) ->
             do if ".cgi" `isSuffixOf` path_prog -- FIXME: add AddHandler mechanism
                 then do let handle = cgiHandleRequest2 st sreq path_prog path_info 
                         liftM Just $ case reqCmd (clientRequest sreq) of
                                        GetReq  -> handle False
                                        PostReq -> handle True
                                        _       -> return $ notImplementedResponse conf
                 else do debug st $ "CGI: not handling " ++ serverFilename sreq ++ ", wrong suffix"
                         return Nothing
  where conf = serverConfig st

cgiHandleRequest2 :: ServerState -> ServerRequest -> FilePath -> String -> Bool -> IO Response
cgiHandleRequest2 st sreq path_prog path_info useReqBody =
    do let conf = serverConfig st
       let req = clientRequest sreq

       env <- mkCGIEnv st sreq path_info
       let wdir = dirname path_prog

       debug st $ "Running CGI program: " ++ path_prog

       (inp,out,err,pid) 
           <- runInteractiveProcess path_prog [] (Just wdir) (Just env)
              
              
       if useReqBody then forkIO (writeBody inp req) >> return ()
                     else hClose inp

       -- log process stderr to the error log
       forkIO (logErrorsFromHandle st err)

       -- FIXME: exception handling
       -- FIXME: close handle?
       output <- hGetContents out

       -- wait in a separate thread, so that this thread can continue.
       -- this is needed since output is lazy.
       forkIO (waitForProcess pid >> return ())

       case parseCGIOutput output of
         Left err -> do logError st err
                        return $ internalServerErrorResponse conf
         Right (outputHeaders, content) -> mkCGIResponse outputHeaders content

mkCGIResponse :: Headers -> String -> IO Response
mkCGIResponse outputHeaders content = 
    do let stat = lookupHeader (HdrCustom "Status") outputHeaders
           loc  = lookupHeader HdrLocation outputHeaders
       (code,desc) <- case stat of
                        Nothing -> let c = maybe 200 (\_ -> 302) loc
                                    in return (c, responseDescription c)
                        Just s  -> case reads s of
                                     [(c,r)] -> return (c,trimLWS r)
                                     _       -> fail "Bad Status line"
       -- FIXME: don't use response constructor directly
       return (Response code desc outputHeaders [] (HereItIs content) True)

-- Split the requested file system path into the path to an
-- existing file, and some extra path info
findProg :: ServerState -> FilePath -> IO (Maybe (FilePath,String))
findProg st filename = case splitPath filename of
                           []    -> return Nothing -- this should never happen
                           [""]  -> return Nothing -- we got an empty path
                           "":p  -> firstFile st "/" p -- absolute path
                           p:r   -> firstFile st p r -- relative path

firstFile :: ServerState -> FilePath -> [String] -> IO (Maybe (FilePath,String))
firstFile st p pi = 
    do m_lstat <- statSymLink p
       checkStat m_lstat
  where conf = serverConfig st

        mkPath x y | "/" `isSuffixOf` x = x ++ y
                   | otherwise          = x ++ "/" ++ y

        mkPathInfo [] = ""
        mkPathInfo p  = "/" ++ glue "/" p

        checkStat Nothing = do debug st $ "findProg: Not found: " ++ show p
                               return Nothing
        checkStat (Just stat) 
            | isDirectory stat = 
                case pi of
                  []    -> do debug st $ "findProg: " ++ show p ++ " is a directory"
                              return Nothing
                  f:pi' -> firstFile st (mkPath p f) pi'
            | isRegularFile stat  = return $ Just (p,mkPathInfo pi)
            | isSymbolicLink stat = if followSymLinks conf 
                                      then statFile p >>= checkStat
                                      else do debug st $ "findProg: Not following symlink: " ++ show p
                                              return Nothing 
            | otherwise         = do debug st $ "Strange file: " ++ show p
                                     return Nothing

mkCGIEnv :: ServerState -> ServerRequest -> String -> IO [(String,String)]
mkCGIEnv st sreq path_info
    = do let req = clientRequest sreq
             remoteAddr = inet_ntoa (clientAddress sreq)
         let scriptName = serverURIPath sreq `dropSuffix` path_info
             -- FIXME: use canonical name if there is no ServerName
             serverEnv = 
                 [
                  ("SERVER_SOFTWARE",   serverSoftware 
                                        ++ "/" ++ serverVersion),
                  ("SERVER_NAME",       hostName (requestHostName sreq)),
                  ("GATEWAY_INTERFACE", "CGI/1.1")
                 ]
             requestEnv = 
                 [
                  ("SERVER_PROTOCOL",   show (reqHTTPVer req)),
                  ("SERVER_PORT",       show (serverPort st)),
                  ("REQUEST_METHOD",    show (reqCmd req)),             
                  ("PATH_TRANSLATED",   serverFilename sreq),
                  ("SCRIPT_NAME",       scriptName),
                  ("QUERY_STRING",      uriQuery (reqURI req) `dropPrefix` "?"),
                  ("REMOTE_ADDR",       remoteAddr),
                  ("PATH_INFO",         path_info)
                 ]
               ++ maybeHeader "AUTH_TYPE"      Nothing -- FIXME
               ++ maybeHeader "REMOTE_USER"    Nothing -- FIXME
               ++ maybeHeader "REMOTE_IDENT"   Nothing -- FIXME
               ++ maybeHeader "REMOTE_HOST"    (fmap hostName (clientName sreq))
               ++ maybeHeader "CONTENT_TYPE"   (getContentType req)
               ++ maybeHeader "CONTENT_LENGTH" (fmap show $ getContentLength req)
             hs = [] -- FIXME: convert headers to (name,value) pairs
             headerEnv = [("HTTP_"++ map toUpper n, v) | (n,v) <- hs]

         return $ serverEnv ++ requestEnv ++ headerEnv

-- Writes the body of a request to a handle.
writeBody :: Handle -> Request -> IO ()
writeBody h req = do hPutStr h (reqBody req)
                     hClose h

-- | Reads lines form the given 'Handle' and log them with 'logError'.
logErrorsFromHandle :: ServerState -> Handle -> IO ()
logErrorsFromHandle st h = 
    (loop `Exception.catch` \e -> 
        case e of
          e | isEOFError (e :: IOException) -> return ()
          _ -> logError st $ "CGI:" ++ show e)
      `Exception.finally` hClose h
  where loop = do l <- hGetLine h
                  logError st l
                  loop

maybeHeader :: String -> Maybe String -> [(String,String)]
maybeHeader n = maybe [] ((:[]) . (,) n)

parseCGIOutput :: String -> Either String (Headers,String)
parseCGIOutput s = case parse pCGIOutput "CGI output" s of
                    Left err -> Left (show err)
                    Right x  -> Right x

pCGIOutput :: Parser  (Headers,String)
pCGIOutput =
    do headers <- pHeaders
       pCRLF
       body <- getInput
       return (headers, body)
