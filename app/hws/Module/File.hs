-- Copyright 2002, Simon Marlow.
-- Copyright 2006, Bjorn Bringert.
module Module.File (desc) where

import Config
import Headers
import Request
import Response
import ServerRequest
import ServerState
import Util

import Control.Monad
import System.Posix

desc :: ModuleDesc
desc = emptyModuleDesc {
                        moduleName = "file",
                        moduleLoad = return funs
                       }

funs :: Module
funs = emptyModule { 
                    moduleHandleRequest = fileHandler
                   }

fileHandler :: ServerState -> ServerRequest  -> IO (Maybe Response)
fileHandler st (ServerRequest { clientRequest = req, 
                                  serverFilename = filename }) = 
   do m_lstat <- statSymLink filename
      checkStat m_lstat
  where
    conf = serverConfig st
    checkStat (Just stat)
         | isRegularFile stat = 
             liftM Just $ case reqCmd req of
                            GetReq  -> serveFile st filename stat False
                            HeadReq -> serveFile st filename stat True
                            _       -> return (notImplementedResponse conf)
         | isSymbolicLink stat =
             if followSymLinks conf 
                then statFile filename >>= checkStat
                else do debug st $ "findProg: Not following symlink: " ++ show filename
                        return Nothing 
         | otherwise = do debug st $ "Strange file: " ++ show filename
                          return Nothing
    checkStat Nothing = do debug st $ "File not found: " ++ show filename
                           return Nothing

serveFile :: ServerState -> FilePath -> FileStatus -> Bool -> IO Response
serveFile st filename stat is_head =
   do -- check we can actually read this file
     access <- fileAccess filename True{-read-} False False
     case access of
       False -> return (notFoundResponse conf);
                   -- not "permission denied", we're being paranoid
                   -- about security.
       True -> 
         do let content_type = getMimeType st filename

            let last_modified = epochTimeToClockTime (modificationTime stat)

            let size = toInteger (fileSize stat)

            return (okResponse conf
                    (FileBody size filename)
                    (mkHeaders [contentTypeHeader content_type, 
                                lastModifiedHeader last_modified])
                    (not is_head) {- send body -})
  where conf = serverConfig st
