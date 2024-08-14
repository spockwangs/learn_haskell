-- Copyright 2002, Simon Marlow.
-- Copyright 2006, Bjorn Bringert.
module Module.Index (desc) where

import Config
import ServerRequest
import ServerState
import Util

import System.Posix



desc :: ModuleDesc
desc = emptyModuleDesc {
                        moduleName = "index",
                        moduleLoad = return funs
                       }

funs :: Module
funs = emptyModule { 
                    moduleTweakRequest = indexTweakRequest
                   }

indexTweakRequest :: ServerState -> ServerRequest -> IO ServerRequest
indexTweakRequest = tweakFilename indexFixPath

indexFixPath :: ServerState -> FilePath -> IO FilePath
indexFixPath st filename = 
  do stat <- statFile filename
     case stat of
       Just stat | isDirectory stat -> do
             let index_filename = filename ++ '/': directoryIndex conf
             debug st $ "index_filename = " ++ show index_filename
             stat <- statFile index_filename
             case stat of
                 Nothing -> return filename
                 Just stat -> return index_filename
       _ -> return filename
  where conf = serverConfig st
