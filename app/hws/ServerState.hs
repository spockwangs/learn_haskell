-- Copyright 2006, Bjorn Bringert.
module ServerState where

import Config
import AccessLogger
import ErrorLogger
import LogLevel
import MimeTypes
import Options
import Response
import ServerRequest
import Util

import Control.Concurrent (myThreadId)
import Control.Monad
import Network.BSD (HostEntry)
import System.Time (TimeDiff)

--
-- * Module API
--

data ModuleDesc = ModuleDesc
 {
  moduleName :: String,
  moduleLoad :: IO Module
 }

emptyModuleDesc :: ModuleDesc
emptyModuleDesc = ModuleDesc
 {
  moduleName = "<unnamed module>",
  moduleLoad = return emptyModule
 }

data Module = Module 
 {
  moduleLoadConfig    :: ServerState -> IO (),
  moduleTranslatePath :: ServerState -> String -> IO (Maybe FilePath),
  moduleTweakRequest  :: ServerState -> ServerRequest -> IO ServerRequest,
  moduleHandleRequest :: ServerState -> ServerRequest -> IO (Maybe Response)
 }

emptyModule :: Module
emptyModule = Module {
                      moduleLoadConfig    = \_   -> return (),
                      moduleTranslatePath = \_ _ -> return Nothing,
                      moduleTweakRequest  =  \_ r -> return r,
                      moduleHandleRequest = \_ _ -> return Nothing
                     }

tweakFilename :: (ServerState -> FilePath -> IO FilePath) 
              -> ServerState -> ServerRequest -> IO ServerRequest
tweakFilename f conf req = 
    do filename' <- f conf (serverFilename req)
       return $ req { serverFilename = filename' }


--
-- * ServerState 
--

data ServerState = ServerState
    {
     serverOptions :: Options,
     serverConfig :: Config,
     serverHostName :: HostEntry,
     serverPort :: Int,
     serverMimeTypes :: MimeTypes,
     serverErrorLogger :: ErrorLoggerHandle,
     serverAccessLoggers :: [AccessLoggerHandle],
     serverModules :: [Module]
    }

-- * MIME types

getMimeType :: ServerState -> FilePath -> String
getMimeType st filename = 
    maybe def show (mimeTypeOf (serverMimeTypes st) filename)
  where def = defaultType (serverConfig st)

-- ** Logging

debug :: ServerState -> String -> IO ()
debug st s = do t <- myThreadId
                logDebug st $ show t ++ ": " ++ s

logError :: ServerState -> String -> IO ()
logError st = logErrorMessage (serverErrorLogger st) LogError

logInfo :: ServerState -> String -> IO ()
logInfo st = logErrorMessage (serverErrorLogger st) LogInfo

logDebug :: ServerState -> String -> IO ()
logDebug st = logErrorMessage (serverErrorLogger st) LogDebug

logAccess :: ServerState -> ServerRequest -> Response -> TimeDiff -> IO ()
logAccess st req resp delay = 
    do msg <- mkAccessLogRequest req resp (serverHostName st) delay
       mapM_ (\l -> logAccessLogRequest l msg) (serverAccessLoggers st)

-- ** Modules

mapModules_ :: ServerState -> (Module -> IO ()) -> IO ()
mapModules_ st f = mapM_ f (serverModules st)

foldModules :: ServerState -> (Module -> a -> IO a) -> a -> IO a
foldModules st f x = foldM (flip f) x (serverModules st)

tryModules :: ServerState -> (Module -> IO (Maybe a)) -> IO (Maybe a)
tryModules st f = firstJustM f (serverModules st)
