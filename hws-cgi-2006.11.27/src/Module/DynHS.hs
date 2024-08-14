-- Copyright 2006 Bjorn Bringert
module Module.DynHS where

import Module.DynHS.GHCUtil

import Headers
import Response
import ServerRequest
import ServerState

import Control.Exception as Exception
import Control.Monad
import Data.List
import System.IO


-- FIXME: keep this in config
packageDirectory :: FilePath
packageDirectory = "/usr/local/lib/ghc-6.6"


desc :: ModuleDesc
desc = emptyModuleDesc {
                        moduleName = "dynhs",
                        moduleLoad = loadDynHS
                       }

loadDynHS :: IO Module
loadDynHS =
    do s <- initGHC packageDirectory
       return $ emptyModule {
                             moduleLoadConfig = dynhsLoadConfig s,
                             moduleHandleRequest = dynhsHandleRequest s
                            }

dynhsLoadConfig :: Session -> ServerState -> IO ()
dynhsLoadConfig s st = setLogAction s (logError st)

dynhsHandleRequest :: Session -> ServerState -> ServerRequest -> IO (Maybe Response)
dynhsHandleRequest s st sreq = 
    do if ".hs" `isSuffixOf` serverFilename sreq
          then dynhsHandleRequest2 s st sreq
          else return Nothing

dynhsHandleRequest2 :: Session -> ServerState -> ServerRequest -> IO (Maybe Response)
dynhsHandleRequest2 s st sreq = withCleanUp s $
-- FIXME: lots of fake stuff here
    do debug st $ "DynHS: Loading " ++ show (serverFilename sreq)
       e_cgiMain <- logGHCErrors s st (getCgiMain s (serverFilename sreq))
       case e_cgiMain of
         Left resp -> return $ Just resp
         Right cgiMain -> liftM Just $ runCgiMain st sreq cgiMain

type CGIMain = [(String,String)] -> [(String,String)] -> IO ([(String,String)], String)

getCgiMain :: Session -> FilePath -> IO CGIMain
getCgiMain s file = getFileValue s file "cgiMain"

runCgiMain :: ServerState -> ServerRequest -> CGIMain -> IO Response
runCgiMain st sreq cgiMain =
    -- FIXME: lots of fake stuff here
    do let env = []
           inputs = []
       (hs,content) <- cgiMain env inputs
       let headers = mkHeaders [Header (mkHeaderName n) v | (n,v) <- hs]
       let code = 200
           desc = "OK"
       return $ Response code desc headers [] (HereItIs content) True

-- GHC utilities

logGHCErrors :: Session -> ServerState -> IO a -> IO (Either Response a)
logGHCErrors s st f 
  = liftM Right f 
    `Exception.catch` 
    (\e -> do logError st (show e)
              -- FIXME: include error message in response
              return $ Left $ internalServerErrorResponse (serverConfig st))

