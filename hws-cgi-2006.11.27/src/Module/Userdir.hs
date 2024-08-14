-- Copyright 2002, Simon Marlow.
-- Copyright 2006, Bjorn Bringert.
module Module.Userdir (desc) where

import Config
import ServerState

import Control.Exception
import System.Posix
import System.IO.Error (tryIOError)

desc :: ModuleDesc
desc = emptyModuleDesc {
                        moduleName = "userdir",
                        moduleLoad = return funs
                       }

funs :: Module
funs = emptyModule { 
                    moduleTranslatePath = userdirTranslatePath
                   }

userdirTranslatePath :: ServerState -> String -> IO (Maybe FilePath)
userdirTranslatePath st ('/':'~':userpath) | not (null (userDir conf)) = 
  do let (user, path) = break (=='/') userpath
     debug st $ "looking for user: " ++ show user
     u_ent <- tryIOError (getUserEntryForName user)
     case u_ent of
       Left _    -> return Nothing
       Right ent -> 
           do let p = '/': homeDirectory ent ++ '/':userDir conf ++ path
              debug st $ "userdir path: " ++ p
              return $ Just p
  where conf = serverConfig st
userdirTranslatePath _ _ = return Nothing
