module Module.DynHS.GHCUtil 
    (Session,
     initGHC,
     setLogAction,
     withCleanUp,
     getFileValue,
     -- * Error logging
     Severity(..), SrcSpan, PprStyle, Message,
     mkLocMessage
    ) where

-- GHC API stuff
import DynFlags
import ErrUtils
import GHC hiding (Module, Session, moduleName)
import qualified GHC (Module, Session)
import HscMain (newHscEnv)
import HscTypes (Session(..))
import Outputable (PprStyle)
import SrcLoc (SrcSpan)
import SysTools (initSysTools)

import Data.Dynamic
import Data.IORef
import System.IO

initGHC :: FilePath -> IO Session
initGHC pkgDir
  = do s <- newSession' Interactive (Just pkgDir)
       modifySessionDynFlags s (\flags -> flags{ hscTarget = HscInterpreted })
       return s



-- Like newSesion, but does not install signal handlers
newSession' :: GhcMode -> Maybe FilePath -> IO Session
newSession' mode mb_top_dir = do
  dflags0 <- initSysTools mb_top_dir defaultDynFlags
  dflags  <- initDynFlags dflags0
  env <- newHscEnv dflags{ ghcMode=mode }
  ref <- newIORef env
  return (Session ref)

setLogAction :: Session -> (String -> IO ()) -> IO ()
setLogAction s f = 
    modifySessionDynFlags s (\flags -> flags { log_action = mkLogAction f })

mkLogAction :: (String -> IO ()) 
            -> Severity -> SrcSpan -> PprStyle -> Message -> IO ()
mkLogAction f severity srcSpan style msg =
    case severity of
      SevInfo  -> f (show (msg style))
      SevFatal -> f (show (msg style))
      _        -> f (show ((mkLocMessage srcSpan msg) style))


modifySessionDynFlags :: Session -> (DynFlags -> DynFlags) -> IO ()
modifySessionDynFlags s f = 
    do flags <- getSessionDynFlags s
       setSessionDynFlags s (f flags)
       return ()

withCleanUp :: Session -> IO a -> IO a
withCleanUp s f = do flags <- getSessionDynFlags s
                     defaultCleanupHandler flags f

loadFile :: Session -> FilePath -> IO GHC.Module
loadFile s file = 
    do let t = Target (TargetFile file Nothing) Nothing
       setTargets s [t]
       succ <- load s LoadAllTargets
       case succ of
         Succeeded -> do m <- fileModule s file
                         setContext s [] [m]
                         return m
         Failed    -> fail $ "Failed to load " ++ show file

fileModule :: Session -> FilePath -> IO GHC.Module
fileModule s f = 
    do gr <- getModuleGraph s       
       case [ms_mod ms | ms <- gr, ml_hs_file (ms_location ms) == Just f]  of
         [m] -> return m
         _   -> fail $ "File " ++ f ++ " does not correspond to a module" 

getValue :: Typeable a => Session -> String -> IO a
getValue s x = 
    do mdyn <- dynCompileExpr s x
       case mdyn of
         Nothing -> fail $ "dynCompileExpr " ++ show x ++ " failed"
         Just dyn -> case fromDynamic dyn of
                       Nothing -> fail $ "Type error: " ++ x 
                                         ++ " is an " ++ show dyn
                       Just x  -> return x

getFileValue :: Typeable a => Session -> FilePath -> String -> IO a
getFileValue s file x = 
    do m <- loadFile s file
       getValue s x
