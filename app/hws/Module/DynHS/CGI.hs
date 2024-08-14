module Module.DynHS.CGI where

import Module.CGI

import Response
import Request
import ServerRequest
import ServerState

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Map as Map
import Data.Maybe (isJust)

import Network.CGI.Monad
import Network.CGI.Protocol


hwsRunCGI :: ServerState -> ServerRequest -> CGI CGIResult -> IO Response
hwsRunCGI st sreq cgi =
  do let path_info = "" -- FIXME: do the path walk
     env <- mkCGIEnv st sreq path_info
     let input = BS.pack $ reqBody $ clientRequest sreq
     (hs,body) <- runCGI_ env input (runCGIT cgi)
     mkCGIResponse hs (BS.unpack body)

-- | Run a CGI action. This is what runCGIEnvFPS really should look like.
runCGI_ :: Monad m =>
           [(String,String)] -- ^ CGI environment variables.
        -> ByteString -- ^ Request body.
        -> (CGIRequest -> m (Headers, CGIResult)) -- ^ CGI action.
        -> m (Headers, ByteString) -- ^ Response (headers and content).
runCGI_ vars inp f
    = do (hs,outp) <- f $ CGIRequest {
                                      cgiVars = Map.fromList vars,
                                      cgiInputs = decodeInput vars inp,
                                      cgiRequestBody = inp
                                     }
         return $ case outp of
           CGIOutput c -> (hs',c)
               where hs' = if isJust (lookup ct hs)
                              then hs else hs ++ [(ct,defaultContentType)]
                     ct = HeaderName "Content-type"
           CGINothing -> (hs, BS.empty)

defaultContentType :: String
defaultContentType = "text/html; charset=ISO-8859-1"