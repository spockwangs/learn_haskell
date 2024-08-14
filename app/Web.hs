module Web where

acceptConnections :: Config -> Socket -> IO ()
acceptConnections conf sock = do
  (handle, remote) <- accept sock
  forkIO (catch
          (talk conf handle remote ‘finally‘ hClose handle)
          (\e -> logError e))
  acceptConnections conf sock

talk :: Config -> Handle -> HostAddress -> IO ()
talk conf handle haddr = do
  strs <- getRequest handle
  case parseRequest strs of
    Left resp -> sendResponse conf handle resp
    Right req -> do
      resp <- genResponse conf req
      sendResponse conf handle resp
      logAccess req resp haddr
      if (isKeepAlive req)
        then talk conf handle haddr
        else return ()
