-- Copyright 2006, Bjorn Bringert.
module ServerRequest where

import Request

import Network.BSD (HostEntry)
import Network.Socket (HostAddress)

-- | All the server's information about a request
data ServerRequest = ServerRequest
 { 
   clientRequest :: Request,
   clientAddress :: HostAddress,
   clientName :: Maybe HostEntry,
   requestHostName :: String,
   serverURIPath :: String,
   serverFilename :: FilePath
 }
  deriving Show
