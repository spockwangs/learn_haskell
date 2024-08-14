-- -----------------------------------------------------------------------------
-- Copyright 2002, Simon Marlow.
-- Copyright 2006, Bjorn Bringert.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
-- 
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
-- 
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
--  * Neither the name of the copyright holder(s) nor the names of
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- -----------------------------------------------------------------------------

module Request (
                HTTPVersion(..),
                http1_1, http1_0,
                Request(..),
                RequestCmd(..),
                RequestBody,
                Connection(..),
                Expect(..),
                pRequestHeaders,
                getHost,
                getConnection
               ) where

import Text.ParserCombinators.Parsec

import Headers
import Parse
import Util

import Data.Char
import Data.Maybe
import Network.URI


-----------------------------------------------------------------------------
-- Requests

-- Request-Line   = Method SP Request-URI SP HTTP-Version CRLF

data RequestCmd
  = OptionsReq
  | GetReq
  | HeadReq
  | PostReq
  | PutReq
  | DeleteReq
  | TraceReq
  | ConnectReq
  | ExtensionReq String

instance Show RequestCmd where
  show c = case c of
                  OptionsReq     -> "OPTIONS"
                  GetReq         -> "GET"
                  HeadReq        -> "HEAD"
                  PostReq        -> "POST"
                  PutReq         -> "PUT"
                  DeleteReq      -> "DELETE"
                  TraceReq       -> "TRACE"
                  ConnectReq     -> "CONNECT"
                  ExtensionReq s -> s

data Request = Request {
     reqCmd     :: RequestCmd,
     reqURI     :: URI,
     reqHTTPVer :: HTTPVersion,
     reqHeaders :: Headers,
     reqBody    :: RequestBody
  }

instance Show Request where
  showsPrec _ Request{reqCmd = cmd, reqURI = uri, reqHTTPVer = ver}
      = shows cmd . (' ':) . shows uri . (' ':) . shows ver

instance HasHeaders Request where
    getHeaders = reqHeaders
    setHeaders req hs = req { reqHeaders = hs} 


data HTTPVersion = HTTPVersion Int Int
  deriving (Eq,Ord)

instance Show HTTPVersion where
    showsPrec _ (HTTPVersion maj min) = 
        showString "HTTP/" . shows maj . showString "." . shows min

http1_1, http1_0 :: HTTPVersion
http1_1 = HTTPVersion 1 1
http1_0 = HTTPVersion 1 0


-- FIXME: use something more efficient
type RequestBody = String


-- Request parsing

-- Parse the request line and the headers, but not the body.
pRequestHeaders :: Parser Request
pRequestHeaders = 
    do (cmd,uri,ver) <- pRequestLine
       headers <- pHeaders
       pCRLF
       return $ Request cmd uri ver headers ""

pRequestLine :: Parser (RequestCmd, URI, HTTPVersion)
pRequestLine = do cmd <- pReqCmd 
                  many1 pSP
                  uri <- pReqURI
                  many1 pSP
                  ver <- pReqHTTPVer
                  pCRLF
                  return (cmd,uri,ver)

pReqCmd :: Parser RequestCmd
pReqCmd = choice [
                  c "OPTIONS" OptionsReq,
                  c "GET"     GetReq,
                  c "HEAD"    HeadReq,
                  c "POST"    PostReq,
                  c "PUT"     PutReq,
                  c "DELETE"  DeleteReq,
                  c "TRACE"   TraceReq,
                  c "CONNECT" ConnectReq,
                  pToken >>= return . ExtensionReq
                 ]
  where c x y = try (string x >> return y)

pReqURI :: Parser URI
pReqURI = 
    do u <- many (noneOf [' '])
       -- FIXME: this does not handle authority Request-URIs
       maybe (fail "Bad Request-URI") return $ parseURIReference u

pReqHTTPVer :: Parser HTTPVersion
pReqHTTPVer = do string "HTTP/"; 
                 major <- int; 
                 char '.'; 
                 minor <- int;
                 return $ HTTPVersion major minor

int :: Parser Int
int = many1 digit >>= readM

-----------------------------------------------------------------------------
-- Getting specific request headers


data Connection 
  = ConnectionClose
  | ConnectionKeepAlive -- non-std?  Netscape generates it.
  | ConnectionOther String
  deriving (Eq, Show)

parseConnection :: String -> [Connection]
parseConnection = map (fn . map toLower) . parseList
     where fn "close"      = ConnectionClose
           fn "keep-alive" = ConnectionKeepAlive
           fn other        = ConnectionOther other

getConnection :: HasHeaders a => a -> [Connection]
getConnection = concatMap parseConnection . lookupHeaders HdrConnection

data Expect 
  = ExpectContinue
  deriving Show

parseExpect :: String -> Maybe Expect
parseExpect s =
  case parseList s of
     ["100-continue"] -> Just ExpectContinue
     _                -> Nothing


getHost :: HasHeaders a => a -> Maybe (String, Maybe Int)
getHost x = lookupHeader HdrHost x >>= parseHost

parseHost :: String -> Maybe (String, Maybe Int)
parseHost s = 
  case port of 
     ""       -> Just (host, Nothing)
     ':':port -> readM port >>= \p -> Just (host, Just p)
     _        -> Nothing
  where (host,port) = break (==':') s

