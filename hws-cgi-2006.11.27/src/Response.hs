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

module Response where

import Config
import Headers
import Parse
import Util

import Control.Monad
import Control.Exception as Exception
import Data.List (genericLength)
import System.IO
import System.Time
import Text.Html


-----------------------------------------------------------------------------
-- Responses

data ResponseBody
  = NoBody
  | FileBody Integer{-size-} FilePath
  | HereItIs String

data Response
  = Response {
      respCode     :: Int,
      respDesc     :: String,
      respHeaders  :: Headers,
      respCoding   :: [TransferCoding], -- either empty or terminated with 
                                        -- ChunkedTransferEncoding 
                                        -- (RFC2616, sec 3.6)
      respBody     :: ResponseBody,     -- filename of body
      respSendBody :: Bool              -- actually send the body?
                                        --  (False for HEAD requests)
   }


instance Show Response where
   showsPrec _ r 
         = showString (showResponseLine r) . showString crLf
           . shows (respHeaders r)

instance HasHeaders Response where
    getHeaders = respHeaders
    setHeaders resp hs = resp { respHeaders = hs} 

showResponseLine :: Response -> String
showResponseLine (Response s desc hs _ _ _) = show s ++ " " ++ desc 


responseBodyLength :: ResponseBody -> Integer
responseBodyLength body = 
    case body of
      NoBody          -> 0
      HereItIs stuff  -> genericLength stuff
      FileBody size _ -> size

hasBody :: ResponseBody -> Bool
hasBody NoBody = False
hasBody _ = True

getFileName :: ResponseBody -> String
getFileName (NoBody) = "<no file>"
getFileName (FileBody _size filename) = filename
getFileName (HereItIs _) = "<generated content>"

sendBody :: Handle -> ResponseBody -> IO ()
sendBody _ NoBody = return ()
sendBody h (HereItIs stuff) = do hPutStr h stuff
                                 hFlush h
sendBody h (FileBody _size filename)
  = Exception.bracket 
        (openFile filename ReadMode)
        (\handle -> hClose handle)
        (\handle -> squirt handle h >> hFlush h)


statusLine :: Int -> String -> String
statusLine code desc = httpVersion ++ ' ': show code ++ ' ': desc

httpVersion = "HTTP/1.1"


-----------------------------------------------------------------------------
-- Response Headers

dateHeader :: IO Header
dateHeader = do
   -- Dates in HTTP/1.1 have to be GMT, which is equivalent to UTC
  clock_time <- getClockTime
  let utc = toUTCTime clock_time
  let time_str = formatTimeSensibly utc
  return $ Header HdrDate time_str

serverHeader :: Header
serverHeader = Header HdrServer (serverSoftware ++ '/':serverVersion)


-----------------------------------------------------------------------------
-- Response codes

contResponse                         = error_resp 100
switchingProtocolsResponse           = error_resp 101
okResponse                           = body_resp 200
createdResponse                      = error_resp 201
acceptedResponse                     = error_resp 202
nonAuthoritiveInformationResponse    = error_resp 203
noContentResponse                    = error_resp 204
resetContentResponse                 = error_resp 205
partialContentResponse               = error_resp 206
multipleChoicesResponse              = error_resp 300
movedPermanentlyResponse             = error_resp 301
foundResponse                        = error_resp 302
seeOtherResponse                     = error_resp 303
notModifiedResponse                  = error_resp 304
useProxyResponse                     = error_resp 305
temporaryRedirectResponse            = error_resp 307
badRequestResponse                   = error_resp 400
unauthorizedResponse                 = error_resp 401
paymentRequiredResponse              = error_resp 402
forbiddenResponse                    = error_resp 403
notFoundResponse                     = error_resp 404
methodNotAllowedResponse             = error_resp 405
notAcceptableResponse                = error_resp 406
proxyAuthenticationRequiredResponse  = error_resp 407
requestTimeOutResponse               = error_resp 408
conflictResponse                     = error_resp 409
goneResponse                         = error_resp 410
lengthRequiredResponse               = error_resp 411
preconditionFailedResponse           = error_resp 412
requestEntityTooLargeResponse        = error_resp 413
requestURITooLargeResponse           = error_resp 414
unsupportedMediaTypeResponse         = error_resp 415
requestedRangeNotSatisfiableResponse = error_resp 416
expectationFailedResponse            = error_resp 417
internalServerErrorResponse          = error_resp 500
notImplementedResponse               = error_resp 501
badGatewayResponse                   = error_resp 502
serviceUnavailableResponse           = error_resp 503
gatewayTimeOutResponse               = error_resp 504
versionNotSupportedResponse          = error_resp 505

responseDescription :: Int -> String
responseDescription 100 = "Continue"                         
responseDescription 101 = "Switching Protocols"              

responseDescription 200 = "OK"                          
responseDescription 201 = "Created"                          
responseDescription 202 = "Accepted"                         
responseDescription 203 = "Non-Authoritative Information"    
responseDescription 204 = "No Content"                       
responseDescription 205 = "Reset Content"                    
responseDescription 206 = "Partial Content"                  

responseDescription 300 = "Multiple Choices"                 
responseDescription 301 = "Moved Permanently"                
responseDescription 302 = "Found"                            
responseDescription 303 = "See Other"                        
responseDescription 304 = "Not Modified"                     
responseDescription 305 = "Use Proxy"                        
responseDescription 307 = "Temporary Redirect"               

responseDescription 400 = "Bad Request"                      
responseDescription 401 = "Unauthorized"                     
responseDescription 402 = "Payment Required"                 
responseDescription 403 = "Forbidden"                        
responseDescription 404 = "Not Found"                        
responseDescription 405 = "Method Not Allowed"               
responseDescription 406 = "Not Acceptable"                   
responseDescription 407 = "Proxy Authentication Required"    
responseDescription 408 = "Request Time-out"                 
responseDescription 409 = "Conflict"                         
responseDescription 410 = "Gone"                             
responseDescription 411 = "Length Required"                  
responseDescription 412 = "Precondition Failed"              
responseDescription 413 = "Request Entity Too Large"         
responseDescription 414 = "Request-URI Too Large"            
responseDescription 415 = "Unsupported Media Type"           
responseDescription 416 = "Requested range not satisfiable"  
responseDescription 417 = "Expectation Failed"               

responseDescription 500 = "Internal Server Error"            
responseDescription 501 = "Not Implemented"                  
responseDescription 502 = "Bad Gateway"                      
responseDescription 503 = "Service Unavailable"              
responseDescription 504 = "Gateway Time-out"                 
responseDescription 505 = "HTTP Version not supported"       
responseDescription _   = "Unknown response"

error_resp code conf
  = Response code (responseDescription code) hs [] 
        (generateErrorPage code conf) True
    where hs = mkHeaders [contentTypeHeader "text/html"]

body_resp code _conf body headers sendBody = 
    Response code (responseDescription code) headers [] body sendBody

-----------------------------------------------------------------------------
-- Error pages

-- We generate some html for the client to display on an error.

generateErrorPage :: Int -> Config -> ResponseBody
generateErrorPage code conf
  = HereItIs (renderHtml (genErrorHtml code conf))

genErrorHtml :: Int -> Config -> Html
genErrorHtml code conf
  = header << thetitle << response
    +++ body <<
         (h1 << response
          +++ hr
          +++ serverSoftware +++ '/' +++ serverVersion
          -- ToDo: use real hostname if we don't have a serverName
          +++ case serverName conf of
                "" -> noHtml
                me -> " on " +++ me +++ br
          +++ case serverAdmin conf of
                "" -> noHtml
                her -> "Server Admin: " +++ 
                       hotlink ("mailto:"++her) [toHtml her]
         )
  where
    descr = responseDescription code
    response = show code +++ ' ' +++ descr
