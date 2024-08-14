-- Copyright 2002 Warrick Gray
-- Copyright 2001,2002 Peter Thiemann
-- Copyright 2003-2006 Bjorn Bringert
module Headers (Headers(..),
                mkHeaders,
                Header(..),
                HeaderName(..),
                HasHeaders(..),
                -- * Header parsing
                pHeaders, mkHeaderName,
                -- * Header manipulation
                insertHeader,
                insertHeaderIfMissing,
                replaceHeader, insertHeaders,
                lookupHeaders, lookupHeader,
                -- * Constructing headers
                contentLengthHeader,
                contentTypeHeader,
                lastModifiedHeader,
                TransferCoding,
                transferCodingHeader,
                -- * Getting values of specific headers
                getContentType, getContentLength
               ) where

import Parse
import Util

import Control.Monad (liftM)
import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)
import Data.Maybe (listToMaybe)
import System.Time (ClockTime, toUTCTime)
import Text.ParserCombinators.Parsec

newtype Headers = Headers { unHeaders :: [Header] }

mkHeaders :: [Header] -> Headers
mkHeaders = Headers

instance Show Headers where
    showsPrec _
        = foldr (.) id . map (\x -> shows x . showString crLf) . unHeaders

instance HasHeaders Headers where
    getHeaders = id
    setHeaders _ = id


-- | This class allows us to write generic header manipulation functions
-- for both 'Request' and 'Response' data types.
class HasHeaders x where
    getHeaders :: x -> Headers
    setHeaders :: x -> Headers -> x
    listHeaders :: x -> [Header]
    listHeaders = unHeaders . getHeaders
    modifyHeaders :: ([Header] -> [Header]) -> x -> x
    modifyHeaders f x = setHeaders x $ mkHeaders $ f $ listHeaders x

-- | The Header data type pairs header names & values.
data Header = Header HeaderName String

instance Show Header where
    showsPrec _ (Header key value)
        = shows key . showString ": " . showString value


-- | HTTP Header Name type:
--  Why include this at all?  I have some reasons
--   1) prevent spelling errors of header names,
--   2) remind everyone of what headers are available,
--   3) might speed up searches for specific headers.
--
--  Arguments against:
--   1) makes customising header names laborious
--   2) increases code volume.
--
data HeaderName =
                 -- Generic Headers --
                  HdrCacheControl
                | HdrConnection
                | HdrDate
                | HdrPragma
                | HdrTrailer
                | HdrTransferEncoding
                | HdrUpgrade
                | HdrVia

                -- Request Headers --
                | HdrAccept
                | HdrAcceptCharset
                | HdrAcceptEncoding
                | HdrAcceptLanguage
                | HdrAuthorization
                | HdrCookie
                | HdrExpect
                | HdrFrom
                | HdrHost
                | HdrIfModifiedSince
                | HdrIfMatch
                | HdrIfNoneMatch
                | HdrIfRange
                | HdrIfUnmodifiedSince
                | HdrMaxForwards
                | HdrProxyAuthorization
                | HdrRange
                | HdrReferer
                | HdrTE
                | HdrUserAgent

                -- Response Headers
                | HdrAge
                | HdrLocation
                | HdrProxyAuthenticate
                | HdrPublic
                | HdrRetryAfter
                | HdrServer
                | HdrSetCookie
                | HdrVary
                | HdrWarning
                | HdrWWWAuthenticate

                -- Entity Headers
                | HdrAllow
                | HdrContentBase
                | HdrContentEncoding
                | HdrContentLanguage
                | HdrContentLength
                | HdrContentLocation
                | HdrContentMD5
                | HdrContentRange
                | HdrContentType
                | HdrETag
                | HdrExpires
                | HdrLastModified

                -- Mime entity headers (for sub-parts)
                | HdrContentTransferEncoding

                -- | Allows for unrecognised or experimental headers.
                | HdrCustom String -- not in header map below.
    deriving (Eq,Ord)


-- Translation between header names and values,
headerNames :: [ (String,HeaderName) ]
headerNames
     = [  ("Cache-Control"        ,HdrCacheControl      )
        , ("Connection"           ,HdrConnection        )
        , ("Date"                 ,HdrDate              )
        , ("Pragma"               ,HdrPragma            )
        , ("Trailer"              ,HdrTrailer           )

        , ("Transfer-Encoding"    ,HdrTransferEncoding  )
        , ("Upgrade"              ,HdrUpgrade           )
        , ("Via"                  ,HdrVia               )
        , ("Accept"               ,HdrAccept            )
        , ("Accept-Charset"       ,HdrAcceptCharset     )
        , ("Accept-Encoding"      ,HdrAcceptEncoding    )
        , ("Accept-Language"      ,HdrAcceptLanguage    )
        , ("Authorization"        ,HdrAuthorization     )
        , ("From"                 ,HdrFrom              )
        , ("Host"                 ,HdrHost              )
        , ("If-Modified-Since"    ,HdrIfModifiedSince   )
        , ("If-Match"             ,HdrIfMatch           )
        , ("If-None-Match"        ,HdrIfNoneMatch       )
        , ("If-Range"             ,HdrIfRange           )
        , ("If-Unmodified-Since"  ,HdrIfUnmodifiedSince )
        , ("Max-Forwards"         ,HdrMaxForwards       )
        , ("Proxy-Authorization"  ,HdrProxyAuthorization)
        , ("Range"                ,HdrRange             )
        , ("Referer"              ,HdrReferer           )
        , ("TE"                   ,HdrTE                )
        , ("User-Agent"           ,HdrUserAgent         )
        , ("Age"                  ,HdrAge               )
        , ("Location"             ,HdrLocation          )
        , ("Proxy-Authenticate"   ,HdrProxyAuthenticate )
        , ("Public"               ,HdrPublic            )
        , ("Retry-After"          ,HdrRetryAfter        )
        , ("Server"               ,HdrServer            )
        , ("Vary"                 ,HdrVary              )
        , ("Warning"              ,HdrWarning           )
        , ("WWW-Authenticate"     ,HdrWWWAuthenticate   )
        , ("Allow"                ,HdrAllow             )
        , ("Content-Base"         ,HdrContentBase       )
        , ("Content-Encoding"     ,HdrContentEncoding   )
        , ("Content-Language"     ,HdrContentLanguage   )
        , ("Content-Length"       ,HdrContentLength     )
        , ("Content-Location"     ,HdrContentLocation   )
        , ("Content-MD5"          ,HdrContentMD5        )
        , ("Content-Range"        ,HdrContentRange      )
        , ("Content-Type"         ,HdrContentType       )
        , ("ETag"                 ,HdrETag              )
        , ("Expires"              ,HdrExpires           )
        , ("Last-Modified"        ,HdrLastModified      )
        , ("Set-Cookie"           ,HdrSetCookie         )
        , ("Cookie"               ,HdrCookie            )
        , ("Expect"               ,HdrExpect            ) ]

toHeaderNameMap :: Map String HeaderName
toHeaderNameMap = Map.fromList [(map toLower x, y) | (x,y) <- headerNames]

fromHeaderNameMap :: Map HeaderName String
fromHeaderNameMap = Map.fromList [(y,x) | (x,y) <- headerNames]

instance Show HeaderName where
    show (HdrCustom s) = s
    show x = case Map.lookup x fromHeaderNameMap of
               Nothing -> error "headerNames incomplete"
               Just h  -> h

mkHeaderName :: String -> HeaderName
mkHeaderName s =
    case Map.lookup (map toLower s) toHeaderNameMap of
      Just n  -> n
      Nothing -> HdrCustom s


-- * Header manipulation functions

-- | Inserts a header with the given name and value.
-- Allows duplicate header names.
insertHeader :: HasHeaders a => HeaderName -> String -> a -> a
insertHeader name value = modifyHeaders (Header name value:)

-- | Adds the new header only if no previous header shares
-- the same name.
insertHeaderIfMissing :: HasHeaders a => HeaderName -> String -> a -> a
insertHeaderIfMissing name value x = setHeaders x $ mkHeaders hs'
  where hs' = case lookupHeader name x of
                Nothing -> Header name value : hs
                Just _  -> hs
        hs = listHeaders (getHeaders x)

-- | Removes old headers with the same name.
replaceHeader :: HasHeaders a => HeaderName -> String -> a -> a
replaceHeader name value = modifyHeaders f
    where f hs = Header name value : [ x | x@(Header n v) <- hs, name /= n ]

-- | Inserts multiple headers.
insertHeaders :: HasHeaders a => [Header] -> a -> a
insertHeaders hdrs = modifyHeaders (hdrs++)

lookupHeaders :: HasHeaders a => HeaderName -> a -> [String]
lookupHeaders name x = [ v | Header n v <- listHeaders x, name == n ]

lookupHeader :: HasHeaders a => HeaderName -> a -> Maybe String
lookupHeader n x = listToMaybe $ lookupHeaders n x


-- * Constructing specific headers

contentLengthHeader :: Integer -> Header
contentLengthHeader i = Header HdrContentLength (show i)

contentTypeHeader :: String -> Header
contentTypeHeader t = Header HdrContentType t

lastModifiedHeader :: ClockTime -> Header
lastModifiedHeader t = Header HdrLastModified (formatTimeSensibly (toUTCTime t))

transferCodingHeader :: TransferCoding -> Header
transferCodingHeader te = Header HdrTransferEncoding (transferCodingStr te)

data TransferCoding
  = ChunkedTransferCoding
  | GzipTransferCoding
  | CompressTransferCoding
  | DeflateTransferCoding
  deriving Eq

transferCodingStr :: TransferCoding -> String
transferCodingStr ChunkedTransferCoding  = "chunked"
transferCodingStr GzipTransferCoding     = "gzip"
transferCodingStr CompressTransferCoding = "compress"
transferCodingStr DeflateTransferCoding  = "deflate"

validTransferCoding :: [TransferCoding] -> Bool
validTransferCoding codings
  | null codings
    || last codings == ChunkedTransferCoding
       && ChunkedTransferCoding `notElem` init codings = True
  | otherwise = False


-- * Values of specific headers

getContentType :: HasHeaders a => a -> Maybe String
getContentType x = lookupHeader HdrContentType x

getContentLength :: HasHeaders a => a -> Maybe Integer
getContentLength x = lookupHeader HdrContentLength x >>= readM


-- * Parsing

pHeaders :: Parser Headers
pHeaders = liftM Headers $ many pHeader

pHeader :: Parser Header
pHeader =
    do name <- pToken
       char ':'
       many pWS1
       line <- lineString
       pCRLF
       extraLines <- many extraFieldLine
       return $ Header (mkHeaderName name) (concat (line:extraLines))

extraFieldLine :: Parser String
extraFieldLine =
    do sp <- pWS1
       line <- lineString
       pCRLF
       return (sp:line)
