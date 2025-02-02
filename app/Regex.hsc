{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Regex where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString (ByteString, useAsCString, empty)
import Data.ByteString.Internal (toForeignPtr)
import Data.ByteString.Unsafe (unsafeDrop, unsafeTake)
import System.IO.Unsafe (unsafePerformIO)

#include <pcre.h>

newtype PCREOption = PCREOption { unPCREOption :: CInt } deriving (Eq, Show)

#{enum PCREOption, PCREOption,
  caseless = PCRE_CASELESS,
  dollar_endonly = PCRE_DOLLAR_ENDONLY,
  dotall = PCRE_DOTALL
 }

combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0

newtype PCREExecOption = PCREExecOption { unwrap :: CInt } deriving (Eq, Show)

combineExecOptions :: [PCREExecOption] -> PCREExecOption
combineExecOptions = PCREExecOption . foldr ((.|.). unwrap) 0

newtype PCRE = PCRE (Ptr PCRE)

foreign import ccall unsafe "pcre.h pcre_compile"
  c_pcre_compile :: CString -> PCREOption -> Ptr CString -> Ptr CInt -> Ptr Word8 -> IO (Ptr PCRE)

data Regex = Regex !(ForeignPtr PCRE) !ByteString
  deriving (Eq, Ord, Show)

compile :: ByteString -> [PCREOption] -> Either String Regex
compile str flags = unsafePerformIO $
  useAsCString str $ \pat -> do
  alloca $ \errptr -> do
    alloca $ \erroffset -> do
      pcre_ptr <- c_pcre_compile pat (combineOptions flags) errptr erroffset nullPtr
      if pcre_ptr == nullPtr
        then do
        err <- peekCString =<< peek errptr
        return (Left err)
        else do
        reg <- newForeignPtr finalizerFree pcre_ptr
        return (Right (Regex reg str))

newtype PCREExtra = PCREExtra (Ptr PCREExtra)

foreign import ccall "pcre.h pcre_exec"
  c_pcre_exec :: Ptr PCRE -> Ptr PCREExtra -> Ptr Word8 -> CInt -> CInt -> PCREExecOption -> Ptr CInt -> CInt -> IO CInt

newtype PCREInfo = PCREInfo { unPCREInfo :: CInt }

#{enum PCREInfo, PCREInfo, info_capturecount = PCRE_INFO_CAPTURECOUNT}

foreign import ccall "pcre.h pcre_fullinfo"
  c_pcre_fullinfo :: Ptr PCRE -> Ptr PCREExtra -> PCREInfo -> Ptr a -> IO CInt

capturedCount :: Ptr PCRE -> IO Int
capturedCount regex_ptr =
  alloca $ \n_ptr -> do
  _ <- c_pcre_fullinfo regex_ptr nullPtr info_capturecount n_ptr
  return . fromIntegral =<< peek (n_ptr :: Ptr CInt)

match :: Regex -> ByteString -> [PCREExecOption] -> Maybe [ByteString]
match (Regex pcre_fp _) subject os = unsafePerformIO $ do
  withForeignPtr pcre_fp $ \pcre_ptr -> do
    n_capt <- capturedCount pcre_ptr
    let ovec_size = (n_capt + 1) * 3
        ovec_bytes = ovec_size * sizeOf (undefined :: CInt)
    allocaBytes ovec_bytes $ \ovec -> do
      let (str_fp, off, len) = toForeignPtr subject
      withForeignPtr str_fp $ \cstr -> do
        r <- c_pcre_exec pcre_ptr nullPtr (cstr `plusPtr` off) (fromIntegral len) 0 (combineExecOptions os) ovec (fromIntegral ovec_size)
        if r < 0
          then return Nothing
          else let loop n o acc =
                     if n == r
                     then return (Just (reverse acc))
                     else do
                       i <- peekElemOff ovec o
                       j <- peekElemOff ovec (o+1)
                       let s = substring i j subject
                       loop (n+1) (o+2) (s : acc)
               in loop 0 0 []
          where
            substring :: CInt -> CInt -> ByteString -> ByteString
            substring x y _ | x == y = empty
            substring a b s = end
              where
                start = unsafeDrop (fromIntegral a) s
                end = unsafeTake (fromIntegral (b-a)) start

matchStr :: ByteString -> ByteString -> Either String [ByteString]
matchStr pat s = do
  regex <- compile pat []
  match regex s [] `orDie` "not matched"
    where
      orDie :: Maybe a -> String -> Either String a
      Nothing `orDie` err = Left err
      (Just x) `orDie` _ = Right x
