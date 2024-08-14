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

module Util where

import Control.Exception as Exception
import Control.Concurrent 
import Control.Monad
import Data.Array.IO
import Data.Char
import Data.List
import Data.Ratio (numerator)
import Foreign.C.Error (getErrno, eNOENT, eNOTDIR)
import Network.Socket as Socket
import System.IO
import System.Exit
import System.Locale
import System.Posix
import System.Time
import System.IO.Error (tryIOError)
import Network.Socket (HostAddress, hostAddressToTuple)

-----------------------------------------------------------------------------
-- Utils

-- ToDo: deHex is supposed to remove the '%'-encoding
deHex :: String -> String
deHex s = s

hPutStrCrLf h s = hPutStr h s >> hPutChar h '\r' >> hPutChar h '\n'

die :: String -> IO ()
die err = do hPutStrLn stderr err
             exitFailure

-----------------------------------------------------------------------------
-- String utils

readM :: (Read a, Monad m, MonadFail m) => String -> m a
readM s = readSM reads s

readSM :: (Monad m, MonadFail m) => ReadS a -> String -> m a
readSM f s = case f s of
                      [] -> fail $ "No parse of " ++ show s
                      [(x,[])] -> return x
                      [(_,_)]  -> fail $ "Junk at end of " ++ show s
                      _  -> fail $ "Ambiguous parse of " ++ show s

lookupLC :: String -> [(String,a)] -> Maybe a
lookupLC s xs = lookup (map toLower s) [(map toLower n,v) | (n,v) <- xs]

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

unlinesS :: [ShowS] -> ShowS
unlinesS = concatS . map (. showChar '\n')


-----------------------------------------------------------------------------
-- List utils

-- Split a list at some delimiter. 
splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = [[]]
splitBy f xs = first : case rest of 
                         _:ys -> splitBy f ys
                         []   -> []
    where (first, rest) = break f xs

glue :: [a] -> [[a]] -> [a]
glue g = concat . intersperse g

splits :: [a] -> [([a],[a])]
splits xs = zip (inits xs) (tails xs)

dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix xs pref | pref `isPrefixOf` xs = drop (length pref) xs
                   | otherwise            = xs

dropSuffix :: Eq a => [a] -> [a] -> [a]
dropSuffix xs suf = reverse (reverse xs `dropPrefix` reverse suf)

-----------------------------------------------------------------------------
-- File path utils

splitPath :: FilePath -> [String]
splitPath = splitBy (=='/')

joinPath :: [String] -> FilePath
joinPath = glue "/"

-- Get the directory component of a path
-- FIXME: is this good enough?
dirname :: FilePath -> FilePath
dirname = reverse . dropWhile (/= '/') . reverse

-----------------------------------------------------------------------------
-- Monad utils

firstJustM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJustM _ []     = return Nothing
firstJustM f (x:xs) = f x >>= maybe (firstJustM f xs) (return . Just)


-----------------------------------------------------------------------------
-- Parsec utils

-----------------------------------------------------------------------------
-- Time utils

formatTimeSensibly :: CalendarTime -> String
formatTimeSensibly time
   = formatCalendarTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" time

epochTimeToClockTime :: EpochTime -> ClockTime
epochTimeToClockTime epoch_time = TOD (numToInteger epoch_time) 0
  where numToInteger = numerator . toRational

-----------------------------------------------------------------------------
-- concurrency utilities

-- block forever
wait :: IO a
wait = newEmptyMVar >>= takeMVar

-----------------------------------------------------------------------------
-- networking utils

accept :: Socket                -- Listening Socket
       -> IO (Handle,SockAddr)  -- StdIO Handle for read/write
accept sock = do
 (sock', addr) <- Socket.accept sock
 handle <- socketToHandle sock' ReadWriteMode
 return (handle,addr)

-----------------------------------------------------------------------------
-- file utils

statFile :: String -> IO (Maybe FileStatus)
statFile = stat_ getFileStatus

statSymLink :: String -> IO (Maybe FileStatus)
statSymLink = stat_ getSymbolicLinkStatus

stat_ :: (FilePath -> IO FileStatus) -> String -> IO (Maybe FileStatus)
stat_ f filename = do
  maybe_stat <- tryIOError (f filename)
  case maybe_stat of
       Left e -> do
          errno <- getErrno
          if errno == eNOENT || errno == eNOTDIR
             then return Nothing
             else ioError e
       Right stat ->
          return (Just stat)

isSymLink :: FilePath -> IO Bool
isSymLink = liftM (maybe False isSymbolicLink) . statSymLink

-----------------------------------------------------------------------------
-- I/O utils

bufsize = 4 * 1024 :: Int

-- squirt data from 'rd' into 'wr' as fast as possible.  We use a 4k
-- single buffer.
squirt :: Handle -> Handle -> IO ()
squirt rd wr = do
  arr <- newArray_ (0, bufsize-1)
  let loop = do r <- hGetArray rd arr bufsize
                if (r == 0) 
                   then return ()
                   else if (r < bufsize) 
                            then hPutArray wr arr r
                            else hPutArray wr arr bufsize >> loop
  loop

-- | Read the given number of bytes from a Handle
hGetChars :: Handle -> Int -> IO String
hGetChars h 0 = return ""
hGetChars h n = do arr <- newArray_ (0, n-1)
                   r   <- hGetArray h arr n
                   when (r < n) $ fail $ ""
                   -- FIXME: input encoding?
                   liftM (map (toEnum . fromEnum)) $ getElems arr

-----------------------------------------------------------------------------
-- Exception utils

-- | Catch IO Errors for which a given predicate is true.
catchSomeIOErrors :: (IOError -> Bool) -> IO a -> (IOError -> IO a) -> IO a
catchSomeIOErrors p = catchJust p'
  where p' e | p e = Just e
        p' _ = Nothing

inet_ntoa :: HostAddress -> String
inet_ntoa a = tupleToString $ hostAddressToTuple a
  where tupleToString (a, b, c, d) = concat $ intersperse "." $ map show [a, b, c, d]
