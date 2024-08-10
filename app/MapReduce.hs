module Main where

import Control.Parallel.Strategies (using, NFData, Strategy, Eval, rdeepseq)
import Control.Exception (bracket, finally)
import Control.Monad (forM, liftM, forM_, join)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy.Char8 as LB
import GHC.Conc (numCapabilities, par, pseq)
import System.IO
import System.Environment (getArgs)

parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strat f xs = map f xs `using` parList strat

parList :: Strategy a -> Strategy [a]
parList strat []     = return []
parList strat (x:xs) = do
  x' <- strat x
  xs' <- parList strat xs
  return (x':xs')

mapReduce
    :: Strategy b    -- evaluation strategy for mapping
    -> (a -> b)      -- map function
    -> Strategy c    -- evaluation strategy for reduction
    -> ([b] -> c)    -- reduce function
    -> [a]           -- list to map over
    -> c
mapReduce mapStrat mapFunc reduceStrat reduceFunc input =
    mapResult `pseq` reduceResult
  where mapResult    = parMap mapStrat mapFunc input
        reduceResult = reduceFunc mapResult `using` reduceStrat

data ChunkSpec = CS {
      chunkOffset :: !Int64
    , chunkLength :: !Int64
    } deriving (Eq, Show)

withChunks :: (NFData a) =>
              (FilePath -> IO [ChunkSpec])
           -> ([LB.ByteString] -> a)
           -> FilePath
           -> IO a
withChunks chunkFunc process path = do
  chunks <- chunkedRead chunkFunc path
  return (process chunks)

chunkedReadWith :: (NFData a) =>
                   ([LB.ByteString] -> a) -> FilePath -> IO a
chunkedReadWith func path =
    withChunks (lineChunks (numCapabilities * 4)) func path

chunkedRead :: (FilePath -> IO [ChunkSpec])
            -> FilePath
            -> IO ([LB.ByteString])
chunkedRead chunkFunc path = do
  chunks <- chunkFunc path
  forM chunks $ \spec -> do
    h <- openFile path ReadMode
    hSeek h AbsoluteSeek (fromIntegral (chunkOffset spec))
    LB.take (chunkLength spec) `liftM` LB.hGetContents h

lineChunks :: Int -> FilePath -> IO [ChunkSpec]
lineChunks numChunks path = do
  bracket (openFile path ReadMode) hClose $ \h -> do
    totalSize <- fromIntegral `liftM` hFileSize h
    let chunkSize = totalSize `div` fromIntegral numChunks
        findChunks offset = do
          let newOffset = offset + chunkSize
          hSeek h AbsoluteSeek (fromIntegral newOffset)
          let findNewline off = do
                eof <- hIsEOF h
                if eof
                  then return [CS offset (totalSize - offset)]
                  else do
                    bytes <- LB.hGet h 4096
                    case LB.elemIndex '\n' bytes of
                      Just n -> do
                        chunks <- findChunks (off + n + 1)
                        return (CS offset (off+n+1 - offset):chunks)
                      Nothing -> findNewline (off + LB.length bytes)
          findNewline newOffset
    findChunks 0

lineCount :: [LB.ByteString] -> Int64
lineCount = mapReduce rdeepseq (LB.count '\n') rdeepseq sum

lineCount2 :: [LB.ByteString] -> Int64
lineCount2 s = sum $ map (LB.count '\n') s

main = do
  args <- getArgs
  forM_ args $ \path -> do
    numLines <- chunkedReadWith lineCount path
    putStrLn $ path ++ ": " ++ show numLines
