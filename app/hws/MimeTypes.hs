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

module MimeTypes (
                  MimeTypes,
                  MimeType(..),
                  initMimeTypes,
                  mimeTypeOf,
                  pMimeType
                 ) where

import Parse

import Data.Map (Map)
import qualified Data.Map as Map hiding (Map)
import Text.ParserCombinators.Parsec

type MimeTypes = Map String MimeType

data MimeType = MimeType String String

instance Show MimeType where
   showsPrec _ (MimeType part1 part2) = showString (part1 ++ '/':part2)

mimeTypeOf :: MimeTypes -> FilePath -> Maybe MimeType
mimeTypeOf mime_types filename = 
    do let ext = extension filename
       if null ext 
         then Nothing 
         else Map.lookup ext mime_types

extension :: String -> String
extension fn = go (reverse fn) ""
  where go []      _   = ""
        go ('.':_) ext = ext
        go (x:s)   ext = go s (x:ext)

initMimeTypes :: FilePath -> IO MimeTypes
initMimeTypes mime_types_file = 
    do stuff <- readFile mime_types_file
       return $ Map.fromList (parseMimeTypes stuff)

parseMimeTypes :: String -> [(String,MimeType)]
parseMimeTypes file =
  [ (ext,val) 
  | Just (val,exts) <- map (parseMimeLine . takeWhile (/= '#')) (lines file)
  , ext <- exts
  ]

parseMimeLine :: String -> Maybe (MimeType, [String])
parseMimeLine l = case parse pMimeLine "MIME line" l of
                    Left _  -> Nothing
                    Right l -> Nothing

pMimeLine :: Parser (MimeType, [String])
pMimeLine = do t <- pMimeType
               es <- (spaces >> sepBy pToken spaces)
               return (t, es)

pMimeType :: Parser MimeType
pMimeType = do part1 <- pToken
               char '/'
               part2 <- pToken
               return $ MimeType part1 part2
