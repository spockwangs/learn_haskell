-- -----------------------------------------------------------------------------
-- Copyright 2002, Simon Marlow.
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

module ConfigParser (parseConfig) where

import Config
import Parse
import Util

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token


type ConfigBuilder = Config -> Config

p :: TokenParser st
p = makeTokenParser tokenDef

tokenDef :: LanguageDef st
tokenDef = emptyDef { 
                     commentLine     = "#",
                     nestedComments  = False,
                     reservedOpNames = [],
                     reservedNames   = [],
                     caseSensitive   = False
                    }


parseConfig :: String -> IO (Either ParseError ConfigBuilder)
parseConfig fname
  = parseFromFile configParser fname

configParser :: Parser ConfigBuilder
configParser = do
  whiteSpace p
  cs <- many configLine
  eof
  return (fixConfig . foldr (.) id cs)

fixConfig :: Config -> Config
fixConfig conf = conf { listen = f (listen conf) }
  where f xs | length xs > 1 = init xs
             | otherwise     = xs

configLine :: Parser ConfigBuilder
configLine
 = do (reserved p "user"                   >> p_user)
  <|> (reserved p "group"                  >> p_group)
  <|> (reserved p "timeout"                >> p_timeout)
  <|> (reserved p "keepalivetimeout"       >> p_keepAliveTimeout)
  <|> (reserved p "maxclients"             >> p_maxClients)
  <|> (reserved p "listen"                 >> p_listen)
  <|> (reserved p "serveradmin"            >> p_serverAdmin)
  <|> (reserved p "servername"             >> p_serverName)
  <|> (reserved p "serveralias"            >> p_serverAlias)
  <|> (reserved p "usecanonicalname"       >> p_useCanonicalName)
  <|> (reserved p "documentroot"           >> p_documentRoot)
  <|> (reserved p "userdir"                >> p_userDir)
  <|> (reserved p "directoryindex"         >> p_directoryIndex)
  <|> (reserved p "accessfilename"         >> p_accessFileName)
  <|> (reserved p "typesconfig"            >> p_typesConfig)
  <|> (reserved p "defaulttype"            >> p_defaultType)
  <|> (reserved p "hostnamelookups"        >> p_hostnameLookups)
  <|> (reserved p "errorlog"               >> p_errorLog)
  <|> (reserved p "loglevel"               >> p_logLevel)
  <|> (reserved p "customlog"              >> p_customLog)
  <|> (reserved p "listen"                 >> p_listen)
  <|> (reserved p "addlanguage"            >> p_addlanguage)
  <|> (reserved p "languagepriority"       >> p_languagepriority)

p_user  = do str <- stringLiteral p; return (\c -> c{user = str})
p_group = do str <- stringLiteral p; return (\c -> c{group = str})
p_timeout = do i <- int; return (\c -> c{requestTimeout = i})
p_keepAliveTimeout = do i <- int; return (\c -> c{keepAliveTimeout = i})
p_maxClients  = do i <- int; return (\c -> c{maxClients = i})
p_serverAdmin = do str <- stringLiteral p; return (\c -> c{serverAdmin = str})
p_serverName = do str <- stringLiteral p; return (\c -> c{serverName = str})
p_serverAlias = do str <- stringLiteral p
                   return (\c -> c{serverAlias = str : serverAlias c})
p_useCanonicalName = do b <- bool; return (\c -> c{useCanonicalName = b})
p_documentRoot = do str <- stringLiteral p; return (\c -> c{documentRoot = str})
p_userDir = do str <- stringLiteral p; return (\c -> c{userDir = str})
p_directoryIndex = do str <- stringLiteral p; return (\c -> c{directoryIndex = str})
p_accessFileName = do str <- stringLiteral p; return (\c -> c{accessFileName = str})
p_typesConfig = do str <- stringLiteral p; return (\c -> c{typesConfig = str})
p_defaultType = do str <- stringLiteral p; return (\c -> c{defaultType = str})

p_hostnameLookups = do b <- bool; return (\c -> c{hostnameLookups = b})
p_errorLog = do str <- stringLiteral p; return (\c -> c{errorLogFile = str})

p_logLevel = do i <- identifier p >>= readM
                return (\c -> c{logLevel = i})

p_customLog = do file <- stringLiteral p
                 format <- stringLiteral p
                 return (\c -> c { customLogs = (file,format) : customLogs c})

p_listen = do maddr <- p_addr
              port <- int
              return (\c -> c{ listen = (maddr,port) : listen c})
 where
  p_addr = option Nothing $ try $ do addr <- p_ip_addr
                                     char ':'
                                     return $ Just addr
  p_ip_addr = do b1 <- p_dec_byte
                 char '.'
                 b2 <- p_dec_byte
                 char '.'
                 b3 <- p_dec_byte
                 char '.'
                 b4 <- p_dec_byte
                 return (b1++"."++b2++"."++b3++"."++b4)
  p_dec_byte = countBetween 1 3 digit

p_addlanguage = do lang <- stringLiteral p; ext <- stringLiteral p; return (\c -> c{addLanguage = (lang,ext) : addLanguage c})
p_languagepriority = do langs <- many (stringLiteral p); return (\c -> c{languagePriority = langs})

bool = do { reserved p "On"; return True } 
   <|> do { reserved p "Off"; return False }

int :: Parser Int
int = do i <- integer p; return (fromInteger i)

