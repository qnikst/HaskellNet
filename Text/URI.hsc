{-# OPTIONS -fglasgow-exts #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.URI
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  stable
-- Portability :  portable
-- 
-- URI parser and utilities
-- 


module Text.URI
    ( URI(..)
    , port'
    , uri, uri'
    , escape, unescape
    , parseURI, parseURI'
    , portToName, nameToPort
    )
    where

import Network
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Text.Packrat.Parse
import Text.Packrat.Pos

import Data.Array
import Data.Char
import Data.List
import Data.Maybe
import Data.Word (Word8)
import Numeric

import Network.BSD (getServicePortNumber, getServiceByPort, serviceName, servicePort)
import System.IO.Unsafe

data URI = URI { scheme   :: String
               , host     :: String
               , user     :: String
               , password :: String
               , port     :: Maybe PortNumber
               , path     :: String
               , query    :: String
               , fragment :: String
               }
#ifdef DEBUG
         deriving (Eq, Show)
#else
         deriving (Eq)

instance Show URI where
    showsPrec d uri = showParen (d>app_prec) $ foldl1 (.) $ show' uri
        where app_prec = 10
              show' (URI sch host u p port path q f) =
                  [ showScheme sch
                  , showUserInfo (escape $ map c2w u) (escape $ map c2w p)
                  , showString $ escape $ map c2w host
                  , showPort port
                  , showString $ escape $ map c2w path
                  , showQuery $ escape $ map c2w q
                  , showFragment $ escape $ map c2w f ]
              showScheme ""      = id
              showScheme s       = showString s . ("://"++)
              showUserInfo "" "" = id
              showUserInfo u ""  = showString u . ('@':)
              showUserInfo u p   = showString u . (':':)
                                   . showString p . ('@':)
              showPort Nothing   = id
              showPort (Just p)  = (':':) . showInt p
              showQuery ""       = id
              showQuery q        = ('?':) . showString q
              showFragment ""    = id
              showFragment f     = ('#':) . showFragment f
              c2w = toEnum . fromEnum
#endif

-- | Obtain the port number for the URI.  If no port number exists,
-- port' would like to estimate the port number from the scheme name.
-- If both failed, it raises an error.
port' :: URI -> PortNumber
port' (URI { port = Just p }) = p
port' (URI { scheme = s })    =
    case nameToPort s of
      Just p  -> p
      Nothing -> error ("no service entries for " ++ s)

    
-- | Parse URI string simiar to 'parseURI'.  The difference is that it
-- raises an error for the case of parse failed, not returns Nothing.
uri' :: ByteString -> URI
uri' u = case dvURI (parse (Pos "<uri>" 1 1) u) of
           Parsed v d' e' -> v
           NoParse e      -> error (show e)
uri :: String -> URI
uri = uri' . BS.pack

-- | Parse URI string and returns the result. If the parse is failed,
-- it simply returns Nothing.
parseURI :: String -> Maybe URI
parseURI = parseURI' . BS.pack
parseURI' :: ByteString -> Maybe URI
parseURI' u = case dvURI (parse (Pos "<uri>" 1 1) u) of
                 Parsed v d' e' -> Just v
                 NoParse e      -> Nothing 

parse pos s = d
    where d = URIDerivs puri psch phost pui pport pabs ppath pq pf pch pos
          puri = pURI d
          psch = pScheme d
          phost = pHost d
          pui   = pUserInfo d
          pport = pPort d
          pabs  = pPathAbs d
          ppath = pPath d
          pq    = pQuery d
          pf    = pFragment d
          pch | BS.null s = NoParse (eofError d)
              | otherwise =
                  let (c, s') = (BS.head s, BS.tail s)
                  in Parsed c (parse (nextPos pos c) s') (nullError d)

              

data URIDerivs = URIDerivs { dvURI      :: Result URIDerivs URI
                           , dvScheme   :: Result URIDerivs String
                           , dvHost     :: Result URIDerivs String
                           , dvUserInfo :: Result URIDerivs [String]
                           , dvPort     :: Result URIDerivs PortNumber
                           , dvPathAbs  :: Result URIDerivs String
                           , dvPath     :: Result URIDerivs String
                           , dvQuery    :: Result URIDerivs String
                           , dvFragment :: Result URIDerivs String
                           , advChar    :: Result URIDerivs Char
                           , advPos     :: Pos
                           }

instance Derivs URIDerivs where
    dvChar = advChar
    dvPos  = advPos


unescape :: String -> String
unescape ('%':c1:c2:cs) = chr ((hex c1)*16+(hex c2)) : unescape cs
    where arr = array ('0', 'f') $ zip "0123456789abcdef" [0..]
          hex = (arr!) . toLower
unescape (c:cs)         = c : unescape cs
unescape ""             = ""

escape :: [Word8] -> String
escape [] = ""
escape (c:cs) | c' `elem` validChars = c' : escape cs
              | otherwise            = escChar c ++ escape cs
    where validChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"!$^&*-_=+|/."
          escChar c  = '%' : map (arr!) [c `div` 16, c `mod` 16]
          arr = listArray (0, 15) "0123456789abcdef"
          w2c = toEnum . fromEnum
          c' = w2c c

consURI :: String -- ^ scheme
        -> String -- ^ host name
        -> String -- ^ user
        -> String -- ^ password
        -> Maybe PortNumber
        -> String -- ^ path
        -> String -- ^ query
        -> String -- ^ fragment
        -> URI
consURI s h u p port path q f =
    URI (unescape s) (unescape h) (unescape u) (unescape p) port (unescape path) (unescape q) (unescape f)


pURI :: URIDerivs -> Result URIDerivs URI
Parser pURI = do sch <- Parser dvScheme
                 char ':'
                 uri <- hierPart sch
                 q <- option "" (Parser dvQuery)
                 f <- option "" (Parser dvFragment)
                 eof
                 return (uri { query = q, fragment = f })
    where hierPart sch = do string "//"
                            ui <- option [] (Parser dvUserInfo)
                            host <- Parser dvHost
                            port <- optional (Parser dvPort)
                            path <- option "" (Parser dvPathAbs)
                            let uri = consURI sch host "" "" port path "" ""
                            case ui of
                              [u, p] -> return $ uri { user = u, password = p }
                              [u]    -> return $ uri { user = u }
                              []     -> return uri
                     <|> do path <- option "" (Parser dvPath)
                            return $ URI sch "" "" "" Nothing path "" ""

pScheme, pHost, pPath, pQuery, pFragment :: URIDerivs
                                         -> Result URIDerivs String
Parser pScheme = do c <- oneOf (['a'..'z']++['A'..'Z'])
                    rest <- many $ oneOf (['a'..'z']++['A'..'Z']++['0'..'9']++"+-.")
                    return (c:rest)
Parser pHost = many1 (noneOf ":/")
Parser pPathAbs = char '/' >> Parser dvPath >>= return . ('/':)
Parser pPath = many1 (noneOf "#?")
Parser pQuery = char '?' >> many1 (noneOf "#")
Parser pFragment = char '#' >> many1 anyChar


pUserInfo :: URIDerivs -> Result URIDerivs [String]
Parser pUserInfo = do ui <- many1 (noneOf ":@") `sepBy1` char ':'
                      char '@'
                      return ui

pPort :: URIDerivs -> Result URIDerivs PortNumber
Parser pPort = char ':' >> many1 digit >>= return . toEnum . read


portToName :: PortNumber -> Maybe String
portToName p = unsafePerformIO $
                (do s <- getServiceByPort p "tcp"
                    return $ Just $ serviceName s)
                `catch`
                (\_ -> return Nothing)

nameToPort :: String -> Maybe PortNumber
nameToPort n = unsafePerformIO $
               fmap Just (getServicePortNumber n)
                        `catch` (\_ -> return Nothing)
