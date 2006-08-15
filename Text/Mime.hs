----------------------------------------------------------------------
-- |
-- Module      :  Text.Mime
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  stable
-- Portability :  portable
-- 
-- Mime Parser
-- 

module Text.Mime where

import Text.Packrat.Parse hiding (space, spaces, Message)
import Text.Packrat.Pos

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Data.Maybe
import Data.Char
import Data.List
import Data.Bits

data Mime = SinglePart [Header] ByteString
          | MultiPart [Header] [Mime]
            deriving (Eq, Show)

type Message = ([Header], ByteString)

type Header     = (FieldName, FieldValue)
type FieldName  = String
type FieldValue = String

data MimeDerivs =
    MimeDerivs { dvMessage :: Result MimeDerivs Message
               , dvMime    :: Result MimeDerivs Mime
               , dvHeader  :: Result MimeDerivs Header
               , dvRest   :: Result MimeDerivs ByteString
               , advChar  :: Result MimeDerivs Char
               , advPos   :: Pos
               }


instance Derivs MimeDerivs where
    dvChar = advChar
    dvPos  = advPos


mime :: ByteString -> Mime
mime = eval dvMime

message :: ByteString -> Message
message = eval dvMessage


mime' :: String -> Mime
mime' = eval' dvMime

message' :: String -> Message
message' = eval' dvMessage



eval :: (MimeDerivs -> Result MimeDerivs r) -> ByteString -> r
eval pMain s = case pMain (parse (Pos "<input>" 1 1) s) of
                     Parsed v d' e' -> v
                     NoParse e      -> error (show e)

parse :: Pos -> ByteString -> MimeDerivs
parse pos s = d
    where d    = MimeDerivs message mime header rest chr pos
          message = pMessage d
          mime    = pMime d
          header  = pHeader d
          rest = if BS.null s
                 then NoParse (eofError d)
                 else Parsed s (parse (BS.foldl nextPos pos s) BS.empty)
                          (nullError d)
          chr  = if BS.null s
                 then NoParse (eofError d)
                 else let (c, s') = (BS.head s, BS.tail s)
                      in Parsed c (parse (nextPos pos c) s') (nullError d)

eval' :: (MimeDerivs -> Result MimeDerivs r) -> String -> r
eval' pMain s = case pMain (parse' (Pos "<input>" 1 1) s) of
                      Parsed v d' e' -> v
                      NoParse e      -> error (show e)

parse' :: Pos -> String -> MimeDerivs
parse' pos s = d
    where d    = MimeDerivs message mime header rest chr pos
          message = pMessage d
          mime    = pMime d
          header  = pHeader d
          rest = case s of
                   "" -> NoParse (eofError d)
                   _  -> Parsed (BS.pack s) (parse' (foldl nextPos pos s) "")
                           (nullError d)
          chr  = case s of
                   (c:s') -> Parsed c (parse' (nextPos pos c) s') (nullError d)
                   _      -> NoParse (eofError d)


----------------------------------------------------------------------

lineBreak :: Derivs d => Parser d String
lineBreak = string "\r\n" <|> string "\n"

pHeader :: MimeDerivs -> Result MimeDerivs (String, String)
Parser pHeader =
    do field <- noneOf "\r\n" `manyTill` char ':'
       many (oneOf " \t")
       value <- noneOf "\r\n" `manyTill` lineBreak
       cont <- many (many1 (oneOf " \t")  >> (anyChar `manyTill` lineBreak))
       return (map toLower field, unwords (value:cont))

pMessage :: MimeDerivs -> Result MimeDerivs Message
Parser pMessage = do headers <- many (Parser dvHeader)
                     lineBreak
                     body <- Parser dvRest
                     return (headers, body)

pMime :: MimeDerivs -> Result MimeDerivs Mime
Parser pMime =
    do headers <- many (Parser dvHeader)
       lineBreak
       if isMultipart headers
         then let b = boundary headers
              in do string ("--"++b)
                    lineBreak
                    mimeBodies <- many $ mimeInner $ string ("--"++b)
                    mimeLast <- mimeInner $ string ("--"++b++"--")
                    return $ MultiPart headers (mimeBodies++[mimeLast])
         else do body <- Parser dvRest
                 return $ SinglePart headers body
    where isMultipart headers =
              case lookup "content-type" headers of
                Nothing -> False
                Just s  -> "multipart/" == (take 10 s)
          boundary headers =
              case lookup "content-type" headers of
                Nothing -> fail ""
                Just s  -> let s' = drop 9 $ head $
                                    dropWhile ((/="boundary=") . (take 9))
                                      (tails s)
                           in if head s' == '"'
                              then takeWhile (/='"') $ tail s'
                              else takeWhile isAlphaNum s'
          mimeInner b =
              do headers <- many (Parser dvHeader)
                 lineBreak
                 if isMultipart headers
                    then let b' = boundary headers
                         in do mimeBodies <- many $ mimeInner $
                                               string ("--"++b')
                               mimeLast <- mimeInner $ string ("--"++b'++"--")
                               b >> lineBreak
                               return $ MultiPart headers
                                          (mimeBodies++[mimeLast])
                    else do body <- (anyChar `manyTill` lineBreak) `manyTill`
                                      (b >> lineBreak)
                            return $ SinglePart headers
                                       (BS.pack $ unlines body)

----------------------------------------------------------------------

type CharSet = String
data RFC2047Derivs =
    RFC2047Derivs { dvMimeStr :: Result RFC2047Derivs [(CharSet, String)]
                  , hdvChar :: Result RFC2047Derivs Char
                  , hdvPos :: Pos
                  }

instance Derivs RFC2047Derivs where
    dvChar = hdvChar
    dvPos  = hdvPos

mimeHeader :: ByteString -> [(CharSet, String)]
mimeHeader s = case dvMimeStr (p (Pos "<input>" 1 1) s) of
                 Parsed v d' e' -> v
                 NoParse e      -> error (show e)
    where p pos s = d
              where d = RFC2047Derivs mstr chr pos
                    mstr = pMimeStr d
                    chr  = if BS.null s
                           then NoParse (eofError d)
                           else let (c, s') = (BS.head s, BS.tail s)
                                in Parsed c (p (nextPos pos c) s')
                                       (nullError d)

mimeHeader' :: String -> [(CharSet, String)]
mimeHeader' = mimeHeader . BS.pack


pMimeStr :: RFC2047Derivs -> Result RFC2047Derivs [(CharSet, String)]
Parser pMimeStr = many $ getRFC2047 <|> normalStr
    where getRFC2047 =
              do string "=?"
                 charset <- anyChar `manyTill` char '?'
                 mechanism <- choice [char 'B', char 'b', char 'Q', char 'q']
                 char '?'
                 body <- if mechanism `elem` "bB"
                         then decodeB64
                         else decodeQuoted
                 return (charset, body)
          normalStr =
              do body <- anyChar `manyTill` char '='
                 return ("", body)

decodeQuoted :: Derivs d => Parser d String
decodeQuoted = do cs <- many (quotedChar <|> noneOf "?")
                  string "?="
                  return cs
    where quotedChar = do char '='
                          n1 <- hexChar
                          n2 <- hexChar
                          return $ chr $ n1 * 16 + n2
          hexChar = choice [ do { c <- oneOf ['0'..'9']
                                ; return $ ord c - ord '0' }
                           , do { c <- oneOf (['A'..'F']++['a'..'f'])
                                ; return $ ord (toLower c) - ord 'a' + 10 }
                           ]
                         

decodeB64 :: Derivs d => Parser d String
decodeB64 = do bodies <- many pQuartet
               optional (char '=' >> optional (char '='))
               string "?="
               return $ concat bodies
    where pQuartet = do a <- b64Chars
                        b <- b64Chars
                        c <- b64Chars
                        d <- b64Chars
                        return $ decodeChars (a,b,c,d)
          b64Chars =
              do many $ noneOf (['a'..'z']++['A'..'Z']++['0'..'9']++"+/?=")
                 choice [ do { c <- oneOf ['a'..'z']
                             ; return $ ord c - ord 'a' + 26 }
                        , do { c <- oneOf ['A'..'Z']
                             ; return $ ord c - ord 'A' }
                        , do { c <- oneOf ['0'..'9']
                             ; return $ ord c - ord '0' + 52 }
                        , char '+' >> return 62
                        , char '/' >> return 63
                        ]
          decodeChars (a, b, c, d) =
              map chr [ n `shiftR` 16 .&. 0xff
                      , n `shiftR`  8 .&. 0xff
                      , n             .&. 0xff]
              where n = a `shiftL` 18 .|. b `shiftL` 12 .|. c `shiftL` 6 .|. d


