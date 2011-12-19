----------------------------------------------------------------------
-- |
-- Module      :  Text.Mime
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Mime Parser
-- 

module Text.Mime where

import Text.Packrat.Parse hiding (space, spaces, Message)
import Text.Packrat.Pos

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import qualified Codec.Binary.Base64.String as B64 (encode, decode)
import Data.Digest.MD5 (hash)

import Data.Maybe
import Data.Char
import Data.List
import Data.Bits
import Data.Array

import qualified Text.PrettyPrint.HughesPJ as PP

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
       return (capital field, unwords (value:cont))

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
              case lookup "Content-Type" headers of
                Nothing -> False
                Just s  -> "multipart/" == (take 10 s)
          boundary headers =
              case lookup "Content-Type" headers of
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
                         in do string ("--"++b')
                               lineBreak
                               mimeBodies <- many $ mimeInner $
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
-- RFC 2047 Mime Header Extentions Parser
-- 

type CharSet = String
data RFC2047Derivs =
    RFC2047Derivs { dvHeaderExts :: Result RFC2047Derivs [(CharSet, String)]
                  , hdvChar :: Result RFC2047Derivs Char
                  , hdvPos :: Pos
                  }

instance Derivs RFC2047Derivs where
    dvChar = hdvChar
    dvPos  = hdvPos

headerExts :: ByteString -> [(CharSet, String)]
headerExts s = case dvHeaderExts (p (Pos "<input>" 1 1) s) of
                 Parsed v d' e' -> v
                 NoParse e      -> error (show e)
    where p pos s = d
              where d = RFC2047Derivs mstr chr pos
                    mstr = pHeaderExts d
                    chr  = if BS.null s
                           then NoParse (eofError d)
                           else let (c, s') = (BS.head s, BS.tail s)
                                in Parsed c (p (nextPos pos c) s')
                                       (nullError d)

headerExts' :: String -> [(CharSet, String)]
headerExts' = headerExts . BS.pack


pHeaderExts :: RFC2047Derivs -> Result RFC2047Derivs [(CharSet, String)]
Parser pHeaderExts = (getRFC2047 <|> normalStr) `sepBy` (many $ oneOf " \t")
    where getRFC2047 =
              do string "=?"
                 charset <- anyChar `manyTill` char '?'
                 mechanism <- choice [char 'B', char 'b', char 'Q', char 'q']
                 char '?'
                 body <- if mechanism `elem` "bB"
                         then decodeB64
                         else decodeQuoted
                 return (charset, body)
          normalStr :: Derivs d => Parser d (String, String)
          normalStr =
              do body <- many1 $ noneOf " \t"
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
               string "?="
               return $ concat bodies
    where pQuartet = do a <- b64Chars
                        b <- b64Chars
                        c <- b64Chars
                        d <- b64Chars
                        return $ decodeChars (a,b,c,d)
                 <|> do a <- b64Chars
                        b <- b64Chars
                        c <- b64Chars
                        char '='
                        return $ init $ decodeChars (a, b, c, 0)
                 <|> do a <- b64Chars
                        b <- b64Chars
                        string "=="
                        return [head $ decodeChars (a, b, 0, 0)]
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


----------------------------------------------------------------------
-- Mime Documents Pretty printer
--

showHeader' :: Header -> PP.Doc
showHeader' (field, value) =
    (PP.text (capital field) PP.<> PP.char ':') PP.<+>
      PP.fsep (map PP.text (words value))

showHeader :: CharSet -> Header -> PP.Doc
showHeader charset (field, value) =
    (PP.text (capital field) PP.<> PP.char ':') PP.<+>
      (PP.fsep $ map (PP.text . prepB64) $ separate value)
    where separate s | length s < 76 - length field = [s]
                     | isJust (find isSpace s)       =
                         concatMap separate $ words s
                     | length s < 998 - length field = [s]
                     | otherwise =
                         let (s', s'') = splitAt (998 - length field) s
                         in s' : separate s''
          prepB64 s | isJust $ find (not.isPrint) s =
                        "=?" ++ charset ++ "?b?" ++ b64Encode s ++ "?="
                    | otherwise = s

capital :: String -> String
capital "" = ""
capital (c:cs) | isAlpha c = toUpper c : inner cs
               | otherwise = c : capital cs
    where inner "" = ""
          inner (c:cs) | isAlpha c = toLower c : inner cs
                       | otherwise = c : capital cs

b64Encode :: String -> String
b64Encode = B64.encode . map (toEnum.ord)

showMessage :: CharSet -> Message -> PP.Doc
showMessage charset (hdrs, body) =
    PP.vcat ((map (showHeader charset) hdrs) ++
               [ PP.empty, PP.text (BS.unpack body)])

showMime :: CharSet -> Mime -> PP.Doc
showMime charset (SinglePart hdrs body) =
    PP.vcat ((map (showHeader charset) hdrs) ++
               [ PP.empty, PP.text (BS.unpack body)])
showMime charset (MultiPart hdrs bodies)
    | isJust $ lookup "Content-Type" hdrs =
      PP.vcat $ map (showHeader charset) hdrs ++
            (PP.empty : mixture boundary (map (showMime charset) bodies))
    | otherwise =
        PP.vcat $ map (showHeader charset) (hdrs++[newHeader]) ++
              (PP.empty : mixture newBoundary
                     (map (showMime charset) bodies))
    where boundary = let s = fromJust $ lookup "Content-Type" hdrs
                         s' = drop 9 $ head $
                                 dropWhile ((/="boundary=") . (take 9))
                                               (tails s)
                     in if head s' == '"'
                        then takeWhile (/='"') $ tail s'
                        else takeWhile isAlphaNum s'
          newBoundary = md5Hash $ concatMap snd hdrs
          newHeader   = ("Content-Type", "multipart/mixed; boundary=\"" ++ newBoundary ++ "\"")
          mixture b [] = [PP.text ("--"++b++"--")]
          mixture b (body:bodies) = PP.text ("--"++b) : body : mixture b bodies

md5Hash = showOctet . hash . map (toEnum.ord)
    where showOctet = concat . map hexChars
          hexChars c = [arr ! (c `div` 16), arr ! (c `mod` 16)]
          arr = listArray (0, 15) "0123456789abcdef"

