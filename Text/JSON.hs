{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-incoherent-instances #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.JSON
-- Copyright   :  (c) Masahiro Sakai 2006, Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  experimental
-- Portability :  portable
-- 
-- A JSON Parser and Serializer and type classes.
-- This code is originally written by Masahiro Sakai. 
-- The original code can be seen at <http://www.tom.sfc.keio.ac.jp/~sakai/d/?date=20060427#p02>
-- 

module Text.JSON
    ( -- * Basic Type and Type Class
      Jsonable(..)
    , JsonNode(..)
      -- * conversion Between String and Jsonable
    , parse, parse'
    , toDoc, toDocPP, pp, toDocRaw, toDocPPRaw
    -- * converters and encode checkers
    , toJsonString
    , encoder
    , utf8Conv, utf16BEConv, utf16LEConv, utf32BEConv, utf32LEConv
    )
where

import Control.Monad hiding (join)
import Text.Packrat.Parse
import Text.Packrat.Pos
import Text.Printf (printf)
import Data.Char (ord, chr, isControl)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.Word (Word8)
import Numeric (readHex, showHex)
import Text.PrettyPrint.HughesPJ hiding (char)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as BS

class Jsonable a where
    fromJson :: JsonNode -> a
    toJson :: a -> JsonNode
    jRead :: String -> a
    jShow :: a -> String

    jRead = fromJson . parse
    jShow = show . toDoc . toJson

instance Jsonable String where
    fromJson (String s) = s
    fromJson _          = error "type mismatch"
    toJson = String

instance Jsonable ByteString where
    fromJson (String s) = pack s
    fromJson _          = error "type mismatch"
    toJson = String . unpack

instance Jsonable Integer where
    fromJson (Number n) = floor n
    fromJson _          = error "type mismatch"
    toJson = Number . fromIntegral

instance Jsonable Int where
    fromJson (Number n) = floor n
    fromJson _          = error "type mismatch"
    toJson = Number . fromIntegral

instance Jsonable Double where
    fromJson (Number n) = n
    fromJson _          = error "type mismatch"
    toJson = Number

instance Jsonable Float where
    fromJson (Number n) = realToFrac n
    fromJson _          = error "type mismatch"
    toJson = Number . realToFrac

instance Jsonable Rational where
    fromJson (Number n) = toRational n
    fromJson _          = error "type mismatch"
    toJson = Number . fromRational

instance (Jsonable a) => Jsonable (M.Map String a) where
    fromJson (Object m) = M.map fromJson m
    fromJson _          = error "type mismatch"
    toJson = Object . M.map toJson

instance (Jsonable a) => Jsonable [(String, a)] where
    fromJson (Object m) = M.toList $ M.map fromJson m
    fromJson _          = error "type mismatch"
    toJson = Object . M.map toJson . M.fromList

instance (Jsonable a) => Jsonable [a] where
    fromJson (Array a) = map fromJson a
    fromJson _         = error "type mismatch"
    toJson a   = Array $ map toJson a

instance Jsonable Bool where
    fromJson (Bool b) = b
    fromJson _        = error "type mismatch"
    toJson = Bool

instance (Jsonable a) => Jsonable (Maybe a) where
    fromJson Null = Nothing
    fromJson a    = Just $ fromJson a
    toJson Nothing  = Null
    toJson (Just x) = toJson x

instance Jsonable JsonNode where
    fromJson = id
    toJson = id


data JsonNode
    = String String
    | Number !Double
    | Object !(M.Map String JsonNode)
    | Array [JsonNode]
    | Bool !Bool
    | Null
    deriving (Eq, Show)

data JsonDerivs = JsonDerivs { dvNode   :: Result JsonDerivs JsonNode
                             , dvStr    :: Result JsonDerivs String
                             , dvNumber :: Result JsonDerivs Double
                             , dvObject :: Result JsonDerivs (M.Map String JsonNode)
                             , dvArray  :: Result JsonDerivs [JsonNode]
                             , advChar  :: Result JsonDerivs Char
                             , advPos   :: Pos
                             }
instance Derivs JsonDerivs where
    dvChar = advChar
    dvPos  = advPos

tok :: Derivs d => Parser d a -> Parser d a
tok p = do{ x <- p; spaces; return x }

pValue :: JsonDerivs -> Result JsonDerivs JsonNode
Parser pValue = msum
                [ liftM String (Parser dvStr)
                , liftM Number (Parser dvNumber)
                , liftM Object (Parser dvObject)
                , liftM Array  (Parser dvArray)
                , string "true"  >> return (Bool True)
                , string "false" >> return (Bool False)
                , string "null"  >> return Null
                ]

pStr :: JsonDerivs -> Result JsonDerivs String
Parser pStr = between (char '"') (char '"') $ many c1
    where c1 =  charIf (\c -> not (c=='"' || c=='\\' || isControl c))
            <|> (char '\\' >> c2)
          c2 = msum
               [ char '"'
               , char '\\'
               , char '/'
               , char 'b' >> return '\b'
               , char 'f' >> return '\f'
               , char 'n' >> return '\n'
               , char 'r' >> return '\r'
               , char 't' >> return '\t'
               , char 'u' >> do xs@[x1,x2,x3,x4] <- count 4 hexDigit
                                if x1 `elem` "dD" && x2 `elem` "89abAB"
                                  then surrogatePair x2 x3 x4
                                  else return $ read $ "'\\x"++xs++"'"
               ]
          surrogatePair x2 x3 x4 =
              do char '\\' >> char 'u'
                 [y1, y2, y3, y4] <- count 4 hexDigit
                 if y1 `elem` "dD" && y2 `elem` "cCdDeEfF"
                    then let x2' = fst (head $ readHex [x2]) - 8
                             y2' = fst (head $ readHex [y2]) - 12
                             xlow = fst $ head $ readHex [x3, x4]
                             ylow = fst $ head $ readHex [y3, y4]
                             shifts = [18, 10, 8, 0]
                         in return $ chr $ 0x10000 + foldl1 (.|.) (zipWith shiftL [x2',xlow,y2',ylow] shifts)
                    else fail ""

(>>+) :: Monad m => m [a] -> m [a] -> m [a]
ma >>+ mb = ma >>= \a -> mb >>= \b -> return (a++b)

pNumber :: JsonDerivs -> Result JsonDerivs Double
Parser pNumber = liftM read $ int >>+ option "" frac >>+ option "" exp
    where digits = many1 digit
          int    = do s  <- option "" (string "-")
                      xs <- digits
                      return (s++xs)
          frac   = char '.' >> liftM ('.':) (many1 digit)
          exp    = e >>+ digits
          e      = do a <- char 'e' <|> char 'E'
                      liftM (a:) (string "+" <|> string "-" <|> string "")

pObject :: JsonDerivs -> Result JsonDerivs (M.Map String JsonNode)
Parser pObject = liftM M.fromList $
         between (tok (char '{')) (char '}') $
         tok member `sepBy` tok (char ',')
    where member = do k <- tok (Parser dvStr)
                      tok (char ':')
                      v <- Parser dvNode
                      return (k,v)

pArray :: JsonDerivs -> Result JsonDerivs [JsonNode]
Parser pArray  = between (tok (char '[')) (char ']') $
                 tok (Parser dvNode) `sepBy` tok (char ',')


parse :: String -> JsonNode
parse s = case dvNode (derive (Pos "JSON parser" 1 1) s) of
            Parsed v d' e' -> v
            NoParse e      -> error (show e)
    where derive pos s = d
              where d   = JsonDerivs nod str num obj arr chr pos
                    nod = pValue d
                    str = pStr d
                    num = pNumber d
                    obj = pObject d
                    arr = pArray d
                    chr = case s of
                            (c:s') -> Parsed c (derive (nextPos pos c) s') (nullError d)
                            _      -> NoParse (eofError d)

parse' :: ByteString -> JsonNode
parse' s = case dvNode (derive (Pos "JSON parser" 1 1) s) of
             Parsed v d' e' -> v
             NoParse e      -> error (show e)
    where derive pos s = d
              where d   = JsonDerivs nod str num obj arr chr pos
                    nod = pValue d
                    str = pStr d
                    num = pNumber d
                    obj = pObject d
                    arr = pArray d
                    chr = if BS.null s
                          then NoParse (eofError d)
                          else let (c, s') = (BS.head s, BS.tail s)
                               in Parsed c (derive (nextPos pos c) s') (nullError d)


toDoc :: JsonNode -> Doc
toDoc = stringifyDoc hsep True

toDocPP :: JsonNode -> Doc
toDocPP = stringifyDoc fsep True

pp :: Jsonable a => a -> String
pp = show . toDocPP . toJson

toDocRaw :: JsonNode -> Doc
toDocRaw = stringifyDoc hsep False
toDocPPRaw :: JsonNode -> Doc
toDocPPRaw = stringifyDoc fsep False


stringifyDoc _ f (String s) = stringifyString s f
stringifyDoc _ _ (Number x)
    | isInfinite x = error "can't stringify infinity"
    | isNaN x      = error "can't stringify NaN"
    | otherwise    = double x
stringifyDoc sep f (Object m) = lbrace <+> join comma [sep [stringifyString k f <> colon, nest 2 (stringifyDoc sep f v)] | (k,v) <- M.toList m] $$ rbrace
stringifyDoc sep f (Array xs) = lbrack <+> join comma (map (stringifyDoc sep f) xs) <+> rbrack
stringifyDoc _ _ (Bool b)   = if b then text "true" else text "false"
stringifyDoc _ _ Null       = text "null"

stringifyString :: String -> Bool -> Doc
stringifyString s b = doubleQuotes $ text $ concatMap (f b) s
    where f _ '"'  = "\\\""
          f _ '\\' = "\\\\"
          f _ '\b' = "\\b"
          f _ '\f' = "\\f"
          f _ '\n' = "\\n"
          f _ '\r' = "\\r"
          f _ '\t' = "\\t"
          f _ '\DEL' = "\\u007f"
          f True c = charToUTFChars c
          f _ c | c' > 0xffff = decSurrogates $ ord c
                | c' > 0x00ff = printf "\\u%04x" c
                | c' < 0x0020 = printf "\\u%04x" c
                | otherwise   = [c]
                where c' = ord c
                      decSurrogates c =
                           let high = (c - 0x10000) `shiftR` 10 .|. 0xd800
                               low  = c .&. 0x3ff .|. 0xdc00
                           in "\\u" ++ showHex high ("\\u" ++ showHex low "")
          charToUTFChars c
              | c' < 0x7f      = [c]
              | c' < 0x7ff     = chars 2
              | c' < 0xffff    = chars 3
              | c' < 0x1fffff  = chars 4
              | c' < 0x3ffffff = chars 5
              | otherwise      = chars 6
              where c' = ord c
                    shifts = [30, 24, 18, 12, 6, 0]
                    headMasks = [0xc0, 0xe0, 0xf0, 0xf8, 0xfc]
                    maskShift s = 0x80 .|.  ((c' `shiftR` s) .&. 0x3f)
                    chars n =
                        let (h:t) = map maskShift $ drop (6-n) shifts
                        in map chr ((h .|. (headMasks !! (n-2))) : t)


join :: Doc -> [Doc] -> Doc
join s = fcat . punctuate s


----------------------------------------------------------------------
-- string converter
-- 

toJsonString :: [Word8] -> String
toJsonString s = (encoder s) s

encoder :: [Word8] -> ([Word8] -> String)
encoder (0:0:0:_:_) = utf32BEConv
encoder (_:0:0:0:_) = utf32LEConv
encoder (0:_:_) = utf16BEConv
encoder (_:0:_) = utf16LEConv
encoder _ = utf8Conv

utf8Conv :: [Word8] -> String
utf8Conv [] = []
utf8Conv (c:cs)
    | c < 0xc0 = chr (fromIntegral c) : utf8Conv cs
    | c < 0xe0 = conv 1
    | c < 0xf0 = conv 2
    | c < 0xf8 = conv 3
    | c < 0xfc = conv 4
    | otherwise = conv 5
    where conv n = let (hd, tl) = splitAt n cs
                       c' = fromIntegral c
                   in utf8CharsToChar n c' (map fromIntegral hd) : utf8Conv tl

utf8CharsToChar :: Int -> Int -> [Int] -> Char
utf8CharsToChar n c cs =
    chr $ foldl f ((c .&. mask) `shiftL` hs) $ zip shifts $ map (.&. 0x3f) cs
    where (hs:shifts) = drop (5-n) [30,24,18,12,6,0]
          mask = [0x1f,0x0f, 0x07, 0x03, 0x01] !! n
          f c' (s, c) = c' .|. (c `shiftL` s)

utf16Conv :: (Int -> Int -> Int) -> [Word8] -> String
utf16Conv _ [] = ""
utf16Conv _ [_] = ""
utf16Conv f (c1:c2:cs) =
    let c = f (fromIntegral c1) (fromIntegral c2)
    in if (c .&. 0xd800) == 0xd800
       then surrogatePair c cs
       else chr (c + 0x10000) : utf16Conv f cs
    where surrogatePair c (c1:c2:cs) =
              if (c' .&. 0xdc00) == 0xdc00
              then chr (high .|. low) : utf16Conv f cs
              else error "invalid UTF-16 encoding"
              where c' = f (fromIntegral c1) (fromIntegral c2)
                    high = (c .&. 0x03ff) `shiftL` 10
                    low  = c' .&. 0x03ff
          surrogatePair c _ = error "invalid UTF-16 encoding"

          nextChar (c1:c2:cs) =
              (f (fromIntegral c1) (fromIntegral c2), cs)

utf16LEConv :: [Word8] -> String
utf16LEConv = utf16Conv f
    where f c1 c2 = c2 `shiftL` 8 .|. c1

utf16BEConv :: [Word8] -> String
utf16BEConv = utf16Conv f
    where f c1 c2 = c1 `shiftL` 8 .|. c2

utf32Conv :: [Int] -> [Word8] -> String
utf32Conv shifts (c1:c2:c3:c4:cs) =
    chr (f $ map fromIntegral [c1, c2, c3, c4]) : utf32Conv shifts cs
    where f cs = foldl1 (.|.) $ zipWith (\c s -> shiftL c s) cs shifts
utf32Conv _ _ = ""

utf32BEConv :: [Word8] -> String
utf32BEConv = utf32Conv [24,16,8,0]

utf32LEConv :: [Word8] -> String
utf32LEConv = utf32Conv [0,8,16,24]
