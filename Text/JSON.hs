{-# OPTIONS -fglasgow-exts -fallow-incoherent-instances #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.JSON
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  experimental
-- Portability :  portable
-- 
-- A JSON Parser and Serializer and type classes.
-- This code is originally written by Masahiro Sakai. 
-- The original code can be seen at http://www.tom.sfc.keio.ac.jp/~sakai/d/?date=20060427#p02 
-- 

module Text.JSON
    ( Jsonable(..)
    , JsonNode(..)
    , parse, parse', toDoc, toDocPP, pp)
where

import Control.Monad hiding (join)
import Text.Packrat.Parse
import Text.Packrat.Pos
import Text.Printf (printf)
import Data.Char (ord, chr, isControl)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Numeric (readHex, showHex)
import Text.PrettyPrint.HughesPJ hiding (char)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as BS

class JsonTypable a where

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

instance Jsonable Double where
    fromJson (Number n) = n
    fromJson _          = error "type mismatch"
    toJson = Number

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
    where digits = many digit
          int    = do s  <- option "" (string "-")
                      x  <- oneOf ['1'..'9']
                      xs <- digits
                      return (s++x:xs)
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
toDoc = stringifyDoc hsep

toDocPP :: Jsonable a => a -> Doc
toDocPP = stringifyDoc fsep . toJson

pp :: Jsonable a => a -> String
pp = show . toDocPP

stringifyDoc _ (String s) = stringifyString s
stringifyDoc _ (Number x)
    | isInfinite x = error "can't stringify infinity"
    | isNaN x      = error "can't stringify NaN"
    | otherwise    = double x
stringifyDoc sep (Object m) = lbrace <+> join comma [sep [stringifyString k <> colon, nest 2 (stringifyDoc sep v)] | (k,v) <- M.toList m] $$ rbrace
stringifyDoc sep (Array xs) = lbrack <+> join comma (map (stringifyDoc sep) xs) <+> rbrack
stringifyDoc _ (Bool b)   = if b then text "true" else text "false"
stringifyDoc _ Null       = text "null"

stringifyString :: String -> Doc
stringifyString s = doubleQuotes $ text $ concatMap f s
    where f '"'  = "\\\""
          f '\\' = "\\\\"
          f '\b' = "\\b"
          f '\f' = "\\f"
          f '\n' = "\\n"
          f '\r' = "\\r"
          f '\t' = "\\t"
          f c | ord c > 0xffff = decSurrogates $ ord c
              | isControl c    = printf "\\u%04x" c
              | otherwise      = [c]
          decSurrogates c =
              let high = (c - 0x10000) `shiftR` 10 .|. 0xd800
                  low  = c .&. 0x3ff .|. 0xdc00
              in "\\u" ++ showHex high ("\\u" ++ showHex low "")

join :: Doc -> [Doc] -> Doc
join s = fcat . punctuate s
