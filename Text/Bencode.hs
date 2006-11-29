{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -fallow-incoherent-instances #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Bencode
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  experimental
-- Portability :  GHC-only
-- 
-- Bencode Parser and Serializer and type classes, for BitTorrent.
-- 

module Text.Bencode
    ( -- * Basic Type and Type Class
      Bencodable(..), BencodeNode(..)
    , parse, parses, encode
    )
where

import Data.Char (isDigit)
import Data.Map (Map, toList, fromList)
import qualified Data.Map as M (empty, insert, map, mapKeys, singleton)
import Data.Word
import Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Writer

class Bencodable a where
    fromBencode :: BencodeNode -> a
    toBencode :: a -> BencodeNode
    bRead :: String -> a
    bShow :: a -> String
    bRead = fromBencode . parse . pack
    bShow = unpack . encode . toBencode

instance Bencodable BencodeNode where
    fromBencode = id
    toBencode = id

instance Bencodable String where
    fromBencode (String s) = unpack s
    fromBencode _          = error "type mismatch"
    toBencode = String . pack

instance Bencodable ByteString where
    fromBencode (String s) = s
    fromBencode _          = error "type mismatch"
    toBencode = String

instance Bencodable Integer where
    fromBencode (Number n) = n
    fromBencode _          = error "type mismatch"
    toBencode = Number

instance Bencodable Int where
    fromBencode (Number n) = fromInteger n
    fromBencode _          = error "type mismatch"
    toBencode = Number . toInteger

instance (Bencodable a) => Bencodable (Map ByteString a) where
    fromBencode (Dictionary m) = M.map fromBencode m
    fromBencode _              = error "type mismatch"
    toBencode = Dictionary . M.map toBencode

instance (Bencodable a) => Bencodable [(ByteString, a)] where
    fromBencode (Dictionary m) = toList $ M.map fromBencode m
    fromBencode _              = error "type mismatch"
    toBencode = Dictionary . M.map toBencode . fromList

instance (Bencodable a) => Bencodable (Map String a) where
    fromBencode (Dictionary m) = M.mapKeys unpack $ M.map fromBencode m
    fromBencode _              = error "type mismatch"
    toBencode = Dictionary . M.map toBencode . M.mapKeys pack

instance (Bencodable a) => Bencodable [(String, a)] where
    fromBencode (Dictionary m) = toList $ M.mapKeys unpack $ M.map fromBencode m
    fromBencode _              = error "type mismatch"
    toBencode = Dictionary . M.mapKeys pack . M.map toBencode . fromList

instance (Bencodable a) => Bencodable [a] where
    fromBencode (List l) = map fromBencode l
    fromBencode _        = error "type mismatch"
    toBencode = List . map toBencode


data BencodeNode = String !ByteString 
                 | Number !Integer
                 | Dictionary !(Map ByteString BencodeNode)
                 | List [BencodeNode]
                   deriving (Eq, Show)

stringP :: ByteString -> Writer [BencodeNode] ByteString
stringP s = if BS.head rest == ':' 
            then Writer (s', [String v]) else error "parse error for stringP"
    where (lenStr, rest) = BS.span isDigit s
          len = read $ unpack lenStr
          rest' = BS.tail rest
          (v, s') = BS.splitAt len rest'

numberP :: ByteString -> Writer [BencodeNode] ByteString
numberP s = Writer (BS.tail rest, [Number $ read $ unpack v])
    where (v, rest) = BS.break (=='e') s
          t = BS.head rest

headIsE = (=='e') . BS.head
untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM c p s = if c s then return s else p s >>= untilM c p

listP :: ByteString -> Writer [BencodeNode] ByteString
listP s = fmap BS.tail $ censor f $ listP' s
    where listP' s = untilM headIsE bencodeP s
          f l = [List l]

censor' :: (Monoid w, Monoid w') => (w -> w') -> Writer w a -> Writer w' a
censor' f (Writer (a, w)) = Writer (a, f w)

dictP :: ByteString -> Writer [BencodeNode] ByteString
dictP s = fmap BS.tail $ censor' f $ dictP' s
    where dictP' s = 
              untilM headIsE (\s -> censor' l2n (stringP s >>= bencodeP)) s
          l2n [String k,v] = M.singleton k v
          f m = [Dictionary m]

bencodeP :: ByteString -> Writer [BencodeNode] ByteString
bencodeP s | isDigit h = stringP s
           | h == 'i'  = numberP s'
           | h == 'l'  = listP s'
           | h == 'd'  = dictP s'
           | otherwise = error ("unidentified type specifier '"++[h]++"'")
    where s' = BS.tail s
          h = BS.head s

parse :: ByteString -> BencodeNode
parse = head . execWriter . bencodeP

parses :: ByteString -> [BencodeNode]
parses s = execWriter (parse' s)
    where parse' s = untilM BS.null bencodeP s

encode :: BencodeNode -> ByteString
encode (String s) = BS.concat [pack $ show $ BS.length s, colon, s]
encode (Number n) = BS.concat [i, pack $ show n, e]
encode (List lst) =  BS.concat (l : (map encode lst) ++ [e])
encode (Dictionary dic) =  BS.concat (d : concatMap f (toList dic) ++ [e])
    where f (k, v) = [encode $ String k, encode v]
colon = BS.singleton ':'
i = BS.singleton 'i'
l = BS.singleton 'l'
d = BS.singleton 'd'
e = BS.singleton 'e'
