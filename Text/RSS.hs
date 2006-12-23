{-# OPTIONS -fglasgow-exts #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.RSS
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  experimental
-- Portability :  portable
-- 
-- RSS 1.0 Printer Library (printing such like Text.HTML)
-- 

module Text.RSS
    ( RSSItem(..), RSSChannel(..), RSSImage(..), DublinCore(..)
    , ATitle(..), ALink(..), ADescription(..), AContent(..), ADC(..)
    , AImage(..), AItems(..), AUri(..)
    , RSSITEM(..), RSSCHANNEL(..), RSSIMAGE(..)
    , rss2Elem, rss2ElemWithAttrs, defaultAttrs, elem2rss
    , Attr, get, set, update, has, add, howMany, set', get', delete
    )
where

import Data.Record
import Data.List (isPrefixOf)
import Data.Maybe
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime
import System.Locale (defaultTimeLocale)
import Text.XML.HaXml
import Text.XML.HaXml.Xml2Haskell

data RSSItem = RSSItem { itemTitle :: String 
                       , itemLink :: String
                       , itemDescription :: Maybe String
                       , itemContent :: Maybe String
                       , itemDC :: [DublinCore]
                       }

data RSSChannel = RSSChannel { chTitle :: String
                             , chLink :: String
                             , chDescription :: String
                             , chDC ::  [DublinCore]
                             , chImage :: Maybe RSSImage
                             , chItems :: [RSSItem]
                             }
data RSSImage = RSSImage { imURI :: String
                         , imTitle :: String
                         , imLink :: String
                         } 

data DublinCore = DCCreator String 
                | DCDate ZonedTime
                | DCSubject String

class ATitle r v | r -> v where title :: Attr r v
class ALink r v | r -> v where link :: Attr r v
class ADescription r v | r -> v where description :: Attr r v
class AContent r v | r -> v where content :: Attr r v
class ADC r v | r -> v where dc :: Attr r v
class AImage r v | r -> v where image :: Attr r v
class AItems r v | r -> v where items :: Attr r v
class AUri r v | r -> v where uri :: Attr r v

class ( ATitle r String, ALink r String, ADescription r (Maybe String)
      , AContent r (Maybe String), ADC r [DublinCore]) 
    => RSSITEM r
class ( ATitle r String, ALink r String, ADescription r String
      , ADC r [DublinCore], AImage r (Maybe RSSImage), AItems r [RSSItem])
    => RSSCHANNEL r
class (AUri r String, ATitle r String, ALink r String) => RSSIMAGE r

instance ATitle RSSItem String where
    title = Attr itemTitle setter
        where setter r v = r { itemTitle = v }
instance ALink RSSItem String where
    link = Attr itemLink setter
        where setter r v = r { itemLink = v }
instance ADescription RSSItem (Maybe String) where
    description = Attr itemDescription setter
        where setter r v = r { itemDescription = v }
instance AContent RSSItem (Maybe String) where
    content = Attr itemContent setter
        where setter r v = r { itemContent = v }
instance ADC RSSItem [DublinCore] where
    dc = Attr itemDC setter
        where setter r v = r { itemDC = v }
instance RSSITEM RSSItem

instance ATitle RSSChannel String where
    title = Attr chTitle setter
        where setter r v = r { chTitle = v }
instance ALink RSSChannel String where
    link = Attr chLink setter
        where setter r v = r { chLink = v }
instance ADescription RSSChannel String where
    description = Attr chDescription setter
        where setter r v = r { chDescription = v }
instance ADC RSSChannel [DublinCore] where
    dc = Attr chDC setter
        where setter r v = r { chDC = v }
instance AImage RSSChannel (Maybe RSSImage) where
    image = Attr chImage setter
        where setter r v = r { chImage = v }
instance AItems RSSChannel [RSSItem] where
    items = Attr chItems setter
        where setter r v = r { chItems = v }
instance RSSCHANNEL RSSChannel

instance AUri RSSImage String where
    uri = Attr imURI setter
        where setter r v = r { imURI = v }
instance ATitle RSSImage String where
    title = Attr imTitle setter
        where setter r v = r { imTitle = v }
instance ALink RSSImage String where
    link = Attr imLink setter
        where setter r v = r { imLink = v }

fromText' :: String -> [Content] -> [String]
fromText' t = map (concat . fst . many fromText) . dropWhile null . map (tag t /> keep)
toText' :: String -> String -> Content
toText' tag cont = CElem $ Elem tag [] [CString False cont]

instance XmlContent RSSImage where
    fromElem (CElem (Elem "image" ats cs) : rest) =
        (Just $ RSSImage u' t l, rest)
        where t = concat $ fromText' "title" cs
              l = concat $ fromText' "link" cs
              u' = concat $ fromText' "url" cs
    fromElem rest = (Nothing, rest)
    toElem (RSSImage u t l) = [CElem $ Elem "image" [about] $ zipWith toText' ["title", "link", "url"] [u, t, l]]
        where about = ("rdf:resource", str2attr u)

instance XmlContent DublinCore where
    fromElem (CElem (Elem "dc:creator" _ cs) : rest) =
        (Just $ DCCreator (concat $ fst $ many fromText cs), rest)
    fromElem (CElem (Elem "dc:subject" _ cs) : rest) =
        (Just $ DCSubject (concat $ fst $ many fromText cs), rest)
    fromElem (CElem (Elem "dc:date" _ cs) : rest) =
        (Just $ DCDate $ ZonedTime (LocalTime (fromGregorian (read year) (read mon) (read day)) (TimeOfDay (read hour) (read min) (fromRational $ toRational (read sec::Double)))) zone, rest)
        where s = concat $ fst $ many fromText cs
              (d, _:h') = break (=='T') s
              (h, z) = break (`elem` "+-Z") h'
              (year:mon:day:_) = breaks '-' d
              (hour:min:sec:_) = breaks ':' h
              zone = case z of
                       "Z" -> utc
                       zs@(_:_) -> 
                           let (zh:zm:_) = map read $ breaks ':' zs
                           in minutesToTimeZone ((abs zh * 60 + zm) * signum zh)
                       _ -> utc
              breaks c d = case break (==c) d of
                             (s, "")   -> [s]
                             (s, rest) -> s : breaks c rest
    fromElem rest = (Nothing, rest)
    toElem (DCCreator c) = [toText' "dc:creator" c]
    toElem (DCSubject s) = [toText' "dc:subject" s]
    toElem (DCDate (ZonedTime d (TimeZone diffs _ _))) = [toText' "dc:date" dateStr]
      where dateStr = formatTime defaultTimeLocale "%FT%T" d ++ zs
            zs = if diffs == 0 then "Z"
                 else let (zh,zm) = diffs `divMod` 60
                          (zh',zm') = if diffs < 0 && zm /= 0 
                                      then (zh+1,60-zm) else (zh,zm)
                          sig = if zh' < 0 then '-' else '+'
                      in sig : show2 zh' ++ ":" ++ show2 zm'
            show2 n | abs n < 10 = '0':show (abs n)
                    | otherwise  = show n

instance XmlContent RSSItem where
    fromElem (CElem (Elem "item" ats cs) : rest) =
        (Just $ RSSItem t l d c dcs, rest)
        where t = concat $ fromText' "title" cs
              l = concat $ fromText' "link" cs
              d = listToMaybe $ fromText' "description" cs
              c = listToMaybe $ fromText' "content:encoded" cs
              dcs = fst $ many fromElem $ concatMap (tagWith (isPrefixOf "dc:")) cs
    fromElem rest = (Nothing, rest)
    toElem (RSSItem t l d c dcs) = [CElem $ Elem "item" [("rdf:about", str2attr l)] ([tt, lt] ++ catMaybes [dt, ct] ++ dct)]
        where tt = toText' "title" t
              lt = toText' "link" l
              dt = d >>= return . toText' "description"
              ct = c >>= return . toText' "content:encoded"
              dct = concatMap toElem dcs

instance XmlContent RSSChannel where
    fromElem (CElem (Elem "channel" ats cs) : rest) =
        (Just $ RSSChannel t l d dcs Nothing [], rest)
        where t = concat $ fromText' "title" cs
              l = concat $ fromText' "link" cs
              d = concat $ fromText' "description" cs
              dcs = fst $ many fromElem $ concatMap (tagWith (isPrefixOf "dc:")) cs
    fromElem rest = (Nothing, rest)
    toElem (RSSChannel t l d dcs im is) = [CElem $ Elem "channel" [("rdf:about", str2attr l)] ([tt, lt, dt, ist]++imt++dct)]
        where tt = toText' "title" t
              lt = toText' "link" l
              dt = toText' "description" d
              imt = map (\im' -> CElem $ Elem "image" [("rdf:resource", str2attr $ get link im')] []) $ maybeToList im
              ist = CElem $ Elem "items" [] 
                    [CElem $ Elem "rdf:Seq" [] (map (l2li . get link) is)]
              dct = concatMap toElem dcs
              l2li l = CElem $ Elem "rdf:li" [("rdf:resource", str2attr l)] []


rss2Elem :: RSSChannel -> Element
rss2Elem = rss2ElemWithAttrs defaultAttrs
rss2ElemWithAttrs :: [(String, String)] -> RSSChannel -> Element
rss2ElemWithAttrs attrs ch = 
    Elem "rdf:RDF" attrs' (toElem img++concatMap toElem es++toElem img)
    where img = get image ch
          es  = get items ch
          attrs' = map (\(f, v) -> (f, str2attr v)) attrs

elem2rss :: Element -> RSSChannel
elem2rss (Elem "rdf:RDF" _ cs) = ch { chItems = is, chImage = img }
    where (Just ch, cs') = fromElem cs
          is = fst $ many fromElem $ concatMap (tag "item") cs'
          img = fst $ fromElem $ concatMap (tag "image") cs'

defaultAttrs = [ ("xmlns", "http://purl.org/rss/1.0/")
               , ("xmlns:rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
               , ("xmlns:dc", "http://purl.org/dc/elements/1.1/")
               , ("xmlns:content", "http://purl.org/rss/1.0/modules/content/")
               ] 
