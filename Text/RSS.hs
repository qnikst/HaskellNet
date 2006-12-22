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
    ( RSSItem(..), RSSChannel(..), RSSImage(..), RSSITEM(..), DublinCore(..)
    , rdfAttrs, renderRSS, renderRSSFull
    , xmlHeader
    )
where

import Text.PrettyPrint.HughesPJ
import Data.Time.LocalTime (LocalTime, ZonedTime(..), TimeZone(..), formatTime)
import System.Locale (defaultTimeLocale)

data RSSItem = RSSItem String String (Maybe String) (Maybe String) [DublinCore]

data RSSChannel = RSSChannel { chTitle :: String
                             , chLink :: String
                             , chDescription :: String
                             , chDC ::  [DublinCore]
                             , chImage :: Maybe RSSImage
                             }
data RSSImage = RSSImage { imURI :: String
                         , imTitle :: String
                         , imLink :: String
                         } 

data DublinCore = DCCreator String 
                | DCDate ZonedTime
                | DCSubject String

class RSSITEM item where
    title :: item -> String
    link :: item -> String
    description :: item -> Maybe String
    content :: item -> Maybe String
    dc :: item -> [DublinCore]
    description = const Nothing
    content = const Nothing

instance RSSITEM RSSItem where
    title (RSSItem t _ _ _ _) = t
    link (RSSItem _ l _ _ _)  = l
    description (RSSItem _ _ d _ _) = d
    content (RSSItem _ _ _ c _) = c
    dc (RSSItem _ _ _ _ d) = d


rdfAttrs = [ ("xmlns", "http://purl.org/rss/1.0/")
           , ("xmlns:rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
           , ("xmlns:dc", "http://purl.org/dc/elements/1.1/")
           , ("xmlns:content", "http://purl.org/rss/1.0/modules/content/")
           ] 

renderRSS :: RSSITEM item => RSSChannel -> [item] -> String
renderRSS = renderRSSFull xmlHeader rdfAttrs
renderRSSFull :: RSSITEM item => 
                 String -> [(String, String)] -> RSSChannel -> [item] -> String
renderRSSFull hdr attrs ch items =
    show (text hdr $+$ tagDoc "rdf:RDF" attrs (channel:image:itemDocs))
    where channel = channelToDoc ch items
          image = maybeDoc imageToDoc (chImage ch)
          itemDocs = map rssItemToDoc items

channelToDoc :: RSSITEM item => RSSChannel -> [item] -> Doc
channelToDoc (RSSChannel cht chl chdesc chdcs _) items = 
    tagDoc "channel" [("rdf:about", chl)]
               ([ textTag "title" cht
                , textTag "link" chl
                , textTag "description" chdesc
                ] ++ map dcToDoc chdcs ++ [item])
    where item = tagDoc "items" []
                    [tagDoc "rdf:Seq" [] (map rssItemToResource items)]

imageToDoc :: RSSImage -> Doc
imageToDoc (RSSImage imuri imt iml) = 
    tagDoc "image" [("rdf:resource", imuri)]
        [textTag "title" imt, textTag "link" iml, textTag "url" imuri]

rssItemToResource :: RSSITEM item => item -> Doc
rssItemToResource item = tagDoc' "rdf:li" [("rdf:resource", link item)]
rssItemToDoc :: RSSITEM item => item -> Doc
rssItemToDoc item =
    tagDoc "item" [("rdf:about", l)] ([ textTag "title" t
                                      , textTag "link" l
                                      , maybeDoc (textTag "description") desc
                                      , maybeDoc contentEncoded cont
                                      ] ++ map dcToDoc dcs)
        where t = title item
              l = link item
              desc = description item
              cont = content item
              dcs = dc item
              contentEncoded v = tagDoc "content:encoded" [] $ 
                                 [text "<![CDATA[" <> text v <> text "]]>"]
 
dcToDoc :: DublinCore -> Doc
dcToDoc (DCCreator cr) = textTag "dc:creator" cr
dcToDoc (DCSubject sub) = textTag "dc:subject" sub
dcToDoc (DCDate (ZonedTime lt (TimeZone mins _ _))) = 
    textTag "dc:date" (lts++zs)
    where lts = formatTime defaultTimeLocale "%FT%X" lt
          zs = let (h, m) = divMod mins 60 in 
               '+' : (show2 h ++ (':' : show2 m))
          show2 n | n < 10    = '0' : show n
                  | otherwise = show n

textTag :: String -> String -> Doc
textTag tag cont = hcat [char '<', text tag, char '>'
                        , text cont, text "</", text tag, char '>']

tagDoc :: String -> [(String, String)] -> [Doc] -> Doc
tagDoc tag attrs children = fcat [hang (((text $ '<':tag) $$ nest (length tag + 2) (fsep $ map attrDoc attrs)) <> char '>') 2 (fcat children), text ("</"++tag++">")]

tagDoc' :: String -> [(String, String)] -> Doc
tagDoc' tag attrs = fsep [text ('<':tag), nest (length tag + 2) (fsep $ map attrDoc attrs), text "/>"]

attrDoc (name, value) = text name <> char '=' <> doubleQuotes (text value)

maybeDoc :: (a -> Doc) -> Maybe a -> Doc
maybeDoc f m = maybe empty f m

xmlHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
