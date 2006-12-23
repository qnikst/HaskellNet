{-# OPTIONS_GHC -fglasgow-exts #-}
----------------------------------------------------------------------
-- |
-- Module      :  Text.Atom
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Atom Syndication format Parser/Printer Library using HaXml
-- 

module Text.Atom
    ( Text(..), EntryContent(..), stringContent, xhtmlContent, binaryContent, srcContent
    , Person, mkPerson
    , Feed, mkFeed, Entry, mkEntry
    , Category, mkCategory, Generator, mkGenerator, defaultGenerator
    , Link, LinkRelation(..), mkLink, selfLink
    , Attr, get, set, update, has, add, howMany, set', get', delete
    , AName(..), AUri(..), AEmail(..), AAuthor(..), ACategory(..)
    , AContributor(..), AGenerator(..), AIcon(..), AId(..), ALink(..)
    , ALogo(..), ARights(..), ASubtitle(..), ATitle(..), AUpdated(..)
    , AEntries(..), AContent(..), APublished(..), ASource(..), ASummary(..)
    , ATerm(..), AScheme(..), ALabel(..), AVersion(..), AHref(..), ARel(..)
    , AMediatype(..), AHreflang(..), ALength(..)
    , PERSON(..), CATEGORY(..), GENERATOR(..), LINK(..), FEED(..), ENTRY(..)
    )
where

import Data.List (genericLength, nubBy, isPrefixOf)
import Data.Char (isSpace)
import Data.Maybe
import Control.Applicative hiding (many)
import Data.Foldable
import Data.Map (findWithDefault, insert, Map)
import qualified Data.Map as M
import Data.Monoid (Sum(..))
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), ZonedTime(..), TimeZone(..), formatTime, minutesToTimeZone, utc)
import System.Locale (defaultTimeLocale)
import Data.Time.Calendar
import Text.XML.HaXml hiding (version)
import Text.XML.HaXml.Escape (xmlEscape, xmlUnEscape, mkXmlEscaper)
import qualified Text.XML.HaXml.Html.Pretty as P (content)
import Text.XML.HaXml.Xml2Haskell
import Text.PrettyPrint.HughesPJ (hcat)
import qualified Codec.Binary.Base64 as B64 (encode, decode)

import Prelude hiding (foldr1, foldr, or, any, all, concatMap, concat, elem)

e2e :: (Enum a, Enum b) => a -> b
e2e = toEnum . fromEnum
b64Encode :: String -> String
b64Encode = map e2e . B64.encode . map e2e
b64Decode :: String -> String
b64Decode = map e2e . B64.decode . map e2e

atomEscaper = mkXmlEscaper [('<', "lt"), ('>', "gt"), ('&', "amp"), ('"', "quot")] (`elem` "<>\"")
xEscape :: [Content] -> [Content]
xEscape cs = let Elem _ _ cs' = xmlEscape atomEscaper (Elem "" [] cs) in cs'

xUnEscape :: [Content] -> [Content]
xUnEscape cs = let Elem _ _ cs' = xmlUnEscape atomEscaper (Elem "" [] cs) in cs'

escapeString :: String -> String
escapeString s = fromText' $ xEscape [CString False s]

unEscapeString :: String -> String
unEscapeString s = fromText' $ xUnEscape [CString False s]

------------------------------------------------------------
-- basic constructs 

data Text = Text String
          | HTML String
          | XHtml [Content]
instance Show Text where
    show (Text s) = "Text "++show s
    show (HTML s) = "HTML "++show s
    show (XHtml cont) = "XHtml \""++show (hcat (map P.content cont))++"\""

data EntryContent = TextCont Text
                  | BinaryData String String
                  | OtherSrc String String
                    deriving Show
stringContent = TextCont . Text
xhtmlContent = TextCont . XHtml
binaryContent t d | typeChecker t = error "binary data must not text, html, or xhtml"
                  | otherwise = BinaryData t d
srcContent t url | typeChecker t = error "other src must not text, html, or xhtml"
                 | otherwise = OtherSrc t url
typeChecker t = any (flip isPrefixOf t) ["text", "html", "xhtml"]


data Person = Person { personName :: !Text
                     , personUri :: Maybe String
                     , personEmail :: Maybe String
                     } deriving Show
mkPerson :: String -> Person
mkPerson s = Person (Text s) Nothing Nothing

------------------------------------------------------------
-- atom types
data Feed = Feed { feedAuthor :: [Person]
                 , feedCategory :: [Category]
                 , feedContributor :: [Person]
                 , feedGenerator :: [Generator]
                 , feedIcon :: Maybe String
                 , feedId :: !String
                 , feedLink :: [Link]
                 , feedLogo :: Maybe String
                 , feedRights :: Maybe Text
                 , feedSubtitle :: Maybe Text
                 , feedTitle :: !Text
                 , feedUpdated :: !ZonedTime
                 , feedEntries :: [Entry]
                 } deriving Show
mkFeed :: Person -- ^ primary author
       -> String -- ^ id
       -> String -- ^ self link
       -> String -- ^ title
       -> ZonedTime -> Feed
mkFeed a id l t ut = Feed [a] [] [] [defaultGenerator] Nothing id [selfLink l] Nothing Nothing Nothing (Text t) ut []

data Entry = Entry { entryAuthor :: [Person]
                   , entryCategory :: [Category]
                   , entryContent :: Maybe EntryContent
                   , entryContributor :: [Person]
                   , entryId :: !String
                   , entryLink :: [Link]
                   , entryPublished :: Maybe ZonedTime
                   , entryRights :: Maybe Text
                   , entrySource :: Maybe Feed
                   , entrySummary :: Maybe String
                   , entryTitle :: !Text
                   , entryUpdated :: !ZonedTime
                   } deriving Show
mkEntry :: String -- ^ id
        -> String -- ^ title
        -> EntryContent -- ^ content
        -> ZonedTime -> Entry
mkEntry id title content updated = Entry [] [] (Just content) [] id [] Nothing Nothing Nothing Nothing (Text title) updated

data Category = Category { categoryTerm :: !String
                         , categoryScheme :: Maybe String
                         , categoryLabel :: Maybe String
                         } deriving Show
mkCategory :: String -> Category
mkCategory s = Category s Nothing Nothing

data Generator = Generator { generatorName :: !String
                           , generatorUri :: Maybe String
                           , generatorVersion :: Maybe String
                           } deriving Show
mkGenerator :: String -> Generator
mkGenerator s = Generator s Nothing Nothing
defaultGenerator :: Generator
defaultGenerator = Generator "HaskellNet" (Just "http://darcs.haskell.org/SoC/haskellnet/") (Just "0.1")

data Link = Link { linkHref :: !String
                 , linkRel :: !LinkRelation
                 , linkMediatype :: Maybe String
                 , linkHreflang :: Maybe String
                 , linkTitle :: Maybe String
                 , linkLength :: Maybe Integer
                 } deriving Show
data LinkRelation = Alternate | Related | Self | Enclosure | Via deriving Eq
instance Show LinkRelation where
    show Alternate = "alternate"
    show Related = "related"
    show Self = "self"
    show Enclosure = "enclosure"
    show Via = "via"
instance Read LinkRelation where
    readsPrec d s = readParen (d > app_prec) (\s -> concatMap (f s) labels) s
        where app_prec = 10
              f s (s', l) | s' `isPrefixOf` s = [(l, drop (length s') s)]
                          | otherwise         = []
              labels = [ ("alternate", Alternate), ("related", Related)
                       , ("self", Self), ("enclosure", Enclosure)
                       , ("via", Via) ]
mkLink :: String -> Link
mkLink s = Link s Alternate Nothing Nothing Nothing Nothing
selfLink :: String -> Link
selfLink s = Link s Self Nothing Nothing Nothing Nothing

------------------------------------------------------------
-- record accessors

data Attr r v = Attr { getter :: r -> v
                     , setter :: r -> v -> r
                     }

get :: Attr r v -> r -> v
get = getter
set :: Attr r v -> v -> r -> r
set a v r = setter a r v
update :: Attr r v -> (v -> v) -> r -> r
update a f r = set a (f $ get a r) r

has :: (Foldable t) => Attr r (t v) -> r -> Bool
has a r = howMany a r > 0

add :: (Alternative t) => Attr r (t v) -> v -> r -> r
add a v r = update a (pure v <|>) r

howMany :: (Foldable t, Integral n) => Attr r (t v) -> r -> n
howMany a r = getSum $ foldMap (const (Sum 1)) $ get a r

set' :: (Applicative t, Foldable t) => Attr r (t v) -> v -> r -> r
set' a v r | has a r   = r
           | otherwise = set a (pure v) r
get' :: (Foldable t) => Attr r (t v) -> r -> v
get' a r = foldr1 const $ get a r

delete :: (Alternative t) => Attr r (t v) -> r -> r
delete a r = set a empty r

------------------------------------------------------------
-- field class/instance declarations

class AName r v | r -> v where    name :: Attr r v
class AUri r v | r -> v where    uri :: Attr r v
class AEmail r v | r -> v where    email :: Attr r v
class AAuthor r v | r -> v where    author :: Attr r v
class ACategory r v | r -> v where    category :: Attr r v
class AContributor r v | r -> v where    contributor :: Attr r v
class AGenerator r v | r -> v where    generator :: Attr r v
class AIcon r v | r -> v where    icon :: Attr r v
class AId r v | r -> v where    identifier :: Attr r v
class ALink r v | r -> v where    link :: Attr r v
class ALogo r v | r -> v where    logo :: Attr r v
class ARights r v | r -> v where    rights :: Attr r v
class ASubtitle r v | r -> v where    subtitle :: Attr r v
class ATitle r v | r -> v where    title :: Attr r v
class AUpdated r v | r -> v where    updated :: Attr r v
class AEntries r v | r -> v where    entries :: Attr r v
class AContent r v | r -> v where    content :: Attr r v
class APublished r v | r -> v where    published :: Attr r v
class ASource r v | r -> v where    source :: Attr r v
class ASummary r v | r -> v where    summary :: Attr r v
class ATerm r v | r -> v where    term :: Attr r v
class AScheme r v | r -> v where    scheme :: Attr r v
class ALabel r v | r -> v where    label :: Attr r v
class AVersion r v | r -> v where    version :: Attr r v
class AHref r v | r -> v where    href :: Attr r v
class ARel r v | r -> v where    rel :: Attr r v
class AMediatype r v | r -> v where    mediatype :: Attr r v
class AHreflang r v | r -> v where    hreflang :: Attr r v
class ALength r v | r -> v where    len :: Attr r v


instance AName Person Text where
    name = Attr personName setter
        where setter r v = r { personName = v }

instance AUri Person (Maybe String) where
    uri = Attr personUri setter
        where setter r v = r { personUri = v }

instance AEmail Person (Maybe String) where
    email = Attr personEmail setter
        where setter r v = r { personEmail = v }

instance AAuthor Feed [Person] where
    author = Attr feedAuthor setter
        where setter r v | check r v = r { feedAuthor = v }
                         | otherwise = error "Feed must have at least one author"
              check r [] = not $ any (null . get author) $ feedEntries r
              check r vs = True

instance ACategory Feed [Category] where
    category = Attr feedCategory setter
        where setter r v = r { feedCategory = v }

instance AContributor Feed [Person] where
    contributor = Attr feedContributor setter
        where setter r v = r { feedContributor = v }

instance AGenerator Feed [Generator] where
    generator = Attr feedGenerator setter
        where setter r v = r { feedGenerator = v }

instance AIcon Feed (Maybe String) where
    icon = Attr feedIcon setter
        where setter r v = r { feedIcon = v }

instance AId Feed String where
    identifier = Attr feedId setter
        where setter r v = r { feedId = v }

instance ALink Feed [Link] where
    link = Attr feedLink setter
        where setter r v | check v   = r { feedLink = v }
                         | otherwise = error "links do not satisfy the specs of atom"
              check ls = let ls'  = filter ((==Alternate) . get rel) ls
                             ls'' = filter ((==Self) . get rel) ls
                         in length (nubBy f1 ls') == length ls' && 
                            not (null ls'')
              f1 a b = get mediatype a == get mediatype b && 
                       get hreflang a == get hreflang b
                        
instance ALogo Feed (Maybe String) where
    logo = Attr feedLogo setter
        where setter r v = r { feedLogo = v }

instance ARights Feed (Maybe Text) where
    rights = Attr feedRights setter
        where setter r v = r { feedRights = v }

instance ASubtitle Feed (Maybe Text) where
    subtitle = Attr feedSubtitle setter
        where setter r v = r { feedSubtitle = v }

instance ATitle Feed Text where
    title = Attr feedTitle setter
        where setter r v = r { feedTitle = v }

instance AUpdated Feed ZonedTime where
    updated = Attr feedUpdated setter
        where setter r v = r { feedUpdated = v }

instance AEntries Feed [Entry] where
    entries = Attr feedEntries setter
        where setter r v = r { feedEntries = v }
              

instance AAuthor Entry [Person] where
    author = Attr entryAuthor setter
        where setter r v = r { entryAuthor = v }

instance ACategory Entry [Category] where
    category = Attr entryCategory setter
        where setter r v = r { entryCategory = v }

instance AContent Entry (Maybe EntryContent) where
    content = Attr entryContent setter
        where setter r v = r { entryContent = v }

instance AContributor Entry [Person] where
    contributor = Attr entryContributor setter
        where setter r v = r { entryContributor = v }

instance AId Entry String where
    identifier = Attr entryId setter
        where setter r v = r { entryId = v }

instance ALink Entry [Link] where
    link = Attr entryLink setter
        where setter r v | check r v = r { entryLink = v }
                         | otherwise = error "links do not satisfy the specs of atom"
              check r ls = 
                  let ls'  = filter ((==Alternate) . get rel) ls
                  in length (nubBy f1 ls') == length ls' && 
                         (isJust (entryContent r) || not (null ls'))
              f1 a b = get mediatype a == get mediatype b && 
                       get hreflang a == get hreflang b


instance APublished Entry (Maybe ZonedTime) where
    published = Attr entryPublished setter
        where setter r v = r { entryPublished = v }

instance ARights Entry (Maybe Text) where
    rights = Attr entryRights setter
        where setter r v = r { entryRights = v }

instance ASource Entry (Maybe Feed) where
    source = Attr entrySource setter
        where setter r v = r { entrySource = fmap (delete entries) v }

instance ASummary Entry (Maybe String) where
    summary = Attr entrySummary setter
        where setter r Nothing | check (get content r) = error "summary is required to this entry"
              setter r v = r { entrySummary = v }
              check (Just (BinaryData _ _)) = True
              check (Just (OtherSrc _ _))   = True
              check _ = False

instance ATitle Entry Text where
    title = Attr entryTitle setter
        where setter r v = r { entryTitle = v }

instance AUpdated Entry ZonedTime where
    updated = Attr entryUpdated setter
        where setter r v = r { entryUpdated = v }

instance ATerm Category String where
    term = Attr categoryTerm setter
        where setter r v = r { categoryTerm = v }

instance AScheme Category (Maybe String) where
    scheme = Attr categoryScheme setter
        where setter r v = r { categoryScheme = v }

instance ALabel Category (Maybe String) where
    label = Attr categoryLabel setter
        where setter r v = r { categoryLabel = v }

instance AName Generator String where
    name = Attr generatorName setter
        where setter r v = r { generatorName = v }

instance AUri Generator (Maybe String) where
    uri = Attr generatorUri setter
        where setter r v = r { generatorUri = v }

instance AVersion Generator (Maybe String) where
    version = Attr generatorVersion setter
        where setter r v = r { generatorVersion = v }

instance AHref Link String where
    href = Attr linkHref setter
        where setter r v = r { linkHref = v }

instance ARel Link LinkRelation where
    rel = Attr linkRel setter
        where setter r v = r { linkRel = v }

instance AMediatype Link (Maybe String) where
    mediatype = Attr linkMediatype setter
        where setter r v = r { linkMediatype = v }

instance AHreflang Link (Maybe String) where
    hreflang = Attr linkHreflang setter
        where setter r v = r { linkHreflang = v }

instance ATitle Link (Maybe String) where
    title = Attr linkTitle setter
        where setter r v = r { linkTitle = v }

instance ALength Link (Maybe Integer) where
    len = Attr linkLength setter
        where setter r v = r { linkLength = v }

------------------------------------------------------------
-- XML <-> ATOM data types interchange

class (AName r Text, AUri r (Maybe String), AEmail r (Maybe String)) 
    => PERSON r
instance PERSON Person
class (ATerm r String, AScheme r (Maybe String), ALabel r (Maybe String))
    => CATEGORY r
instance CATEGORY Category
class (AName r String, AUri r (Maybe String), AVersion r (Maybe String))
    => GENERATOR r
instance GENERATOR Generator
class ( AHref r String, ARel r LinkRelation, AMediatype r (Maybe String)
      , AHreflang r (Maybe String), ATitle r (Maybe String)
      , ALength r (Maybe Integer))
    => LINK r
instance LINK Link

class ( AAuthor r [Person], ACategory r [Category], AContributor r [Person]
      , AGenerator r [Generator], AIcon r (Maybe String), AId r String
      , ALink r [Link], ALogo r (Maybe String), ARights r (Maybe Text)
      , ASubtitle r (Maybe Text), ATitle r Text, AUpdated r ZonedTime)
    => FEED r
instance FEED Feed
class ( AAuthor r [Person], ACategory r [Category]
      , AContent r (Maybe EntryContent), AContributor r [Person], AId r String
      , ALink r [Link], APublished r (Maybe ZonedTime), ARights r (Maybe Text)
      , ASource r (Maybe Feed), ASummary r (Maybe String)
      , ATitle r Text, AUpdated r ZonedTime)
    => ENTRY r
instance ENTRY Entry

s2cont :: String -> Content
s2cont s = CString False (escapeString s)

simpleCont :: String -> [Content] -> Content
simpleCont t cs = CElem $ Elem t [] cs

p2cont :: String -> Person -> Content
p2cont t p = simpleCont t (n:(u++m))
    where n = t2cont "name" $ get name p
          u = toList $ fmap (\u -> simpleCont "uri" [s2cont u]) $ get uri p
          m = toList $ fmap (\m -> simpleCont "email" [s2cont m]) $ get email p
cont2p :: Content -> Person
cont2p (CElem (Elem _ _ cs)) = Person n u m
    where n = cont2t $ head $ getTags "name" cs
          u = fmap fromText' $ listToMaybe $ getChildren "uri" cs
          m = fmap fromText' $ listToMaybe $ getChildren "email" cs

t2cont :: String -> Text -> Content
t2cont t (Text s) = CElem $ Elem t [("type", str2attr "text")] [s2cont s]
t2cont t (HTML s) = CElem $ Elem t [("type", str2attr "html")] [s2cont s]
t2cont t (XHtml cont) = CElem $ Elem t [("type", str2attr "xhtml")] [CElem $ Elem "div" [("xmlns", str2attr "http://www.w3.org/1999/xhtml")] cont]

cont2t :: Content -> Text
cont2t (CElem (Elem _ ats cs)) =
       case typ of
         Just t | t == "html" || t == "text/html"   -> HTML contText
                | t == "xhtml" || t == "text/xhtml" -> XHtml cont
         _                                          -> Text contText 
    where typ = possibleA fromAttrToStr "type" ats
          contText = fromText' $ xUnEscape cs
          cont = stripSpaces `o` children $ head cs

zt2cont :: String -> ZonedTime -> Content
zt2cont t (ZonedTime d (TimeZone diffs _ _))
    = simpleCont t [s2cont $ formatTime defaultTimeLocale "%FT%T" d ++ zs ]
    where zs = if diffs == 0 then "Z"
               else let (zh,zm) = diffs `divMod` 60
                        (zh',zm') = if diffs < 0 && zm /= 0 
                                    then (zh+1,60-zm) else (zh,zm)
                        sig = if zh' < 0 then '-' else '+'
                    in sig : show2 zh' ++ ":" ++ show2 zm'
          show2 n | abs n < 10 = '0':show (abs n)
                  | otherwise  = show n


breaks :: Char -> String -> [String]
breaks c s = case rest of
               []     -> [s1]
               (_:s') -> s1:breaks c s'
    where (s1, rest) = break (==c) s

s2zt :: String -> ZonedTime
s2zt s = let (d, _:h') = break (=='T') s
             (h, z) = break (`elem` "+-Z") h'
             (year:mon:day:_) = breaks '-' d
             (hour:min:sec:_) = breaks ':' h
             zone = case z of
                      "Z" -> utc
                      zs@(_:_) -> 
                          let (zh:zm:_) = map read $ breaks ':' zs
                          in minutesToTimeZone ((abs zh * 60 + zm) * signum zh)
                      _ -> utc
         in ZonedTime (LocalTime (fromGregorian (read year) (read mon) (read day))
                                 (TimeOfDay (read hour) (read min) (fromRational $ toRational (read sec::Double))))
                      zone

fromMany :: (XmlContent a) => [Content] -> [a]
fromMany = fst . many fromElem

fromText' :: [Content] -> String
fromText' = concat . fst . many fromText

getTags :: String -> [Content] -> [Content]
getTags s = concatMap (tag s)

getChildren :: String -> [Content] -> [[Content]]
getChildren s = dropWhile null . map (tag s /> keep)

stripSpaces :: CFilter
stripSpaces = foldXml (ifTxt (\s -> if all isSpace s then none else keep) keep)

instance XmlContent Category where
    fromElem css@(CElem (Elem "category" ats cs):rest) = 
        case lookup "term" ats of
          Just v -> (Just (Category (attr2str v) s l), rest)
          Nothing -> (Nothing, css)
        where s = possibleA fromAttrToStr "scheme" ats
              l = possibleA fromAttrToStr "label" ats
    fromElem css = (Nothing, css)
    toElem c = [CElem (Elem "category" (catMaybes [t, s, l]) [])]
        where t = Just ("term", str2attr $ get term c)
              s = get scheme c >>= return . (,) "scheme" . str2attr
              l = get label c >>= return . (,) "label" . str2attr
instance XmlContent Generator where
    fromElem (CElem (Elem "generator" ats cs):rest) = 
        (Just (Generator n u v), rest)
        where n = fromText' cs
              u = possibleA fromAttrToStr "uri" ats
              v = possibleA fromAttrToStr "version" ats
    fromElem cs = (Nothing, cs)
    toElem g = [CElem $ Elem "generator" (catMaybes [u, v])
                          [s2cont $ get name g]]
        where u = get uri g >>= Just . (,) "uri" . str2attr
              v = get version g >>= Just . (,) "version" . str2attr
instance XmlContent Link where
    fromElem (CElem (Elem "link" ats cs):rest) =
        (Just (Link hr r t hl ti len), rest)
        where hr = case possibleA fromAttrToStr "href" ats of
                     Nothing -> error "link requires href attribute"
                     Just v -> v
              r = maybe Alternate read $ possibleA fromAttrToStr "rel" ats
              t = possibleA fromAttrToStr "type" ats
              hl = possibleA fromAttrToStr "hreflang" ats
              ti = possibleA fromAttrToStr "title" ats
              len = fmap read $ possibleA fromAttrToStr "length" ats
    fromElem cs = (Nothing, cs)
    toElem l = [CElem $ Elem "link" (hr:r:catMaybes [t,hl,ti,len']) []]
        where hr = ("href", str2attr $ get href l)
              r = ("rel", str2attr $ show $ get rel l)
              t = get mediatype l >>= return . (,) "type" . str2attr
              hl = get hreflang l >>= return . (,) "hreflang" . str2attr
              ti = get title l >>= return . (,) "title" . str2attr
              len' = get len l >>= return . (,) "length" . str2attr . show
instance XmlContent EntryContent where
    fromElem (CElem (Elem "content" ats cs):rest) = 
        case possibleA fromAttrToStr "type" ats of
          Just "text" -> (Just $ TextCont $ Text inText, rest)
          Just "html" -> (Just $ TextCont $ HTML inText, rest)
          Just "xhtml" -> (Just $ TextCont $ XHtml inHtml, rest)
          Just "text/html" -> (Just $ TextCont $ HTML inText, rest)
          Just "text/xhtml" -> (Just $ TextCont $ XHtml inHtml, rest)
          Just s | "text" `isPrefixOf` s -> 
                     (Just $ TextCont $ Text inText, rest)
                 | otherwise -> case src of
                                  Just d -> (Just $ OtherSrc s d, rest)
                                  Nothing -> (Just $ BinaryData s $ b64Decode inText, rest)
        where inText = fromText' cs
              inHtml = concatMap stripSpaces $ head $ getChildren "div" $ cs
              src = possibleA fromAttrToStr "src" ats
    fromElem cs = (Nothing, cs)
    toElem (TextCont (Text s)) = [CElem $ Elem "content" [("type", str2attr "text")] [s2cont s]]
    toElem (TextCont (HTML s)) = [CElem $ Elem "content" [("type", str2attr "html")] [s2cont s]]
    toElem (TextCont (XHtml cont)) = [CElem $ Elem "content" [("type", str2attr "xhtml")] [CElem $ Elem "div" [("xmlns", str2attr "http://www.w3.org/1999/xhtml")] cont]]
    toElem (BinaryData t d) = [CElem $ Elem "content" [("type", str2attr t)] [CString False (b64Encode d)]]
    toElem (OtherSrc t d) = [CElem $ Elem "content" [ ("type", str2attr t), ("src", str2attr t)] []]
instance XmlContent Entry where
    fromElem (CElem (Elem "entry" _ cs):rest) =
        (Just $ Entry authors cats content conts ident links pubd rts src summ tit upd, rest)
        where authors = map cont2p $ getTags "author" cs
              cats = fromMany $ getTags "category" cs
              content = listToMaybe $ fromMany $ getTags "content" cs
              conts = map cont2p $ getTags "contributor" cs
              ident = fromText' $ head $ getChildren "id" cs
              links = fromMany $ getTags "link" cs
              pubd = fmap (s2zt . fromText') $ listToMaybe $ getChildren "published" cs
              rts = fmap cont2t $ listToMaybe $ getTags "rights" cs
              src = (listToMaybe $ getChildren "source" cs) >>= fst . fromElem
              summ = fmap fromText' $ listToMaybe $ getChildren "summary" cs
              tit = cont2t $ head $ getTags "title" cs
              upd = s2zt $ fromText' $ head $ getChildren "updated" cs
    fromElem rest = (Nothing, rest)
    toElem e = [CElem $ Elem "entry" [] (concat [authors, cats, contents, conts, ident, links, pubd, rts, src, summ, tit, upd])]
        where authors = map (p2cont "author") $ get author e
              cats = toElem $ get category e
              contents = toElem $ get content e
              conts = map (p2cont "contributor") $ get contributor e
              ident = [simpleCont "id" [s2cont $ get identifier e]]
              links = toElem $ get link e
              pubd = toList $ fmap (zt2cont "published") $ get published e
              rts = toList $ fmap (t2cont "rights") $ get rights e
              src = toList $ fmap (\s -> CElem (Elem "source" [] (toElem s))) $ get source e 
              summ = toList $ fmap (\s -> simpleCont "summary" [s2cont s]) $ get summary e
              tit = [t2cont "title" $ get title e]
              upd = [zt2cont "updated" $ get updated e]
instance XmlContent Feed where
    fromElem (CElem (Elem "feed" _ cs):rest) = 
        (Just $ Feed authors cats conts gens ic ident links lg rts subtit tit upd ents, rest)
        where authors = map cont2p $ getTags "author" cs
              cats = fromMany $ getTags "category" cs
              conts = map cont2p $ getTags "contributor" cs
              gens = fromMany $ getTags "generator" cs
              ic = fmap fromText' $ listToMaybe $ getChildren "icon" cs
              ident = fromText' $ head $ getChildren  "id" cs
              links = fromMany $ getTags "link" cs
              lg = fmap fromText' $ listToMaybe $ getChildren "logo" cs
              rts = fmap cont2t $ listToMaybe $ getTags "rights" cs
              subtit = fmap cont2t $ listToMaybe $ getTags "subtitle" cs
              tit = cont2t $ head $ getTags "title" cs
              upd = s2zt $ fromText' $ head $ getChildren "updated" cs
              ents = fromMany $ getTags "entry" cs
    fromElem rest = (Nothing, rest)
    toElem f = [CElem $ Elem "feed" [("xmlns", str2attr "http://www.w3.org/2005/Atom")] (concat [authors, cats, conts, gens, ic, ident, links, lg, rts, subtit, tit, upd, ents])]
        where authors = map (p2cont "author") $ get author f
              cats = toElem $ get category f
              conts = map (p2cont "contributor") $ get contributor f
              gens = toElem $ get generator f
              ic = toList $ fmap (\i -> simpleCont "icon" [s2cont i]) $ get icon f
              ident = [simpleCont "id" [s2cont $ get identifier f]]
              links = toElem $ get link f
              lg = toList $ fmap (\l -> simpleCont "logo" [s2cont l]) $ get logo f
              rts = toList $ fmap (t2cont "rights") $ get rights f
              subtit = toList $ fmap (t2cont "subtitle") $ get subtitle f
              tit = [t2cont "title" $ get title f]
              upd = [zt2cont "updated" $ get updated f]
              ents = toElem $ feedEntries f
