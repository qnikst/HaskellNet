module Network.HaskellNet.IMAP
    ( connectIMAP, connectIMAPPort, connectStream
      -- * IMAP commands
      -- ** any state commands
    , noop, capability, logout
      -- ** not authenticated state commands
    , login, authenticate
      -- ** autenticated state commands
    , select, examine, create, delete, rename
    , subscribe, unsubscribe
    , list, lsub, status, append, appendFull, appendFullUID
      -- ** selected state commands
    , check, close, expunge
    , search, store, copy, copyUID, copyUIDs, copyUIDR, uidExpunge, uidExpungeR, move
    , idle
      -- * fetch commands
    , fetch, fetchHeader, fetchSize, fetchHeaderFields, fetchHeaderFieldsNot
    , fetchFlags, fetchR, fetchByString, fetchByStringR
    , fetchByByteString, fetchByByteStringR
    , fetchPeek, fetchRPeek
      -- * other types
    , Flag(..), Attribute(..), MailboxStatus(..)
    , AppendUID(..), CopyUID(..), UIDSet
    , SearchQuery(..), FlagsQuery(..)
    , A.AuthType(..)
    )
where

import Network.Compat
import qualified Network.HaskellNet.Auth as A
import Network.HaskellNet.BSStream
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.Parsers
import Network.HaskellNet.IMAP.Types
import Network.HaskellNet.IMAP.UTF7
import Network.Socket (PortNumber)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

import Control.Monad

import System.Time

import Data.Maybe
import Data.List hiding (delete)
import Data.Char

import Text.Packrat.Parse (Result)
import Control.Applicative -- support old toolchains
import Prelude

-- suffixed by `s'
data SearchQuery = ALLs
                 | FLAG Flag
                 | UNFLAG Flag
                 | BCCs String
                 | BEFOREs CalendarTime
                 | BODYs String
                 | CCs String
                 | FROMs String
                 | HEADERs String String
                 | LARGERs Integer
                 | NEWs
                 | NOTs SearchQuery
                 | OLDs
                 | ONs CalendarTime
                 | ORs SearchQuery SearchQuery
                 | SENTBEFOREs CalendarTime
                 | SENTONs CalendarTime
                 | SENTSINCEs CalendarTime
                 | SINCEs CalendarTime
                 | SMALLERs Integer
                 | SUBJECTs String
                 | TEXTs String
                 | TOs String
                 | XGMRAW String
                 | UIDs [UID]


instance Show SearchQuery where
    showsPrec d q = showParen (d>app_prec) $ showString $ showQuery q
        where app_prec = 10
              showQuery ALLs            = "ALL"
              showQuery (FLAG f)        = showFlag f
              showQuery (UNFLAG f)      = "UN" ++ showFlag f
              showQuery (BCCs addr)     = "BCC " ++ quoteIMAPString addr
              showQuery (BEFOREs t)     = "BEFORE " ++ dateToStringIMAP t
              showQuery (BODYs s)       = "BODY " ++ quoteIMAPString s
              showQuery (CCs addr)      = "CC " ++ quoteIMAPString addr
              showQuery (FROMs addr)    = "FROM " ++ quoteIMAPString addr
              showQuery (HEADERs f v)   = "HEADER " ++ f ++ " " ++ quoteIMAPString v
              showQuery (LARGERs siz)   = "LARGER " ++ show siz
              showQuery NEWs            = "NEW"
              showQuery (NOTs qry)      = "NOT " ++ show qry
              showQuery OLDs            = "OLD"
              showQuery (ONs t)         = "ON " ++ dateToStringIMAP t
              showQuery (ORs q1 q2)     = "OR " ++ show q1 ++ " " ++ show q2
              showQuery (SENTBEFOREs t) = "SENTBEFORE " ++ dateToStringIMAP t
              showQuery (SENTONs t)     = "SENTON " ++ dateToStringIMAP t
              showQuery (SENTSINCEs t)  = "SENTSINCE " ++ dateToStringIMAP t
              showQuery (SINCEs t)      = "SINCE " ++ dateToStringIMAP t
              showQuery (SMALLERs siz)  = "SMALLER " ++ show siz
              showQuery (SUBJECTs s)    = "SUBJECT " ++ quoteIMAPString s
              showQuery (TEXTs s)       = "TEXT " ++ quoteIMAPString s
              showQuery (TOs addr)      = "TO " ++ quoteIMAPString addr
              showQuery (XGMRAW s)      = "X-GM-RAW " ++ quoteIMAPString s
              showQuery (UIDs uids)     = concat $ intersperse "," $
                                          map show uids
              showFlag Seen        = "SEEN"
              showFlag Answered    = "ANSWERED"
              showFlag Flagged     = "FLAGGED"
              showFlag Deleted     = "DELETED"
              showFlag Draft       = "DRAFT"
              showFlag Recent      = "RECENT"
              showFlag (Keyword s) = "KEYWORD " ++ s

data FlagsQuery = ReplaceFlags [Flag]
                | PlusFlags [Flag]
                | MinusFlags [Flag]
                | ReplaceGmailLabels [GmailLabel]
                | PlusGmailLabels [GmailLabel]
                | MinusGmailLabels [GmailLabel]

----------------------------------------------------------------------
-- establish connection

connectIMAPPort :: String -> PortNumber -> IO IMAPConnection
connectIMAPPort hostname port =
    handleToStream <$> connectTo hostname port
    >>= connectStream

connectIMAP :: String -> IO IMAPConnection
connectIMAP hostname = connectIMAPPort hostname 143

connectStream :: BSStream -> IO IMAPConnection
connectStream s =
    do msg <- bsGetLine s
       unless (and $ BS.zipWith (==) msg (BS.pack "* OK")) $
              fail "cannot connect to the server"
       newConnection s

----------------------------------------------------------------------
-- normal send commands
sendCommand' :: IMAPConnection -> String -> IO (ByteString, Int)
sendCommand' c cmdstr = do
  num <- sendCommandNoResponse c cmdstr
  resp <- getResponse (stream c)
  return (resp, num)

sendCommandNoResponse :: IMAPConnection -> String -> IO Int
sendCommandNoResponse c cmdstr = do
  (_, num) <- withNextCommandNum c $ \num -> do
              let bytes = encodeUtf8 $ show6 num ++ " " ++ cmdstr
              BS.length bytes `seq` bsPutCrLf (stream c) bytes
  return num

encodeUtf8 :: String -> ByteString
encodeUtf8 = TextEncoding.encodeUtf8 . Text.pack

show6 :: (Ord a, Num a, Show a) => a -> String
show6 n | n > 100000 = show n
        | n > 10000  = '0' : show n
        | n > 1000   = "00" ++ show n
        | n > 100    = "000" ++ show n
        | n > 10     = "0000" ++ show n
        | otherwise  = "00000" ++ show n

sendCommand :: IMAPConnection -> String
            -> (RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, v))
            -> IO v
sendCommand imapc cmdstr pFunc = snd <$> sendCommandWithResponse imapc cmdstr pFunc

-- | Like 'sendCommand', but also returns the tagged 'ServerResponse' so callers
-- can inspect response codes (e.g. the UIDPLUS @COPYUID@/@APPENDUID@ codes).
sendCommandWithResponse :: IMAPConnection -> String
                        -> (RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, v))
                        -> IO (ServerResponse, v)
sendCommandWithResponse imapc cmdstr pFunc =
    do (buf, num) <- sendCommand' imapc cmdstr
       let (resp, mboxUp, value) = eval pFunc (show6 num) buf
       case resp of
         OK _ _        -> do mboxUpdate imapc mboxUp
                             return (resp, value)
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)

getResponse :: BSStream -> IO ByteString
getResponse s = unlinesCRLF <$> getLs
    where unlinesCRLF = BS.concat . concatMap (:[crlfStr])
          getLs =
              do l <- strip <$> bsGetLine s
                 case () of
                   _ | BS.null l -> return [l]
                     | isLiteral l ->  do l' <- getLiteral l (getLitLen l)
                                          ls <- getLs
                                          return (l' : ls)
                     | isTagged l -> (l:) <$> getLs
                     | otherwise -> return [l]
          getLiteral l len =
              do lit <- bsGet s len
                 l2 <- strip <$> bsGetLine s
                 let l' = BS.concat [l, crlfStr, lit, l2]
                 if isLiteral l2
                   then getLiteral l' (getLitLen l2)
                   else return l'
          crlfStr = BS.pack "\r\n"
          isLiteral l = not (BS.null l) &&
                        BS.last l == '}' &&
                        BS.last (fst (BS.spanEnd isDigit (BS.init l))) == '{'
          getLitLen = read . BS.unpack . snd . BS.spanEnd isDigit . BS.init
          isTagged l = BS.head l == '*' && BS.head (BS.tail l) == ' '

mboxUpdate :: IMAPConnection -> MboxUpdate -> IO ()
mboxUpdate conn (MboxUpdate exists' recent') = do
  when (isJust exists') $
       modifyMailboxInfo conn $ \mbox -> mbox { _exists = fromJust exists' }

  when (isJust recent') $
       modifyMailboxInfo conn $ \mbox -> mbox { _recent = fromJust recent' }

----------------------------------------------------------------------
-- IMAP commands
--

idle :: IMAPConnection -> Int -> IO ()
idle conn timeout =
    do
        (buf',num) <- sendCommand' conn "IDLE"
        buf <-
            if BS.take 2 buf' == BS.pack "+ "
                then do
                    _ <- bsWaitForInput (stream conn) timeout
                    bsPutCrLf (stream conn) $ BS.pack "DONE"
                    getResponse $ stream conn
                else
                    return buf'
        let (resp, mboxUp, value) = eval pNone (show6 num) buf
        case resp of
         OK _ _ -> do mboxUpdate conn mboxUp
                      return value
         NO _ msg -> fail ("NO: " ++ msg)
         BAD _ msg -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)

noop :: IMAPConnection -> IO ()
noop conn = sendCommand conn "NOOP" pNone

capability :: IMAPConnection -> IO [String]
capability conn = sendCommand conn "CAPABILITY" pCapability

logout :: IMAPConnection -> IO ()
logout c = do bsPutCrLf (stream c) $ BS.pack "a0001 LOGOUT"
              bsClose (stream c)

login :: IMAPConnection -> A.UserName -> A.Password -> IO ()
login conn username password = sendCommand conn ("LOGIN " ++ (escapeLogin username) ++ " " ++ (escapeLogin password))
                               pNone

authenticate :: IMAPConnection -> A.AuthType
             -> A.UserName -> A.Password -> IO ()
authenticate conn A.LOGIN username password =
    do (_, num) <- sendCommand' conn "AUTHENTICATE LOGIN"
       bsPutCrLf (stream conn) $ BS.pack userB64
       bsGetLine (stream conn)
       bsPutCrLf (stream conn) $ BS.pack passB64
       buf <- getResponse $ stream conn
       let (resp, mboxUp, value) = eval pNone (show6 num) buf
       case resp of
         OK _ _        -> do mboxUpdate conn $ mboxUp
                             return value
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)
    where (userB64, passB64) = A.login username password
authenticate conn at username password =
    do (c, num) <- sendCommand' conn $ "AUTHENTICATE " ++ show at
       let challenge =
               if BS.take 2 c == BS.pack "+ "
               then A.b64Decode $ BS.unpack $ head $
                    dropWhile (isSpace . BS.last) $ BS.inits $ BS.drop 2 c
               else ""
       bsPutCrLf (stream conn) $ BS.pack $
                 A.auth at challenge username password
       buf <- getResponse $ stream conn
       let (resp, mboxUp, value) = eval pNone (show6 num) buf
       case resp of
         OK _ _        -> do mboxUpdate conn $ mboxUp
                             return value
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)

_select :: String -> IMAPConnection -> String -> IO ()
_select cmd conn mboxName =
    do mbox' <- sendCommand conn (cmd ++ quoteMailboxName mboxName) pSelect
       setMailboxInfo conn $ mbox' { _mailbox = mboxName }

select :: IMAPConnection -> MailboxName -> IO ()
select = _select "SELECT "

examine :: IMAPConnection -> MailboxName -> IO ()
examine = _select "EXAMINE "

create :: IMAPConnection -> MailboxName -> IO ()
create conn mboxname = sendCommand conn ("CREATE " ++ quoteMailboxName mboxname) pNone

delete :: IMAPConnection -> MailboxName -> IO ()
delete conn mboxname = sendCommand conn ("DELETE " ++ quoteMailboxName mboxname) pNone

rename :: IMAPConnection -> MailboxName -> MailboxName -> IO ()
rename conn mboxorg mboxnew =
    sendCommand conn ("RENAME " ++ quoteMailboxName mboxorg ++ " " ++ quoteMailboxName mboxnew) pNone

subscribe :: IMAPConnection -> MailboxName -> IO ()
subscribe conn mboxname = sendCommand conn ("SUBSCRIBE " ++ quoteMailboxName mboxname) pNone

unsubscribe :: IMAPConnection -> MailboxName -> IO ()
unsubscribe conn mboxname = sendCommand conn ("UNSUBSCRIBE " ++ quoteMailboxName mboxname) pNone

list :: IMAPConnection -> IO [([Attribute], MailboxName)]
list conn = (map (\(a, _, m) -> (a, m))) <$> listFull conn "\"\"" "*"

lsub :: IMAPConnection -> IO [([Attribute], MailboxName)]
lsub conn = (map (\(a, _, m) -> (a, m))) <$> lsubFull conn "\"\"" "*"

listFull :: IMAPConnection -> String -> String
         -> IO [([Attribute], String, MailboxName)]
listFull conn ref pat = sendCommand conn (unwords ["LIST", ref, pat]) pList

lsubFull :: IMAPConnection -> String -> String
         -> IO [([Attribute], String, MailboxName)]
lsubFull conn ref pat = sendCommand conn (unwords ["LSUB", ref, pat]) pLsub

status :: IMAPConnection -> MailboxName -> [MailboxStatus]
       -> IO [(MailboxStatus, Integer)]
status conn mbox stats =
    let cmd = "STATUS " ++ quoteMailboxName mbox ++ " (" ++ (unwords $ map show stats) ++ ")"
    in sendCommand conn cmd pStatus

append :: IMAPConnection -> MailboxName -> ByteString -> IO ()
append conn mbox mailData = appendFull conn mbox mailData Nothing Nothing

appendFull :: IMAPConnection -> MailboxName -> ByteString
           -> Maybe [Flag] -> Maybe CalendarTime -> IO ()
appendFull conn mbox mailData flags' time =
    appendFullUID conn mbox mailData flags' time >> return ()

-- | Like 'appendFull', but returns the UIDPLUS @APPENDUID@ response code
-- (RFC 4315) when the server supports it, identifying the UID assigned to the
-- appended message. Returns 'Nothing' on servers that don't advertise UIDPLUS.
appendFullUID :: IMAPConnection -> MailboxName -> ByteString
              -> Maybe [Flag] -> Maybe CalendarTime -> IO (Maybe AppendUID)
appendFullUID conn mbox mailData flags' time =
    do (buf, num) <- sendCommand' conn
                (concat ["APPEND ", quoteMailboxName mbox
                        , fstr, tstr, " {" ++ show len ++ "}"])
       when (BS.null buf || (BS.head buf /= '+')) $
              fail "illegal server response"
       bsPut (stream conn) mailData
       bsPutCrLf (stream conn) BS.empty
       buf2 <- getResponse $ stream conn
       let (resp, mboxUp, ()) = eval pNone (show6 num) buf2
       case resp of
         OK stat _     -> do mboxUpdate conn mboxUp
                             return $ appendUIDFromStatus stat
         NO _ msg      -> fail ("NO: "++msg)
         BAD _ msg     -> fail ("BAD: "++msg)
         PREAUTH _ msg -> fail ("PREAUTH: "++msg)
    where len       = BS.length mailData
          tstr      = maybe "" ((" "++) . datetimeToStringIMAP) time
          fstr      = maybe "" ((" ("++) . (++")") . unwords . map show) flags'
          appendUIDFromStatus (Just (APPENDUID_sc appendUID')) = Just appendUID'
          appendUIDFromStatus _ = Nothing

check :: IMAPConnection -> IO ()
check conn = sendCommand conn "CHECK" pNone

close :: IMAPConnection -> IO ()
close conn =
    do sendCommand conn "CLOSE" pNone
       setMailboxInfo conn emptyMboxInfo

expunge :: IMAPConnection -> IO [Integer]
expunge conn = sendCommand conn "EXPUNGE" pExpunge

search :: IMAPConnection -> [SearchQuery] -> IO [UID]
search conn queries =
    let charset = if any searchQueryNeedsUtf8 queries
                  then "CHARSET UTF-8"
                  else ""
    in searchCharset conn charset queries

-- | Whether a search query carries non-ASCII text and therefore needs an
-- explicit @CHARSET UTF-8@ on the SEARCH command (RFC 3501 §6.4.4).
searchQueryNeedsUtf8 :: SearchQuery -> Bool
searchQueryNeedsUtf8 (BCCs s)       = containsNonAscii s
searchQueryNeedsUtf8 (BODYs s)      = containsNonAscii s
searchQueryNeedsUtf8 (CCs s)        = containsNonAscii s
searchQueryNeedsUtf8 (FROMs s)      = containsNonAscii s
searchQueryNeedsUtf8 (HEADERs f v)  = containsNonAscii f || containsNonAscii v
searchQueryNeedsUtf8 (NOTs q)       = searchQueryNeedsUtf8 q
searchQueryNeedsUtf8 (ORs q1 q2)    = searchQueryNeedsUtf8 q1 || searchQueryNeedsUtf8 q2
searchQueryNeedsUtf8 (SUBJECTs s)   = containsNonAscii s
searchQueryNeedsUtf8 (TEXTs s)      = containsNonAscii s
searchQueryNeedsUtf8 (TOs s)        = containsNonAscii s
searchQueryNeedsUtf8 (XGMRAW s)     = containsNonAscii s
searchQueryNeedsUtf8 _              = False

containsNonAscii :: String -> Bool
containsNonAscii = any ((> 0x7f) . ord)

searchCharset :: IMAPConnection -> Charset -> [SearchQuery]
              -> IO [UID]
searchCharset conn charset queries =
    sendCommand conn ("UID SEARCH "
                    ++ (if not . null $ charset
                           then charset ++ " "
                           else "")
                    ++ unwords (map show queries)) pSearch

fetch :: IMAPConnection -> UID -> IO ByteString
fetch conn uid =
    do lst <- fetchByByteString conn uid "BODY[]"
       return $ fromMaybe BS.empty $ lookup' "BODY[]" lst

-- | Like 'fetch' but without marking the email as seen/read
fetchPeek :: IMAPConnection -> UID -> IO ByteString
fetchPeek conn uid =
    do lst <- fetchByByteString conn uid "BODY.PEEK[]"
       return $ fromMaybe BS.empty $ lookup' "BODY[]" lst

fetchHeader :: IMAPConnection -> UID -> IO ByteString
fetchHeader conn uid =
    do lst <- fetchByByteString conn uid "BODY[HEADER]"
       return $ fromMaybe BS.empty $ lookup' "BODY[HEADER]" lst

fetchSize :: IMAPConnection -> UID -> IO Int
fetchSize conn uid =
    do lst <- fetchByByteString conn uid "RFC822.SIZE"
       return $ maybe 0 (read . BS.unpack) $ lookup "RFC822.SIZE" lst

fetchHeaderFields :: IMAPConnection
                  -> UID -> [String] -> IO ByteString
fetchHeaderFields conn uid hs =
    do let fetchCmd = "BODY[HEADER.FIELDS ("++unwords hs++")]"
       lst <- fetchByByteString conn uid fetchCmd
       return $ fromMaybe BS.empty $ lookup' fetchCmd lst

fetchHeaderFieldsNot :: IMAPConnection
                     -> UID -> [String] -> IO ByteString
fetchHeaderFieldsNot conn uid hs =
    do let fetchCmd = "BODY[HEADER.FIELDS.NOT ("++unwords hs++")]"
       lst <- fetchByByteString conn uid fetchCmd
       return $ fromMaybe BS.empty $ lookup' fetchCmd lst

fetchFlags :: IMAPConnection -> UID -> IO [Flag]
fetchFlags conn uid =
    do lst <- fetchByByteString conn uid "FLAGS"
       return $ getFlags $ lookup "FLAGS" lst
    where getFlags Nothing  = []
          getFlags (Just s) = eval' dvFlags "" (BS.unpack s)

fetchR :: IMAPConnection -> (UID, UID)
       -> IO [(UID, ByteString)]
fetchR conn r =
    do lst <- fetchByByteStringR conn r "BODY[]"
       return $ map (\(uid, vs) -> (uid, fromMaybe BS.empty $
                                       lookup' "BODY[]" vs)) lst

-- | Like 'fetchR' but without marking the email as seen/read
fetchRPeek :: IMAPConnection -> (UID, UID) -> IO [(UID, ByteString)]
fetchRPeek conn range =
    do ls <- fetchByByteStringR conn range "BODY.PEEK[]"
       return $ map (\(uid, vs) -> (uid, fromMaybe BS.empty $ lookup' "BODY[]" vs)) ls

-- | Fetch arbitrary data items and return values as 'String's.
--
-- This is kept for compatibility. Prefer 'fetchByByteString' for message
-- bodies or other data that may be large or non-textual.
fetchByString :: IMAPConnection -> UID -> String
              -> IO [(String, String)]
fetchByString conn uid command =
    map (\(key, value) -> (key, BS.unpack value)) <$> fetchByByteString conn uid command

-- | Fetch arbitrary data items and return raw 'ByteString' values.
--
-- Literal values are read directly from the stream instead of first building a
-- full response buffer, so this is the preferred API for large messages.
fetchByByteString :: IMAPConnection -> UID -> String
                  -> IO [(String, ByteString)]
fetchByByteString conn uid command =
    do lst <- fetchCommandBS conn ("UID FETCH "++show uid++" "++command) id
       case lst of
         (_, pairs):_ -> return pairs
         [] -> return []

-- | Range variant of 'fetchByString'.
fetchByStringR :: IMAPConnection -> (UID, UID) -> String
               -> IO [(UID, [(String, String)])]
fetchByStringR conn (s, e) command =
    map unpackFetch <$> fetchByByteStringR conn (s, e) command
    where unpackFetch (uid, pairs) =
              (uid, map (\(key, value) -> (key, BS.unpack value)) pairs)

-- | Range variant of 'fetchByByteString'.
fetchByByteStringR :: IMAPConnection -> (UID, UID) -> String
                   -> IO [(UID, [(String, ByteString)])]
fetchByByteStringR conn (s, e) command =
    fetchCommandBS conn ("UID FETCH "++show s++":"++show e++" "++command) proc
    where proc (n, ps) =
              (maybe (toEnum (fromIntegral n)) (read . BS.unpack) (lookup' "UID" ps), ps)

fetchCommand :: IMAPConnection -> String
             -> ((Integer, [(String, String)]) -> b) -> IO [b]
fetchCommand conn command proc =
    fetchCommandBS conn command $ \(n, ps) ->
        proc (n, map (\(key, value) -> (key, BS.unpack value)) ps)

fetchCommandBS :: IMAPConnection -> String
               -> ((Integer, [(String, ByteString)]) -> b) -> IO [b]
fetchCommandBS conn command proc =
    do num <- sendCommandNoResponse conn command
       (resp, mboxUp, values) <- getFetchResponseBS (stream conn) (show6 num)
       case resp of
         OK _ _        -> do mboxUpdate conn mboxUp
                             return $ map proc values
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)

-- Streaming FETCH response parser. Literal payloads are read with 'bsGet' so
-- large message bodies do not require buffering the complete server response.
getFetchResponseBS :: BSStream -> String
                   -> IO (ServerResponse, MboxUpdate, [(Integer, [(String, ByteString)])])
getFetchResponseBS s tag = go Nothing Nothing []
  where
    go exists' recent' fetches = do
        line <- strip <$> bsGetLine s
        case parseTaggedOrFatalLineBS tag line of
          Just resp -> return (resp, MboxUpdate exists' recent', reverse fetches)
          Nothing ->
              case parseFetchLineStartBS line of
                Just fetchInput -> do
                    fetchLine <- parseFetchLineBS s fetchInput
                    go exists' recent' (fetchLine:fetches)
                Nothing ->
                    case parseNumberedUpdateLineBS "EXISTS" line of
                      Just n -> go (Just n) recent' fetches
                      Nothing ->
                          case parseNumberedUpdateLineBS "RECENT" line of
                            Just n -> go exists' (Just n) fetches
                            Nothing -> do
                                skipLineLiteralsBS s line
                                go exists' recent' fetches

parseTaggedOrFatalLineBS :: String -> ByteString -> Maybe ServerResponse
parseTaggedOrFatalLineBS tag line =
    if isTaggedLine line || isFatalLine line
       then let (resp, _, ()) = eval pNone tag (BS.append line crlf)
            in Just resp
       else Nothing
  where
    isTaggedLine = BS.isPrefixOf (BS.pack $ tag ++ " ")
    isFatalLine input =
        case stripPrefixBS (BS.pack "* ") input of
          Just rest -> startsWithCIBS (BS.pack "BYE") rest
          Nothing -> False

parseFetchLineStartBS :: ByteString -> Maybe (Integer, ByteString)
parseFetchLineStartBS input =
    do rest1 <- stripPrefixBS (BS.pack "* ") input
       (numBytes, rest2) <- span1BS isDigit rest1
       rest3 <- stripSpaces1BS rest2
       rest4 <- stripPrefixCIBS (BS.pack "FETCH") rest3
       rest5 <- stripCharBS '(' $ dropSpacesBS rest4
       return (read $ BS.unpack numBytes, rest5)

parseFetchLineBS :: BSStream -> (Integer, ByteString)
                 -> IO (Integer, [(String, ByteString)])
parseFetchLineBS s (num, input) =
    do pairs <- parseFetchPairsBS s [] input
       return (num, pairs)

parseFetchPairsBS :: BSStream -> [(String, ByteString)] -> ByteString
                  -> IO [(String, ByteString)]
parseFetchPairsBS s pairs input =
    case BS.uncons input of
      Just (')', rest) | BS.null (dropSpacesBS rest) -> return $ reverse pairs
      _ -> case parseFetchKeyBS input of
             Just (key, rest1) -> do
                 valueResult <- parseFetchValueBS s rest1
                 case valueResult of
                   Just (value, rest2) ->
                       parseFetchPairsBS s ((key, value):pairs) $ dropSpacesBS rest2
                   Nothing -> fetchParseError "cannot parse FETCH value" input
             Nothing -> fetchParseError "cannot parse FETCH key" input

parseFetchKeyBS :: ByteString -> Maybe (String, ByteString)
parseFetchKeyBS input =
    let (name, rest1) = BS.span isFetchKeyChar input
    in if BS.null name
       then Nothing
       else do (section, rest2) <- parseFetchSectionBS rest1
               rest3 <- stripSpaces1BS rest2
               return (map toUpper (BS.unpack name) ++ BS.unpack section, rest3)
  where
    isFetchKeyChar c = not $ c `elem` " [)\r\n"

parseFetchSectionBS :: ByteString -> Maybe (ByteString, ByteString)
parseFetchSectionBS input =
    case BS.uncons input of
      Just ('[', rest1) ->
          let (sectionBody, rest2) = BS.break (== ']') rest1
          in case BS.uncons rest2 of
               Just (']', rest3) ->
                   do (origin, rest4) <- parseFetchOriginBS rest3
                      return (BS.concat [BS.pack "[", sectionBody, BS.pack "]", origin], rest4)
               _ -> Nothing
      _ -> Just (BS.empty, input)

parseFetchOriginBS :: ByteString -> Maybe (ByteString, ByteString)
parseFetchOriginBS input =
    case BS.uncons input of
      Just ('<', rest1) ->
          do (digits, rest2) <- span1BS isDigit rest1
             rest3 <- stripCharBS '>' rest2
             return (BS.concat [BS.pack "<", digits, BS.pack ">"], rest3)
      _ -> Just (BS.empty, input)

parseFetchValueBS :: BSStream -> ByteString -> IO (Maybe (ByteString, ByteString))
parseFetchValueBS s input =
    case BS.uncons input of
      Just ('(', _) -> return $ parseParenValueBS input
      Just ('{', _) -> Just <$> parseLiteralValueBS s input
      Just ('~', rest) | BS.take 1 rest == BS.pack "{" ->
          Just <$> parseLiteralValueBS s input
      Just ('"', _) -> return $ parseQuotedValueBS input
      _ -> return $ parseAtomValueBS input

parseParenValueBS :: ByteString -> Maybe (ByteString, ByteString)
parseParenValueBS input =
    do valueLen <- scanParenValueEndBS input
       let (value, rest) = BS.splitAt valueLen input
       return (value, rest)

parseLiteralValueBS :: BSStream -> ByteString -> IO (ByteString, ByteString)
parseLiteralValueBS s input =
    case parseLiteralMarkerBS input of
      Just literalLen -> do
          literal <- bsGet s literalLen
          if BS.length literal /= literalLen
             then fetchParseError "short FETCH literal" input
             else do tailLine <- strip <$> bsGetLine s
                     return (literal, tailLine)
      Nothing -> fetchParseError "cannot parse FETCH literal marker" input

parseLiteralMarkerBS :: ByteString -> Maybe Int
parseLiteralMarkerBS input =
    do rest1 <- case BS.uncons input of
                  Just ('~', rest) -> Just rest
                  _ -> Just input
       rest2 <- stripCharBS '{' rest1
       (lenBytes, rest3) <- span1BS isDigit rest2
       let rest4 = case BS.uncons rest3 of
                     Just ('+', rest) -> rest
                     _ -> rest3
       rest5 <- stripCharBS '}' rest4
       if BS.null (dropSpacesBS rest5)
          then Just $ read $ BS.unpack lenBytes
          else Nothing

skipLineLiteralsBS :: BSStream -> ByteString -> IO ()
skipLineLiteralsBS s line =
    case literalLengthAtLineEndBS line of
      Just literalLen -> do
          _ <- bsGet s literalLen
          nextLine <- strip <$> bsGetLine s
          skipLineLiteralsBS s nextLine
      Nothing -> return ()

literalLengthAtLineEndBS :: ByteString -> Maybe Int
literalLengthAtLineEndBS line =
    let stripped = strip line
    in if BS.length stripped >= 3 && BS.last stripped == '}'
       then parseLiteralTailBS $ reverse $ BS.unpack $ BS.init stripped
       else Nothing
  where
    parseLiteralTailBS revBeforeClose =
        case break (== '{') revBeforeClose of
          (insideRev, _ : _) -> parseLiteralInsideBS $ reverse insideRev
          _ -> Nothing
    parseLiteralInsideBS inside =
        let digits' = case reverse inside of
                        '+' : rest -> reverse rest
                        _ -> inside
        in if not (null digits') && all isDigit digits'
           then Just $ read digits'
           else Nothing

parseQuotedValueBS :: ByteString -> Maybe (ByteString, ByteString)
parseQuotedValueBS input =
    do valueLen <- scanQuotedValueEndBS 1 input
       let (value, rest) = BS.splitAt valueLen input
       return (value, rest)

parseAtomValueBS :: ByteString -> Maybe (ByteString, ByteString)
parseAtomValueBS input =
    let (value, rest) = BS.span isAtomValueChar input
    in if BS.null value then Nothing else Just (value, rest)
  where
    isAtomValueChar c = not $ c `elem` " (){%*\"\\]\r\n"

scanParenValueEndBS :: ByteString -> Maybe Int
scanParenValueEndBS input =
    case BS.uncons input of
      Just ('(', _) -> go 0 0
      _ -> Nothing
  where
    inputLen = BS.length input
    go :: Int -> Int -> Maybe Int
    go i depth
        | i >= inputLen = Nothing
        | otherwise =
            case BS.index input i of
              '"' -> do next <- scanQuotedValueEndBS (i + 1) input
                        go next depth
              '(' -> go (i + 1) (depth + 1)
              ')' | depth == 1 -> Just (i + 1)
                  | depth > 1 -> go (i + 1) (depth - 1)
                  | otherwise -> Nothing
              _ -> go (i + 1) depth

scanQuotedValueEndBS :: Int -> ByteString -> Maybe Int
scanQuotedValueEndBS = go
  where
    go i input
        | i >= BS.length input = Nothing
        | otherwise =
            case BS.index input i of
              '\\' -> go (i + 2) input
              '"' -> Just (i + 1)
              _ -> go (i + 1) input

parseNumberedUpdateLineBS :: String -> ByteString -> Maybe Integer
parseNumberedUpdateLineBS name input =
    do rest1 <- stripPrefixBS (BS.pack "* ") input
       (numBytes, rest2) <- span1BS isDigit rest1
       rest3 <- stripSpaces1BS rest2
       rest4 <- stripPrefixCIBS (BS.pack name) rest3
       if BS.null (dropSpacesBS rest4)
          then Just $ read $ BS.unpack numBytes
          else Nothing

stripPrefixBS :: ByteString -> ByteString -> Maybe ByteString
stripPrefixBS prefix input =
    if prefix `BS.isPrefixOf` input
    then Just $ BS.drop (BS.length prefix) input
    else Nothing

stripPrefixCIBS :: ByteString -> ByteString -> Maybe ByteString
stripPrefixCIBS prefix input =
    if startsWithCIBS prefix input
    then Just $ BS.drop (BS.length prefix) input
    else Nothing

startsWithCIBS :: ByteString -> ByteString -> Bool
startsWithCIBS prefix input =
    let prefixLen = BS.length prefix
        candidate = BS.take prefixLen input
    in BS.length candidate == prefixLen
       && BS.map toUpper candidate == BS.map toUpper prefix

stripCharBS :: Char -> ByteString -> Maybe ByteString
stripCharBS expected input =
    case BS.uncons input of
      Just (actual, rest) | actual == expected -> Just rest
      _ -> Nothing

span1BS :: (Char -> Bool) -> ByteString -> Maybe (ByteString, ByteString)
span1BS predicate input =
    let result@(matching, _) = BS.span predicate input
    in if BS.null matching then Nothing else Just result

dropSpacesBS :: ByteString -> ByteString
dropSpacesBS = BS.dropWhile (== ' ')

stripSpaces1BS :: ByteString -> Maybe ByteString
stripSpaces1BS input =
    let rest = dropSpacesBS input
    in if BS.length rest == BS.length input then Nothing else Just rest

fetchParseError :: String -> ByteString -> a
fetchParseError message input =
    error $ message ++ ": " ++ show (BS.take 120 input)

storeFull :: IMAPConnection -> String -> FlagsQuery -> Bool
          -> IO [(UID, [Flag])]
storeFull conn uidstr query isSilent =
    fetchCommand conn ("UID STORE " ++ uidstr ++ " " ++ flgs query) procStore
    where fstrs fs = "(" ++ (concat $ intersperse " " $ map show fs) ++ ")"
          toFStr s fstrs' =
              s ++ (if isSilent then ".SILENT" else "") ++ " " ++ fstrs'
          flgs (ReplaceGmailLabels ls) = toFStr "X-GM-LABELS" $ fstrs ls
          flgs (PlusGmailLabels ls)    = toFStr "+X-GM-LABELS" $ fstrs ls
          flgs (MinusGmailLabels ls)   = toFStr "-X-GM-LABELS" $ fstrs ls
          flgs (ReplaceFlags fs)       = toFStr "FLAGS" $ fstrs fs
          flgs (PlusFlags fs)          = toFStr "+FLAGS" $ fstrs fs
          flgs (MinusFlags fs)         = toFStr "-FLAGS" $ fstrs fs
          procStore (n, ps) = (maybe (toEnum (fromIntegral n)) read
                                         (lookup' "UID" ps)
                              ,maybe [] (eval' dvFlags "") (lookup' "FLAG" ps))


store :: IMAPConnection -> UID -> FlagsQuery -> IO ()
store conn i q = storeFull conn (show i) q True >> return ()

copyFull :: IMAPConnection -> String -> String -> IO ()
copyFull conn uidStr mbox = copyUIDFull conn uidStr mbox >> return ()

-- | Like 'copyFull', but returns the UIDPLUS @COPYUID@ response code (RFC 4315)
-- when the server supports it, identifying the UIDs assigned to the copied
-- messages in the destination mailbox. Returns 'Nothing' on servers that don't
-- advertise UIDPLUS.
copyUIDFull :: IMAPConnection -> String -> String -> IO (Maybe CopyUID)
copyUIDFull conn uidStr mbox =
    do (resp, ()) <- sendCommandWithResponse conn ("UID COPY " ++ uidStr ++ " " ++ quoteMailboxName mbox) pNone
       return $ copyUIDFromResponse resp
  where
    copyUIDFromResponse (OK (Just (COPYUID_sc copyUID')) _) = Just copyUID'
    copyUIDFromResponse _ = Nothing

copy :: IMAPConnection -> UID -> MailboxName -> IO ()
copy conn uid mbox     = copyFull conn (show uid) mbox

-- | Copy a single message to a mailbox, returning its UIDPLUS @COPYUID@ code.
copyUID :: IMAPConnection -> UID -> MailboxName -> IO (Maybe CopyUID)
copyUID conn uid mbox = copyUIDFull conn (show uid) mbox

-- | Copy a set of messages to a mailbox, returning the UIDPLUS @COPYUID@ code.
copyUIDs :: IMAPConnection -> [UID] -> MailboxName -> IO (Maybe CopyUID)
copyUIDs _ [] _ = fail "copyUIDs: empty UID set"
copyUIDs conn uids mbox = copyUIDFull conn (showUIDList uids) mbox

-- | Copy a contiguous UID range (inclusive) to a mailbox, returning the
-- UIDPLUS @COPYUID@ code.
copyUIDR :: IMAPConnection -> (UID, UID) -> MailboxName -> IO (Maybe CopyUID)
copyUIDR conn range mbox = copyUIDFull conn (showUIDRange range) mbox

-- | @UID EXPUNGE@ (RFC 4315): permanently remove only the \\Deleted messages
-- within the given UID set, leaving other \\Deleted messages untouched. Returns
-- the message sequence numbers expunged.
uidExpunge :: IMAPConnection -> [UID] -> IO [Integer]
uidExpunge _ [] = fail "uidExpunge: empty UID set"
uidExpunge conn uids = uidExpungeBySet conn $ showUIDList uids

-- | Like 'uidExpunge' but over a contiguous UID range (inclusive).
uidExpungeR :: IMAPConnection -> (UID, UID) -> IO [Integer]
uidExpungeR conn range = uidExpungeBySet conn $ showUIDRange range

uidExpungeBySet :: IMAPConnection -> UIDSet -> IO [Integer]
uidExpungeBySet conn uidSet = sendCommand conn ("UID EXPUNGE " ++ uidSet) pExpunge

showUIDList :: [UID] -> UIDSet
showUIDList = intercalate "," . map show

showUIDRange :: (UID, UID) -> UIDSet
showUIDRange (start, end) = show start ++ ":" ++ show end

move :: IMAPConnection -> UID -> MailboxName -> IO ()
move conn uid mboxname = sendCommand conn ("UID MOVE " ++ show uid ++ " " ++ quoteMailboxName mboxname) pNone

----------------------------------------------------------------------
-- auxialiary functions

quoteMailboxName :: MailboxName -> String
quoteMailboxName = quoteIMAPString . encodeMailboxName

quoteIMAPString :: String -> String
quoteIMAPString s = "\"" ++ concatMap escapeChar s ++ "\""
    where escapeChar '"' = "\\\""
          escapeChar '\\' = "\\\\"
          escapeChar c = [c]

showMonth :: Month -> String
showMonth January   = "Jan"
showMonth February  = "Feb"
showMonth March     = "Mar"
showMonth April     = "Apr"
showMonth May       = "May"
showMonth June      = "Jun"
showMonth July      = "Jul"
showMonth August    = "Aug"
showMonth September = "Sep"
showMonth October   = "Oct"
showMonth November  = "Nov"
showMonth December  = "Dec"

show2 :: Int -> String
show2 n | n < 10    = '0' : show n
        | otherwise = show n


show4 :: (Ord a, Num a, Show a) => a -> String
show4 n | n > 1000 = show n
        | n > 100  = '0' : show n
        | n > 10   = "00" ++ show n
        | otherwise  = "000" ++ show n

dateToStringIMAP :: CalendarTime -> String
dateToStringIMAP date = concat $ intersperse "-" [show2 $ ctDay date
                                                 , showMonth $ ctMonth date
                                                 , show $ ctYear date]
timeToStringIMAP :: CalendarTime -> String
timeToStringIMAP c = concat
                     $ intersperse ":"
                     $ fmap show2 [ctHour c, ctMin c, ctSec c]

-- Convert CalenarTime to "date-time" string per RFC3501
datetimeToStringIMAP :: CalendarTime -> String
datetimeToStringIMAP c =
  "\""
  ++ dateToStringIMAP c
  ++ " "
  ++ timeToStringIMAP c
  ++ " "
  ++ zone (ctTZ c)
  ++ "\""
  where
    zone s =
      (if s>=0 then "+" else "-") ++
      show4 (s `div` 3600)

strip :: ByteString -> ByteString
strip = fst . BS.spanEnd isSpace . BS.dropWhile isSpace

crlf :: BS.ByteString
crlf = BS.pack "\r\n"

bsPutCrLf :: BSStream -> ByteString -> IO ()
bsPutCrLf h s = bsPut h s >> bsPut h crlf >> bsFlush h

lookup' :: String -> [(String, b)] -> Maybe b
lookup' _ [] = Nothing
lookup' q ((k,v):xs) | matchesFetchKey q k = return v
                     | otherwise        = lookup' q xs

matchesFetchKey :: String -> String -> Bool
matchesFetchKey expected actual =
    expected == actual || normalizeFetchKey expected == normalizeFetchKey actual

normalizeFetchKey :: String -> String
normalizeFetchKey = stripOrigin . stripPeek . map toUpper
  where
    stripPeek key =
        case stripPrefix "BODY.PEEK[" key of
          Just rest -> "BODY[" ++ rest
          Nothing -> key
    stripOrigin key =
        case break (== '<') key of
          (bodySection, '<':_) | "BODY[" `isPrefixOf` bodySection -> bodySection
          _ -> key

-- TODO: This is just a first trial solution for this stack overflow question:
--       http://stackoverflow.com/questions/26183675/error-when-fetching-subject-from-email-using-haskellnets-imap
--       It must be reviewed. References: rfc3501#6.2.3, rfc2683#3.4.2.
--       This function was tested against the password: `~1!2@3#4$5%6^7&8*9(0)-_=+[{]}\|;:'",<.>/? (with spaces in the laterals).
escapeLogin :: String -> String
escapeLogin x = "\"" ++ replaceSpecialChars x ++ "\""
    where
        replaceSpecialChars ""     = ""
        replaceSpecialChars (c:cs) = escapeChar c ++ replaceSpecialChars cs
        escapeChar '"' = "\\\""
        escapeChar '\\' = "\\\\"
        escapeChar '{' = "\\{"
        escapeChar '}' = "\\}"
        escapeChar s   = [s]
