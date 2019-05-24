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
    , list, lsub, status, append
      -- ** selected state commands
    , check, close, expunge
    , search, store, copy
    , idle
      -- * fetch commands
    , fetch, fetchHeader, fetchSize, fetchHeaderFields, fetchHeaderFieldsNot
    , fetchFlags, fetchR, fetchByString, fetchByStringR
      -- * other types
    , Flag(..), Attribute(..), MailboxStatus(..)
    , SearchQuery(..), FlagsQuery(..)
    , A.AuthType(..)
    )
where

import Network.Socket (PortNumber)
import Network.Compat
import Network.HaskellNet.BSStream
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.Types
import Network.HaskellNet.IMAP.Parsers
import qualified Network.HaskellNet.Auth as A

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Control.Applicative ((<$>), (<*>))
import Control.Monad

import System.Time

import Data.Maybe
import Data.List hiding (delete)
import Data.Char

import Text.Packrat.Parse (Result)

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
                 | UIDs [UID]


instance Show SearchQuery where
    showsPrec d q = showParen (d>app_prec) $ showString $ showQuery q
        where app_prec = 10
              showQuery ALLs            = "ALL"
              showQuery (FLAG f)        = showFlag f
              showQuery (UNFLAG f)      = "UN" ++ showFlag f
              showQuery (BCCs addr)     = "BCC " ++ addr
              showQuery (BEFOREs t)     = "BEFORE " ++ dateToStringIMAP t
              showQuery (BODYs s)       = "BODY " ++ s
              showQuery (CCs addr)      = "CC " ++ addr
              showQuery (FROMs addr)    = "FROM " ++ addr
              showQuery (HEADERs f v)   = "HEADER " ++ f ++ " " ++ v
              showQuery (LARGERs siz)   = "LARGER {" ++ show siz ++ "}"
              showQuery NEWs            = "NEW"
              showQuery (NOTs qry)      = "NOT " ++ show qry
              showQuery OLDs            = "OLD"
              showQuery (ONs t)         = "ON " ++ dateToStringIMAP t
              showQuery (ORs q1 q2)     = "OR " ++ show q1 ++ " " ++ show q2
              showQuery (SENTBEFOREs t) = "SENTBEFORE " ++ dateToStringIMAP t
              showQuery (SENTONs t)     = "SENTON " ++ dateToStringIMAP t
              showQuery (SENTSINCEs t)  = "SENTSINCE " ++ dateToStringIMAP t
              showQuery (SINCEs t)      = "SINCE " ++ dateToStringIMAP t
              showQuery (SMALLERs siz)  = "SMALLER {" ++ show siz ++ "}"
              showQuery (SUBJECTs s)    = "SUBJECT " ++ s
              showQuery (TEXTs s)       = "TEXT " ++ s
              showQuery (TOs addr)      = "TO " ++ addr
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
  (_, num) <- withNextCommandNum c $ \num -> bsPutCrLf (stream c) $
              BS.pack $ show6 num ++ " " ++ cmdstr
  resp <- getResponse (stream c)
  return (resp, num)

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
sendCommand imapc cmdstr pFunc =
    do (buf, num) <- sendCommand' imapc cmdstr
       let (resp, mboxUp, value) = eval pFunc (show6 num) buf
       case resp of
         OK _ _        -> do mboxUpdate imapc mboxUp
                             return value
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
          isLiteral l = BS.last l == '}' &&
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
         OK _ _        -> do mboxUpdate conn mboxUp
                             return value
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
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
    do mbox' <- sendCommand conn (cmd ++ quoted mboxName) pSelect
       setMailboxInfo conn $ mbox' { _mailbox = mboxName }
    where
       quoted s = "\"" ++ s ++ "\""

select :: IMAPConnection -> MailboxName -> IO ()
select = _select "SELECT "

examine :: IMAPConnection -> MailboxName -> IO ()
examine = _select "EXAMINE "

create :: IMAPConnection -> MailboxName -> IO ()
create conn mboxname = sendCommand conn ("CREATE " ++ mboxname) pNone

delete :: IMAPConnection -> MailboxName -> IO ()
delete conn mboxname = sendCommand conn ("DELETE " ++ mboxname) pNone

rename :: IMAPConnection -> MailboxName -> MailboxName -> IO ()
rename conn mboxorg mboxnew =
    sendCommand conn ("RENAME " ++ mboxorg ++ " " ++ mboxnew) pNone

subscribe :: IMAPConnection -> MailboxName -> IO ()
subscribe conn mboxname = sendCommand conn ("SUBSCRIBE " ++ mboxname) pNone

unsubscribe :: IMAPConnection -> MailboxName -> IO ()
unsubscribe conn mboxname = sendCommand conn ("UNSUBSCRIBE " ++ mboxname) pNone

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
    let cmd = "STATUS " ++ mbox ++ " (" ++ (unwords $ map show stats) ++ ")"
    in sendCommand conn cmd pStatus

append :: IMAPConnection -> MailboxName -> ByteString -> IO ()
append conn mbox mailData = appendFull conn mbox mailData Nothing Nothing

appendFull :: IMAPConnection -> MailboxName -> ByteString
           -> Maybe [Flag] -> Maybe CalendarTime -> IO ()
appendFull conn mbox mailData flags' time =
    do (buf, num) <- sendCommand' conn
                (concat ["APPEND ", mbox
                        , fstr, tstr, " {" ++ show len ++ "}"])
       when (BS.null buf || (BS.head buf /= '+')) $
              fail "illegal server response"
       mapM_ (bsPutCrLf $ stream conn) mailLines
       bsPutCrLf (stream conn) BS.empty
       buf2 <- getResponse $ stream conn
       let (resp, mboxUp, ()) = eval pNone (show6 num) buf2
       case resp of
         OK _ _ -> mboxUpdate conn mboxUp
         NO _ msg -> fail ("NO: "++msg)
         BAD _ msg -> fail ("BAD: "++msg)
         PREAUTH _ msg -> fail ("PREAUTH: "++msg)
    where mailLines = BS.lines mailData
          len       = sum $ map ((2+) . BS.length) mailLines
          tstr      = maybe "" ((" "++) . show) time
          fstr      = maybe "" ((" ("++) . (++")") . unwords . map show) flags'

check :: IMAPConnection -> IO ()
check conn = sendCommand conn "CHECK" pNone

close :: IMAPConnection -> IO ()
close conn =
    do sendCommand conn "CLOSE" pNone
       setMailboxInfo conn emptyMboxInfo

expunge :: IMAPConnection -> IO [Integer]
expunge conn = sendCommand conn "EXPUNGE" pExpunge

search :: IMAPConnection -> [SearchQuery] -> IO [UID]
search conn queries = searchCharset conn "" queries

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
    do lst <- fetchByString conn uid "BODY[]"
       return $ maybe BS.empty BS.pack $ lookup' "BODY[]" lst

fetchHeader :: IMAPConnection -> UID -> IO ByteString
fetchHeader conn uid =
    do lst <- fetchByString conn uid "BODY[HEADER]"
       return $ maybe BS.empty BS.pack $ lookup' "BODY[HEADER]" lst

fetchSize :: IMAPConnection -> UID -> IO Int
fetchSize conn uid =
    do lst <- fetchByString conn uid "RFC822.SIZE"
       return $ maybe 0 read $ lookup' "RFC822.SIZE" lst

fetchHeaderFields :: IMAPConnection
                  -> UID -> [String] -> IO ByteString
fetchHeaderFields conn uid hs =
    do lst <- fetchByString conn uid ("BODY[HEADER.FIELDS "++unwords hs++"]")
       return $ maybe BS.empty BS.pack $
              lookup' ("BODY[HEADER.FIELDS "++unwords hs++"]") lst

fetchHeaderFieldsNot :: IMAPConnection
                     -> UID -> [String] -> IO ByteString
fetchHeaderFieldsNot conn uid hs =
    do let fetchCmd = "BODY[HEADER.FIELDS.NOT "++unwords hs++"]"
       lst <- fetchByString conn uid fetchCmd
       return $ maybe BS.empty BS.pack $ lookup' fetchCmd lst

fetchFlags :: IMAPConnection -> UID -> IO [Flag]
fetchFlags conn uid =
    do lst <- fetchByString conn uid "FLAGS"
       return $ getFlags $ lookup' "FLAGS" lst
    where getFlags Nothing  = []
          getFlags (Just s) = eval' dvFlags "" s

fetchR :: IMAPConnection -> (UID, UID)
       -> IO [(UID, ByteString)]
fetchR conn r =
    do lst <- fetchByStringR conn r "BODY[]"
       return $ map (\(uid, vs) -> (uid, maybe BS.empty BS.pack $
                                       lookup' "BODY[]" vs)) lst
fetchByString :: IMAPConnection -> UID -> String
              -> IO [(String, String)]
fetchByString conn uid command =
    do lst <- fetchCommand conn ("UID FETCH "++show uid++" "++command) id
       return $ snd $ head lst

fetchByStringR :: IMAPConnection -> (UID, UID) -> String
               -> IO [(UID, [(String, String)])]
fetchByStringR conn (s, e) command =
    fetchCommand conn ("UID FETCH "++show s++":"++show e++" "++command) proc
    where proc (n, ps) =
              (maybe (toEnum (fromIntegral n)) read (lookup' "UID" ps), ps)

fetchCommand :: IMAPConnection -> String
             -> ((Integer, [(String, String)]) -> b) -> IO [b]
fetchCommand conn command proc =
    (map proc) <$> sendCommand conn command pFetch

storeFull :: IMAPConnection -> String -> FlagsQuery -> Bool
          -> IO [(UID, [Flag])]
storeFull conn uidstr query isSilent =
    fetchCommand conn ("UID STORE " ++ uidstr ++ " " ++ flgs query) procStore
    where fstrs fs = "(" ++ (concat $ intersperse " " $ map show fs) ++ ")"
          toFStr s fstrs' =
              s ++ (if isSilent then ".SILENT" else "") ++ " " ++ fstrs'
          flgs (ReplaceFlags fs) = toFStr "FLAGS" $ fstrs fs
          flgs (PlusFlags fs)    = toFStr "+FLAGS" $ fstrs fs
          flgs (MinusFlags fs)   = toFStr "-FLAGS" $ fstrs fs
          procStore (n, ps) = (maybe (toEnum (fromIntegral n)) read
                                         (lookup' "UID" ps)
                              ,maybe [] (eval' dvFlags "") (lookup' "FLAG" ps))


store :: IMAPConnection -> UID -> FlagsQuery -> IO ()
store conn i q = storeFull conn (show i) q True >> return ()

copyFull :: IMAPConnection -> String -> String -> IO ()
copyFull conn uidStr mbox =
    sendCommand conn ("UID COPY " ++ uidStr ++ " " ++ mbox) pNone

copy :: IMAPConnection -> UID -> MailboxName -> IO ()
copy conn uid mbox     = copyFull conn (show uid) mbox

----------------------------------------------------------------------
-- auxialiary functions

dateToStringIMAP :: CalendarTime -> String
dateToStringIMAP date = concat $ intersperse "-" [show2 $ ctDay date
                                                 , showMonth $ ctMonth date
                                                 , show $ ctYear date]
    where show2 n | n < 10    = '0' : show n
                  | otherwise = show n
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

strip :: ByteString -> ByteString
strip = fst . BS.spanEnd isSpace . BS.dropWhile isSpace

crlf :: BS.ByteString
crlf = BS.pack "\r\n"

bsPutCrLf :: BSStream -> ByteString -> IO ()
bsPutCrLf h s = bsPut h s >> bsPut h crlf >> bsFlush h

lookup' :: String -> [(String, b)] -> Maybe b
lookup' _ [] = Nothing
lookup' q ((k,v):xs) | q == lastWord k  = return v
                     | otherwise        = lookup' q xs
    where
        lastWord = last . words

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
