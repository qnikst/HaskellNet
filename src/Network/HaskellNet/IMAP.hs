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
      -- * fetch commands
    , fetch, fetchHeader, fetchSize, fetchHeaderFields, fetchHeaderFieldsNot
    , fetchFlags, fetchR, fetchByString, fetchByStringR
      -- * other types
    , Flag(..), Attribute(..), MailboxStatus(..)
    , SearchQuery(..), FlagsQuery(..)
    )
where

import Network
import Network.HaskellNet.BSStream
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.Types
import Network.HaskellNet.IMAP.Parsers
import qualified Network.HaskellNet.Auth as A

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Control.Applicative ((<$>))
import Control.Monad

import System.IO
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

connectIMAPPort :: String -> PortNumber -> IO (IMAPConnection Handle)
connectIMAPPort hostname port =
    connectTo hostname (PortNumber port) >>= connectStream

connectIMAP :: String -> IO (IMAPConnection Handle)
connectIMAP hostname = connectIMAPPort hostname 143

connectStream :: BSStream s => s -> IO (IMAPConnection s)
connectStream s =
    do msg <- bsGetLine s
       unless (and $ BS.zipWith (==) msg (BS.pack "* OK")) $
              fail "cannot connect to the server"
       newConnection s

----------------------------------------------------------------------
-- normal send commands
sendCommand' :: (BSStream s) => IMAPConnection s -> String -> IO (ByteString, Int)
sendCommand' c cmdstr = do
  (_, num) <- withNextCommandNum c $ \num -> bsPutCrLf c $ BS.pack $ show6 num ++ " " ++ cmdstr
  resp <- getResponse c
  return (resp, num)

show6 :: (Ord a, Num a) => a -> String
show6 n | n > 100000 = show n
        | n > 10000  = '0' : show n
        | n > 1000   = "00" ++ show n
        | n > 100    = "000" ++ show n
        | n > 10     = "0000" ++ show n
        | otherwise  = "00000" ++ show n

sendCommand :: BSStream s => IMAPConnection s -> String
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

getResponse :: BSStream s => s -> IO ByteString
getResponse s = unlinesCRLF <$> getLs
    where unlinesCRLF = BS.concat . concatMap (:[crlfStr])
          getLs =
              do l <- strip <$> bsGetLine s
                 case () of
                   _ | isLiteral l ->  do l' <- getLiteral l (getLitLen l)
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

mboxUpdate :: IMAPConnection s -> MboxUpdate -> IO ()
mboxUpdate conn (MboxUpdate exists' recent') = do
  when (isJust exists') $
       modifyMailboxInfo conn $ \mbox -> mbox { _exists = fromJust exists' }

  when (isJust recent') $
       modifyMailboxInfo conn $ \mbox -> mbox { _recent = fromJust recent' }

----------------------------------------------------------------------
-- IMAP commands
--

noop :: BSStream s => IMAPConnection s -> IO ()
noop conn = sendCommand conn "NOOP" pNone

capability :: BSStream s => IMAPConnection s -> IO [String]
capability conn = sendCommand conn "CAPABILITY" pCapability

logout :: (BSStream s) => IMAPConnection s -> IO ()
logout c = do bsPutCrLf c $ BS.pack "a0001 LOGOUT"
              bsClose c

login :: BSStream s => IMAPConnection s -> A.UserName -> A.Password -> IO ()
login conn username password = sendCommand conn ("LOGIN " ++ username ++ " " ++ password)
                               pNone

authenticate :: (BSStream s) => IMAPConnection s -> A.AuthType
             -> A.UserName -> A.Password -> IO ()
authenticate conn A.LOGIN username password =
    do (_, num) <- sendCommand' conn "AUTHENTICATE LOGIN"
       bsPutCrLf conn $ BS.pack userB64
       bsGetLine conn
       bsPutCrLf conn $ BS.pack passB64
       buf <- getResponse conn
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
       bsPutCrLf conn $ BS.pack $ A.auth at challenge username password
       buf <- getResponse conn
       let (resp, mboxUp, value) = eval pNone (show6 num) buf
       case resp of
         OK _ _        -> do mboxUpdate conn $ mboxUp
                             return value
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)

_select :: (BSStream s) => String -> IMAPConnection s -> String -> IO ()
_select cmd conn mboxName =
    do mbox' <- sendCommand conn (cmd ++ mboxName) pSelect
       setMailboxInfo conn $ mbox' { _mailbox = mboxName }

select :: BSStream s => IMAPConnection s -> Mailbox -> IO ()
select = _select "SELECT "

examine :: BSStream s => IMAPConnection s -> Mailbox -> IO ()
examine = _select "EXAMINE "

create :: BSStream s => IMAPConnection s -> Mailbox -> IO ()
create conn mboxname = sendCommand conn ("CREATE " ++ mboxname) pNone

delete :: BSStream s => IMAPConnection s -> Mailbox -> IO ()
delete conn mboxname = sendCommand conn ("DELETE " ++ mboxname) pNone

rename :: BSStream s => IMAPConnection s -> Mailbox -> Mailbox -> IO ()
rename conn mboxorg mboxnew =
    sendCommand conn ("RENAME " ++ mboxorg ++ " " ++ mboxnew) pNone

subscribe :: BSStream s => IMAPConnection s -> Mailbox -> IO ()
subscribe conn mboxname = sendCommand conn ("SUBSCRIBE " ++ mboxname) pNone

unsubscribe :: BSStream s => IMAPConnection s -> Mailbox -> IO ()
unsubscribe conn mboxname = sendCommand conn ("UNSUBSCRIBE " ++ mboxname) pNone

list :: BSStream s => IMAPConnection s -> IO [([Attribute], Mailbox)]
list conn = (map (\(a, _, m) -> (a, m))) <$> listFull conn "\"\"" "*"

lsub :: BSStream s => IMAPConnection s -> IO [([Attribute], Mailbox)]
lsub conn = (map (\(a, _, m) -> (a, m))) <$> lsubFull conn "\"\"" "*"

listFull :: BSStream s => IMAPConnection s -> String -> String
         -> IO [([Attribute], String, Mailbox)]
listFull conn ref pat = sendCommand conn (unwords ["LIST", ref, pat]) pList

lsubFull :: BSStream s => IMAPConnection s -> String -> String
         -> IO [([Attribute], String, Mailbox)]
lsubFull conn ref pat = sendCommand conn (unwords ["LSUB", ref, pat]) pLsub

status :: BSStream s => IMAPConnection s -> Mailbox -> [MailboxStatus]
       -> IO [(MailboxStatus, Integer)]
status conn mbox stats =
    let cmd = "STATUS " ++ mbox ++ " (" ++ (unwords $ map show stats) ++ ")"
    in sendCommand conn cmd pStatus

append :: BSStream s => IMAPConnection s -> Mailbox -> ByteString -> IO ()
append conn mbox mailData = appendFull conn mbox mailData [] Nothing

appendFull :: BSStream s => IMAPConnection s -> Mailbox -> ByteString
           -> [Flag] -> Maybe CalendarTime -> IO ()
appendFull conn mbox mailData flags' time =
    do (buf, num) <- sendCommand' conn
                (unwords ["APPEND", mbox
                         , fstr, tstr,  "{" ++ show len ++ "}"])
       unless (BS.null buf || (BS.head buf /= '+')) $
              fail "illegal server response"
       mapM_ (bsPutCrLf conn) mailLines
       buf2 <- getResponse conn
       let (resp, mboxUp, ()) = eval pNone (show6 num) buf2
       case resp of
         OK _ _ -> mboxUpdate conn mboxUp
         NO _ msg -> fail ("NO: "++msg)
         BAD _ msg -> fail ("BAD: "++msg)
         PREAUTH _ msg -> fail ("PREAUTH: "++msg)
    where mailLines = BS.lines mailData
          len       = sum $ map ((2+) . BS.length) mailLines
          tstr      = maybe "" show time
          fstr      = unwords $ map show flags'

check :: BSStream s => IMAPConnection s -> IO ()
check conn = sendCommand conn "CHECK" pNone

close :: BSStream s => IMAPConnection s -> IO ()
close conn =
    do sendCommand conn "CLOSE" pNone
       setMailboxInfo conn emptyMboxInfo

expunge :: BSStream s => IMAPConnection s -> IO [Integer]
expunge conn = sendCommand conn "EXPUNGE" pExpunge

search :: BSStream s => IMAPConnection s -> [SearchQuery] -> IO [UID]
search conn queries = searchCharset conn "" queries

searchCharset :: BSStream s => IMAPConnection s -> Charset -> [SearchQuery]
              -> IO [UID]
searchCharset conn charset queries =
    sendCommand conn ("UID SEARCH "
                    ++ (if not . null $ charset
                           then charset ++ " "
                           else "")
                    ++ unwords (map show queries)) pSearch

fetch :: BSStream s => IMAPConnection s -> UID -> IO ByteString
fetch conn uid =
    do lst <- fetchByString conn uid "BODY[]"
       return $ maybe BS.empty BS.pack $ lookup "BODY[]" lst

fetchHeader :: BSStream s => IMAPConnection s -> UID -> IO ByteString
fetchHeader conn uid =
    do lst <- fetchByString conn uid "BODY[HEADER]"
       return $ maybe BS.empty BS.pack $ lookup "BODY[HEADER]" lst

fetchSize :: BSStream s => IMAPConnection s -> UID -> IO Int
fetchSize conn uid =
    do lst <- fetchByString conn uid "RFC822.SIZE"
       return $ maybe 0 read $ lookup "RFC822.SIZE" lst

fetchHeaderFields :: BSStream s => IMAPConnection s
                  -> UID -> [String] -> IO ByteString
fetchHeaderFields conn uid hs =
    do lst <- fetchByString conn uid ("BODY[HEADER.FIELDS "++unwords hs++"]")
       return $ maybe BS.empty BS.pack $
              lookup ("BODY[HEADER.FIELDS "++unwords hs++"]") lst

fetchHeaderFieldsNot :: BSStream s => IMAPConnection s
                     -> UID -> [String] -> IO ByteString
fetchHeaderFieldsNot conn uid hs =
    do let fetchCmd = "BODY[HEADER.FIELDS.NOT "++unwords hs++"]"
       lst <- fetchByString conn uid fetchCmd
       return $ maybe BS.empty BS.pack $ lookup fetchCmd lst

fetchFlags :: BSStream s => IMAPConnection s -> UID -> IO [Flag]
fetchFlags conn uid =
    do lst <- fetchByString conn uid "FLAGS"
       return $ getFlags $ lookup "FLAGS" lst
    where getFlags Nothing  = []
          getFlags (Just s) = eval' dvFlags "" s

fetchR :: BSStream s => IMAPConnection s -> (UID, UID)
       -> IO [(UID, ByteString)]
fetchR conn r =
    do lst <- fetchByStringR conn r "BODY[]"
       return $ map (\(uid, vs) -> (uid, maybe BS.empty BS.pack $
                                       lookup "BODY[]" vs)) lst
fetchByString :: BSStream s => IMAPConnection s -> UID -> String
              -> IO [(String, String)]
fetchByString conn uid command =
    do lst <- fetchCommand conn ("UID FETCH "++show uid++" "++command) id
       return $ snd $ head lst

fetchByStringR :: BSStream s => IMAPConnection s -> (UID, UID) -> String
               -> IO [(UID, [(String, String)])]
fetchByStringR conn (s, e) command =
    fetchCommand conn ("UID FETCH "++show s++":"++show e++" "++command) proc
    where proc (n, ps) =
              (maybe (toEnum (fromIntegral n)) read (lookup "UID" ps), ps)

fetchCommand :: (BSStream s) => IMAPConnection s -> String
             -> ((Integer, [(String, String)]) -> b) -> IO [b]
fetchCommand conn command proc =
    (map proc) <$> sendCommand conn command pFetch

storeFull :: BSStream s => IMAPConnection s -> String -> FlagsQuery -> Bool
          -> IO [(UID, [Flag])]
storeFull conn uidstr query isSilent =
    fetchCommand conn ("UID STORE " ++ uidstr ++ flgs query) procStore
    where fstrs fs = "(" ++ (concat $ intersperse " " $ map show fs) ++ ")"
          toFStr s fstrs' =
              s ++ (if isSilent then ".SILENT" else "") ++ " " ++ fstrs'
          flgs (ReplaceFlags fs) = toFStr "FLAGS" $ fstrs fs
          flgs (PlusFlags fs)    = toFStr "+FLAGS" $ fstrs fs
          flgs (MinusFlags fs)   = toFStr "-FLAGS" $ fstrs fs
          procStore (n, ps) = (maybe (toEnum (fromIntegral n)) read
                                         (lookup "UID" ps)
                              ,maybe [] (eval' dvFlags "") (lookup "FLAG" ps))


store :: BSStream s => IMAPConnection s -> UID -> FlagsQuery -> IO ()
store conn i q       = storeFull conn (show i) q True >> return ()

copyFull :: (BSStream s) => IMAPConnection s -> String -> String -> IO ()
copyFull conn uidStr mbox =
    sendCommand conn ("UID COPY " ++ uidStr ++ " " ++ mbox) pNone

copy :: BSStream s => IMAPConnection s -> UID -> Mailbox -> IO ()
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
