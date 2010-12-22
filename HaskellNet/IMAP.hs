----------------------------------------------------------------------
-- |
-- Module      :  HaskellNet.IMAP
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  stable
-- Portability :  portable
-- 
-- IMAP client implementation
-- 

module HaskellNet.IMAP
    ( -- * connection type and corresponding actions
      IMAPConnection
    , mailbox, exists, recent
    , flags, permanentFlags, isWritable, isFlagWritable
    , uidNext, uidValidity
    , stream
    , connectIMAP, connectIMAPPort, connectStream
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
import HaskellNet.BSStream
import HaskellNet.Auth hiding (auth, login)
import qualified HaskellNet.Auth as A

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Data.Digest.MD5
import Control.Monad
import Control.Monad.Writer

import System.IO
import System.Time

import Data.IORef
import Data.Maybe
import Data.Word
import Data.List hiding (delete)
import Data.Char

import Text.IMAPParsers hiding (exists, recent)
import Text.Packrat.Parse (Result)


----------------------------------------------------------------------
-- connection type and corresponding functions
data BSStream s => IMAPConnection s = 
    IMAPC s (IORef MailboxInfo) (IORef Int)

mailbox :: BSStream s => IMAPConnection s -> IO Mailbox
mailbox (IMAPC _ mbox _) = fmap _mailbox $ readIORef mbox
exists, recent :: BSStream s => IMAPConnection s -> IO Integer
exists (IMAPC _ mbox _) = fmap _exists $ readIORef mbox
recent (IMAPC _ mbox _) = fmap _recent $ readIORef mbox

flags, permanentFlags :: BSStream s => IMAPConnection s -> IO [Flag]
flags (IMAPC _ mbox _) = fmap _flags $ readIORef mbox
permanentFlags (IMAPC _ mbox _) = fmap _permanentFlags $ readIORef mbox

isWritable, isFlagWritable :: BSStream s => IMAPConnection s -> IO Bool
isWritable (IMAPC _ mbox _) = fmap _isWritable $ readIORef mbox
isFlagWritable (IMAPC _ mbox _) = fmap _isFlagWritable $ readIORef mbox

uidNext, uidValidity :: BSStream s => IMAPConnection s -> IO UID
uidNext (IMAPC _ mbox _) = fmap _uidNext $ readIORef mbox
uidValidity (IMAPC _ mbox _) = fmap _uidValidity $ readIORef mbox

stream :: BSStream s => IMAPConnection s -> s
stream (IMAPC s _ _) = s




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
              showQuery (NOTs q)        = "NOT " ++ show q
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
              showQuery (UIDs uids)     = concat $ intersperse "," $ map show uids
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
connectIMAPPort hostname port = connectTo hostname (PortNumber port) >>= connectStream

connectIMAP :: String -> IO (IMAPConnection Handle)
connectIMAP hostname = connectIMAPPort hostname 143

connectStream :: BSStream s => s -> IO (IMAPConnection s)
connectStream s =
    do msg <- bsGetLine s
       unless (and $ BS.zipWith (==) msg (BS.pack "* OK")) $ fail "cannot connect to the server"
       mbox <- newIORef emptyMboxInfo
       c <- newIORef 0
       return $ IMAPC s mbox c

emptyMboxInfo = MboxInfo "" 0 0 [] [] False False 0 0

----------------------------------------------------------------------
-- normal send commands
sendCommand' :: BSStream s => IMAPConnection s -> String -> IO ByteString
sendCommand' (IMAPC s mbox nr) cmdstr =
    do num <- readIORef nr 
       bsPutCrLf s $ BS.pack $ show6 num ++ " " ++ cmdstr
       modifyIORef nr (+1)
       getResponse s

show6 n | n > 100000 = show n
        | n > 10000  = '0' : show n
        | n > 1000   = "00" ++ show n
        | n > 100    = "000" ++ show n
        | n > 10     = "0000" ++ show n
        | otherwise  = "00000" ++ show n

sendCommand :: BSStream s => IMAPConnection s -> String -> (RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, v)) -> IO v
sendCommand imapc@(IMAPC _ mbox nr) cmdstr pFunc =
    do num <- readIORef nr
       buf <- sendCommand' imapc cmdstr
       let (resp, mboxUp, value) = eval pFunc (show6 num) buf
       case resp of
         OK _ _        -> do mboxUpdate mbox $ mboxUp
                             return value
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)

getResponse :: BSStream s => s -> IO ByteString
getResponse s = fmap unlinesCRLF getLs
    where unlinesCRLF = BS.concat . concatMap (:[crlf]) 
          getLs = 
              do l <- fmap strip $ bsGetLine s
                 case () of
                   _ | isLiteral l ->  do l' <- getLiteral l (getLitLen l)
                                          ls <- getLs
                                          return (l' : ls)
                     | isTagged l -> fmap (l:) getLs
                     | otherwise -> return [l]
          getLiteral l len = 
              do lit <- bsGet s len
                 l2 <- fmap strip $ bsGetLine s
                 let l' = BS.concat [l, crlf, lit, l2]
                 if isLiteral l2
                   then getLiteral l' (getLitLen l2)
                   else return l'
          crlf = BS.pack "\r\n"
          isLiteral l = BS.last l == '}' && BS.last (fst (BS.spanEnd isDigit (BS.init l))) == '{'
          getLitLen = read . BS.unpack . snd . BS.spanEnd isDigit . BS.init
          isTagged l = BS.head l == '*' && BS.head (BS.tail l) == ' '

mboxUpdate :: IORef MailboxInfo -> MboxUpdate -> IO ()
mboxUpdate mbox (MboxUpdate exists recent) =
    do when (isJust exists) $ do mb <- readIORef mbox
                                 writeIORef mbox (mb { _exists = e })
       when (isJust recent) $ do mb <- readIORef mbox
                                 writeIORef mbox (mb { _recent = r })
    where e = fromJust exists
          r = fromJust recent

----------------------------------------------------------------------
-- IMAP commands
-- 

noop :: BSStream s => IMAPConnection s -> IO ()
noop conn@(IMAPC s mbox _) = sendCommand conn "NOOP" pNone

capability :: BSStream s => IMAPConnection s -> IO [String]
capability conn = sendCommand conn "CAPABILITY" pCapability

logout :: BSStream s => IMAPConnection s -> IO ()
logout conn@(IMAPC s _ _) = do bsPutCrLf s $ BS.pack "a0001 LOGOUT"
                               bsClose s

login :: BSStream s => IMAPConnection s -> UserName -> Password -> IO ()
login conn user pass = sendCommand conn ("LOGIN " ++ user ++ " " ++ pass) pNone

select, examine, create, delete :: BSStream s =>
                                   IMAPConnection s -> Mailbox -> IO ()
_select cmd conn@(IMAPC s mbox _) mboxName =
    do mbox' <- sendCommand conn (cmd ++ mboxName) pSelect
       writeIORef mbox (mbox' { _mailbox = mboxName })

authenticate :: BSStream s => IMAPConnection s -> AuthType -> UserName -> Password -> IO ()
authenticate conn@(IMAPC s mbox nr) LOGIN user pass =
    do num <- readIORef nr
       sendCommand' conn "AUTHENTICATE LOGIN"
       bsPutCrLf s $ BS.pack userB64
       bsGetLine s
       bsPutCrLf s $ BS.pack passB64
       buf <- getResponse s
       let (resp, mboxUp, value) = eval pNone (show6 num) buf
       case resp of
         OK _ _        -> do mboxUpdate mbox $ mboxUp
                             return value
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)
    where (userB64, passB64) = A.login user pass
authenticate conn@(IMAPC s mbox nr) at user pass =
    do num <- readIORef nr
       c <- sendCommand' conn $ "AUTHENTICATE " ++ show at
       let challenge =
               if BS.take 2 c == BS.pack "+ "
               then b64Decode $ BS.unpack $ head $ dropWhile (isSpace . BS.last) $ BS.inits $ BS.drop 2 c
               else ""
       bsPutCrLf s $ BS.pack $ A.auth at challenge user pass
       buf <- getResponse s
       let (resp, mboxUp, value) = eval pNone (show6 num) buf
       case resp of
         OK _ _        -> do mboxUpdate mbox $ mboxUp
                             return value
         NO _ msg      -> fail ("NO: " ++ msg)
         BAD _ msg     -> fail ("BAD: " ++ msg)
         PREAUTH _ msg -> fail ("preauth: " ++ msg)

select = _select "SELECT "
examine = _select "EXAMINE "
create conn mboxname = sendCommand conn ("CREATE " ++ mboxname) pNone
delete conn mboxname = sendCommand conn ("DELETE " ++ mboxname) pNone

rename :: BSStream s => IMAPConnection s -> Mailbox -> Mailbox -> IO ()
rename conn mboxorg mboxnew =
    sendCommand conn ("RENAME " ++ mboxorg ++ " " ++ mboxnew) pNone

subscribe, unsubscribe :: BSStream s => IMAPConnection s -> Mailbox -> IO ()
subscribe conn mboxname = sendCommand conn ("SUBSCRIBE " ++ mboxname) pNone
unsubscribe conn mboxname = sendCommand conn ("UNSUBSCRIBE " ++ mboxname) pNone

list, lsub :: BSStream s => IMAPConnection s -> IO [([Attribute], Mailbox)]
list conn = fmap (map (\(a, _, m) -> (a, m))) $ listFull conn "\"\"" "*"
lsub conn = fmap (map (\(a, _, m) -> (a, m))) $ lsubFull conn "\"\"" "*"

listPat, lsubPat :: BSStream s => IMAPConnection s -> String -> IO [([Attribute], String, Mailbox)]
listPat conn pat = listFull conn "\"\"" pat
lsubPat conn pat = lsubFull conn "\"\"" pat

listFull, lsubFull :: BSStream s => IMAPConnection s -> String -> String -> IO [([Attribute], String, Mailbox)]
listFull conn ref pat = sendCommand conn (unwords ["LIST", ref, pat]) pList
lsubFull conn ref pat = sendCommand conn (unwords ["LSUB", ref, pat]) pLsub

status :: BSStream s => IMAPConnection s -> Mailbox -> [MailboxStatus] -> IO [(MailboxStatus, Integer)]
status conn mbox stats =
    sendCommand conn ("STATUS " ++ mbox ++ " (" ++ (unwords $ map show stats) ++ ")") pStatus

append :: BSStream s => IMAPConnection s -> Mailbox -> ByteString -> IO ()
append conn mbox mailData = appendFull conn mbox mailData [] Nothing

appendFull :: BSStream s => IMAPConnection s -> Mailbox -> ByteString -> [Flag] -> Maybe CalendarTime -> IO ()
appendFull conn@(IMAPC s mbInfo nr) mbox mailData flags time = 
    do num <- readIORef nr
       buf <- sendCommand' conn
                (unwords ["APPEND", mbox
                         , fstr, tstr,  "{" ++ show len ++ "}"])
       unless (BS.null buf || (BS.head buf /= '+')) $ fail "illegal server response"
       mapM_ (bsPutCrLf s) mailLines
       buf <- getResponse s
       let (resp, mboxUp, ()) = eval pNone (show6 num) buf
       case resp of
         OK _ _ -> mboxUpdate mbInfo mboxUp
         NO _ msg -> fail ("NO: "++msg)
         BAD _ msg -> fail ("BAD: "++msg)
         PREAUTH _ msg -> fail ("PREAUTH: "++msg)
    where mailLines = BS.lines mailData
          len       = sum $ map ((2+) . BS.length) mailLines
          tstr      = maybe "" show time
          fstr      = unwords $ map show flags

check :: BSStream s => IMAPConnection s -> IO ()
check conn = sendCommand conn "CHECK" pNone

close :: BSStream s => IMAPConnection s -> IO ()
close conn@(IMAPC s mbox _) =
    do sendCommand conn "CLOSE" pNone
       writeIORef mbox emptyMboxInfo

expunge :: BSStream s => IMAPConnection s -> IO [Integer]
expunge conn = sendCommand conn "EXPUNGE" pExpunge

search :: BSStream s => IMAPConnection s -> [SearchQuery] -> IO [UID]
search conn queries = searchCharset conn "" queries

searchCharset :: BSStream s => IMAPConnection s -> Charset -> [SearchQuery] -> IO [UID]
searchCharset conn charset queries =
    sendCommand conn ("UID SEARCH " 
                    ++ (if not . null $ charset 
                           then charset ++ " " 
                           else "") 
                    ++ unwords (map show queries)) pSearch

fetch, fetchHeader :: BSStream s => IMAPConnection s -> UID -> IO ByteString
fetch conn uid =
    do lst <- fetchByString conn uid "BODY[]"
       return $ maybe BS.empty BS.pack $ lookup "BODY[]" lst
fetchHeader conn uid =
    do lst <- fetchByString conn uid "BODY[HEADER]"
       return $ maybe BS.empty BS.pack $ lookup "BODY[HEADER]" lst
fetchSize :: BSStream s => IMAPConnection s -> UID -> IO Int
fetchSize conn uid =
    do lst <- fetchByString conn uid "RFC822.SIZE"
       return $ maybe 0 read $ lookup "RFC822.SIZE" lst
fetchHeaderFields, fetchHeaderFieldsNot :: BSStream s => IMAPConnection s -> UID -> [String] -> IO ByteString
fetchHeaderFields conn uid hs =
    do lst <- fetchByString conn uid ("BODY[HEADER.FIELDS "++unwords hs++"]")
       return $ maybe BS.empty BS.pack $
              lookup ("BODY[HEADER.FIELDS "++unwords hs++"]") lst
fetchHeaderFieldsNot conn uid hs = 
    do lst <- fetchByString conn uid ("BODY[HEADER.FIELDS.NOT "++unwords hs++"]")
       return $ maybe BS.empty BS.pack $ lookup ("BODY[HEADER.FIELDS.NOT "++unwords hs++"]") lst
fetchFlags :: BSStream s => IMAPConnection s -> UID -> IO [Flag]
fetchFlags conn uid =
    do lst <- fetchByString conn uid "FLAGS"
       return $ getFlags $ lookup "FLAGS" lst
    where getFlags Nothing  = []
          getFlags (Just s) = eval' dvFlags "" s

fetchR :: BSStream s => IMAPConnection s -> (UID, UID) -> IO [(UID, ByteString)]
fetchR conn r =
    do lst <- fetchByStringR conn r "BODY[]"
       return $ map (\(uid, vs) -> (uid, maybe BS.empty BS.pack $ lookup "BODY[]" vs)) lst
fetchByString :: BSStream s => IMAPConnection s -> UID -> String -> IO [(String, String)]
fetchByString conn uid command =
    do lst <- fetchCommand conn ("UID FETCH "++show uid++" "++command) id
       return $ snd $ head lst
fetchByStringR :: BSStream s => IMAPConnection s -> (UID, UID) -> String -> IO [(UID, [(String, String)])]
fetchByStringR conn (s, e) command =
    fetchCommand conn ("UID FETCH "++show s++":"++show e++" "++command) proc
    where proc (n, ps) =
              (maybe (toEnum (fromIntegral n)) read (lookup "UID" ps), ps)

fetchCommand conn command proc =
    fmap (map proc) $ sendCommand conn command pFetch

storeFull :: BSStream s => IMAPConnection s -> String -> FlagsQuery -> Bool -> IO [(UID, [Flag])]
storeFull conn uidstr query isSilent =
    fetchCommand conn ("UID STORE " ++ uidstr ++ flags query) procStore
    where fstrs fs = "(" ++ (concat $ intersperse " " $ map show fs) ++ ")"
          toFStr s fstrs =
              s ++ (if isSilent then ".SILENT" else "") ++ " " ++ fstrs
          flags (ReplaceFlags fs) = toFStr "FLAGS" $ fstrs fs
          flags (PlusFlags fs)    = toFStr "+FLAGS" $ fstrs fs
          flags (MinusFlags fs)   = toFStr "-FLAGS" $ fstrs fs
          procStore (n, ps) = (maybe (toEnum (fromIntegral n)) read
                                         (lookup "UID" ps)
                              ,maybe [] (eval' dvFlags "") (lookup "FLAG" ps))


store :: BSStream s => IMAPConnection s -> UID -> FlagsQuery -> IO ()
storeR :: BSStream s => IMAPConnection s -> (UID, UID) -> FlagsQuery -> IO ()
store conn i q       = storeFull conn (show i) q True >> return ()
storeR conn (s, e) q = storeFull conn (show s++":"++show e) q True >> return ()
-- storeResults is used without .SILENT, so that its response contains its result flags
storeResults :: BSStream s => IMAPConnection s -> UID -> FlagsQuery -> IO [Flag]
storeResultsR :: BSStream s => IMAPConnection s -> (UID, UID) -> FlagsQuery -> IO [(UID, [Flag])]
storeResults conn i q       =
    storeFull conn (show i) q False >>= return . snd . head
storeResultsR conn (s, e) q = storeFull conn (show s++":"++show e) q False

copy :: BSStream s => IMAPConnection s -> UID -> Mailbox -> IO ()
copyR :: BSStream s => IMAPConnection s -> (UID, UID) -> Mailbox -> IO ()
copyFull conn uidStr mbox =
    sendCommand conn ("UID COPY " ++ uidStr ++ " " ++ mbox) pNone

copy conn uid mbox     = copyFull conn (show uid) mbox
copyR conn (s, e) mbox = copyFull conn (show s++":"++show e) mbox


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
