----------------------------------------------------------------------
-- |
-- Module      :  HaskellNet.IMAP
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  implementing
-- Portability :  portable
-- 
-- IMAP client implementation
-- 

module HaskellNet.IMAP
where

import Network (PortNumber)
import HaskellNet.Stream hiding (close)
import qualified HaskellNet.Stream as S (close)
import HaskellNet.TCP

import Data.Digest.MD5
import Control.Monad

import System.IO
import System.Time

import Text.ParserCombinators.Parsec hiding (space)

import Data.IORef
import Data.Word
import Data.Maybe
import Data.List

type Mailbox = String
type UID = Word64
data MailboxInfo = MboxInfo { _mailbox :: Mailbox
                            , _exists :: Integer
                            , _recent :: Integer
                            , _flags :: [Flag]
                            , _permanentFlags :: [Flag]
                            , _isWritable :: Bool
                            , _isFlagWritable :: Bool
                            , _uidNext :: UID
                            , _uidValidity :: UID
                            } 


data Stream s => IMAPConnection s = 
    IMAPC s (IORef MailboxInfo) (IORef Int)

mailbox :: Stream s => IMAPConnection s -> IO Mailbox
mailbox (IMAPC _ mbox _) = fmap _mailbox $ readIORef mbox
exists, recent :: Stream s => IMAPConnection s -> IO Integer
exists (IMAPC _ mbox _) = fmap _exists $ readIORef mbox
recent (IMAPC _ mbox _) = fmap _recent $ readIORef mbox

flags, permanentFlags :: Stream s => IMAPConnection s -> IO [Flag]
flags (IMAPC _ mbox _) = fmap _flags $ readIORef mbox
permanentFlags (IMAPC _ mbox _) = fmap _permanentFlags $ readIORef mbox

isWritable, isFlagWritable :: Stream s => IMAPConnection s -> IO Bool
isWritable (IMAPC _ mbox _) = fmap _isWritable $ readIORef mbox
isFlagWritable (IMAPC _ mbox _) = fmap _isFlagWritable $ readIORef mbox

uidNext, uidValidity :: Stream s => IMAPConnection s -> IO UID
uidNext (IMAPC _ mbox _) = fmap _uidNext $ readIORef mbox
uidValidity (IMAPC _ mbox _) = fmap _uidValidity $ readIORef mbox

stream :: Stream s => IMAPConnection s -> s
stream (IMAPC s _ _) = s


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


data IMAPCommand = CAPABILITY  -- ^ check the capability of the server
                 | NOOP        -- ^ no operation, but the server may response some status
                 | LOGOUT
                 | STARTTLS    -- ^ defined RFC 3501, but not implemented now
                 | AUTHENTICATE AuthType
                 | LOGIN String {- ^ user name -} String {- ^ password -}
                 | SELECT Mailbox         -- ^ select a mailbox
                 | EXAMINE Mailbox        -- ^ select a mailbox as read-only
                 | CREATE Mailbox         -- ^ create a mailbox
                 | DELETE Mailbox         -- ^ delete a mailbox
                 | RENAME Mailbox Mailbox -- ^ rename a mailbox
                 | SUBSCRIBE Mailbox      -- ^ subscribe a mailbox
                 | UNSUBSCRIBE Mailbox    -- ^ unsubscribe a mailbox
                 | LIST String String     -- ^ list the mailboxs
                 | LSUB String String     -- ^ list the subscribed mailboxs
                 | STATUS Mailbox [MailboxStatus] -- ^ query the status of the mvailbox
                 | APPEND Mailbox [String] (Maybe String) String -- ^ append a mail data into a mailbox
                 | CHECK                     -- ^ request for checkpoint of the current mailbox
                 | CLOSE                     -- ^ close the current mailbox
                 | EXPUNGE                   -- ^ remove all marked mails actually
                 | SEARCH Charset SearchQuery -- ^ search mails in the current mailbox
                 | FETCH (Int, Int) [MessageQuery]
                 | STORE (Int, Int) FlagsQuery
                 | COPY (Int, Int) Mailbox
                 | UID IMAPCommand

type AuthType = ()
type Charset = String
data Flag = Seen
          | Answered
          | Flagged
          | Deleted
          | Draft
          | Recent
          | Keyword String
            deriving Eq

instance Show Flag where
    showsPrec d f = showParen (d > app_prec) $ showString $ showFlag f
        where app_prec = 10
              showFlag Seen        = "\\Seen"
              showFlag Answered    = "\\Answered"
              showFlag Flagged     = "\\Flagged"
              showFlag Deleted     = "\\Deleted"
              showFlag Draft       = "\\Draft"
              showFlag Recent      = "\\Recent"
              showFlag (Keyword s) = "\\" ++ s

data Attribute = Noinferiors
               | Noselect
               | Marked
               | Unmarked
data StatusCode = ALERT
                | BADCHARSET [String]
                | CAPABILITY_sc [String]
                | PARSE
                | PERMANENTFLAGS [Flag]
                | READ_ONLY
                | READ_WRITE
                | TRYCREATE
                | UIDNEXT_sc UID
                | UIDVALIDITY_sc UID
                | UNSEEN_sc Integer
                  deriving Eq


-- | the query data type for the status command
data MailboxStatus = MESSAGES     -- ^ the number of messages in the mailbox
                   | RECENT       -- ^ the number of messages with the \Recent flag set
                   | UIDNEXT      -- ^ the next unique identifier value of the mailbox
                   | UIDVALIDITY  -- ^ the unique identifier validity value of the mailbox
                     deriving (Show, Read)

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
            


data MessageQueryInner = PART Int (Maybe MessageQueryInner)
                       | BODY
                       | HEADER
                       | HEADER_FIELD [String]
                       | HEADER_FIELD_NOT [String]
                       | MIME

-- suffixed by `f'
data MessageQuery = BODYf [MessageQueryInner]
                  | BODY_PEEKf [MessageQueryInner]
                  | ENVELOPEf
                  | FLAGSf
                  | INTERNALDATEf
                  | RFC822f
                  | RFC822_HEADERf
                  | RFC822_SIZEf
                  | RFC822_TEXTf
                  | UIDf
                  | ALLf
                  | FASTf
                  | FULLf

data FlagsQuery = ReplaceFlags [Flag]
                | PlusFlags [Flag]
                | MinusFlags [Flag]


-- server responses
data ServerResponse = OK (Maybe StatusCode) String
                    | NO (Maybe StatusCode) String
                    | BAD (Maybe StatusCode) String
                    | PREAUTH (Maybe StatusCode) String
                    | FETCHr Integer [(MessageQuery, String)]


type ResponseParser st = CharParser st ServerResponse

crlf :: String
crlf = "\r\n"

space :: CharParser st Char
space = char ' '

atomChar :: CharParser st Char
atomChar = noneOf " (){%*\"\\]"

doParser :: Monad m => CharParser () (m ()) -> String -> m ()
doParser p = doEither . parse p ""
    where doEither (Right m) = m
          doEither (Left _)  = return ()

catRights :: [Either a b] -> [b]
catRights []           = []
catRights (Right r:tl) = r : catRights tl
catRights (_:tl)       = catRights tl

rights :: [Either a b] -> [b]
rights [] = []
rights (Right r:es) = r:rights es
rights (_:es)       = rights es


listResponse :: String -> CharParser st ([Attribute], String, Mailbox)
listResponse list = 
    do string list
       attrs <- parseAttrs
       sep <- parseSep
       mbox <- parseMailbox
       return $ (attrs, sep, mbox)
    where parseAttr =
              do char '\\'
                 choice [ try (string "Noinferior") >> return Noinferiors
                        , string "Noselect" >> return Noselect
                        , string "Marked" >> return Marked
                        , string "Unmarked" >> return Unmarked
                        ]
          parseAttrs = space >> between (char '(') (char ')')
                                     (parseAttr `sepBy` space)
          parseSep = space >> char '"' >> anyChar `manyTill` char '"'
          parseMailbox = space >> anyChar `manyTill` string crlf

normalResponse :: [(String, Maybe StatusCode -> String -> ServerResponse)] -> ResponseParser st
normalResponse list = 
    do respcode <- parseCode
       space
       stat <- option Nothing (parseStatusCode >>= \s -> space >> return (Just s))
       body <- anyChar `manyTill` string crlf
       return $ respcode stat body
    where parseCode = choice $ map (\(s, c) -> try (string s) >> return c) list

statusResponse :: CharParser st [(MailboxStatus, Integer)] 
statusResponse =
    do string "STATUS"
       space
       mbox <- anyChar `manyTill` space
       stats <- between (char '(') (char ')') (parseStat `sepBy1` space)
       string crlf
       return stats
    where parseStat =
              do cons <- choice [ string "MESSAGES" >>= return . read
                                , string "RECENT" >>= return . read
                                , try (string "UIDNEXT") >>= return . read
                                , string "UIDVALIDITY" >>= return . read
                                ]
                 space
                 num <- many1 digit >>= return . read
                 return (cons, num)

searchResponse :: CharParser st [UID]
searchResponse =
    do string "SEARCH"
       space
       nums <- (many1 digit) `sepBy` space
       string crlf
       return $ map read nums

flagsResponse :: CharParser st [Flag]
flagsResponse =
    do string "FLAGS"
       space
       flags <- between (char '(') (char ')') (parseFlag `sepBy` space)
       string crlf
       return flags

parseFlag :: CharParser st Flag
parseFlag = do char '\\'
               choice [ string "Seen" >> return Seen
                      , string "Answered" >> return Answered
                      , string "Flagged" >> return Flagged
                      , string "Deleted" >> return Deleted
                      , string "Draft" >> return Draft
                      , string "Recent" >> return Recent
                      , many1 atomChar >>= return . Keyword ]
        <|> (many1 atomChar >>= return . Keyword)

numberedResponse :: [(String, Integer -> a)] -> CharParser st a
numberedResponse list =
    do num <- many1 digit >>= return . read
       space
       cons <- choice $ map (\(s, c) -> try (string s) >> return c) list
       string crlf
       return $ cons num


fetchResponse :: ResponseParser st
fetchResponse = undefined

responseDone :: String -> ResponseParser st
responseDone tag =
    do string tag
       space
       normalResponse [("OK", OK), ("NO", NO), ("BAD", BAD)]

parseStatusCode :: CharParser st StatusCode
parseStatusCode = between (char '[') (char ']') $
                  choice [ string "ALERT" >> return ALERT
                         , string "BADCHARSET" >> option [] parenWords >>= return . BADCHARSET
                         , string "CAPABILITY" >> space >> ((many1 $ noneOf " ]") `sepBy1` space) >>= return . CAPABILITY_sc
                         , try (string "PARSE") >> return PARSE
                         , string "PERMANENTFLAGS" >> parenWords >>= return . PERMANENTFLAGS . map (either (error "no such flag") id . parse parseFlag "")
                         , try (string "READ-ONLY") >> return READ_ONLY
                         , string "READ-WRITE" >> return READ_WRITE 
                         , string "TRYCREATE" >> return TRYCREATE
                         , try (string "UNSEEN") >> space >> many1 digit >>= return . UNSEEN_sc . read
                         , try (string "UIDNEXT") >> space >> many1 digit >>= return . UIDNEXT_sc . read
                         , string "UIDVALIDITY" >> space >> many1 digit >>= return . UIDVALIDITY_sc . read ]
    where parenWords = space >> between (char '(') (char ')') ((many1 $ noneOf " )") `sepBy1` space)

capabilityResponse :: CharParser st [String]
capabilityResponse = do try $ string "CAPABILITY"
                        spaces
                        line <- many anyChar
                        return (words line)

zipParser :: CharParser st a -> CharParser st b -> CharParser st ([a], [b])
zipParser p1 p2 = do list <- many (fmap Left p1 <|> fmap Right p2)
                     return (lefts list, rights list)
    where lefts [] = []
          lefts (Left l:ls) = l : lefts ls
          lefts (_:ls) = lefts ls
          rights [] = []
          rights (Right r:rs) = r : rights rs
          rights (_:rs) = rights rs


connectIMAPPort :: String -> PortNumber -> IO (IMAPConnection Connection)
connectIMAPPort hostname port = openTCPPort hostname (fromEnum port) >>= connectStream

connectIMAP :: String -> IO (IMAPConnection Connection)
connectIMAP hostname = connectIMAPPort hostname 143

connectStream :: Stream s => s -> IO (IMAPConnection s)
connectStream s =
    do msg <- fmap (either (const "") id) $ readLine s
       unless (and $ zipWith (==) msg "* OK") $ fail "cannot connect to the server"
       mbox <- newIORef (MboxInfo "" 0 0 [] [] False False 0 0)
       c <- newIORef 0
       return $ IMAPC s mbox c

sendCommand' :: Stream s => IMAPConnection s -> String -> IO (String, [String])
sendCommand' (IMAPC s mbox nr) cmdstr =
    do num <- readIORef nr 
       writeBlock s (show6 num ++ " " ++ cmdstr ++ crlf)
       modifyIORef nr (+1)
       readLines [] s
    where readLines ls s =
              do l <- fmap (either (const "") id) $ readLine s
                 if (and $ zipWith (==) l "* ")
                    then readLines (drop 2 l:ls) s
                    else return (l, ls)

show6 n | n > 100000 = show n
        | n > 10000  = '0' : show n
        | n > 1000   = "00" ++ show n
        | n > 100    = "000" ++ show n
        | n > 10     = "0000" ++ show n
        | otherwise  = "00000" ++ show n

sendCommand :: Stream s => IMAPConnection s -> String -> IO ([String], ServerResponse)
sendCommand imapc@(IMAPC _ mbox nr) cmdstr =
    do num <- readIORef nr
       (done, ls) <- sendCommand' imapc cmdstr
       let resp = parse (responseDone (show6 num)) "" done
       mboxUpdateResponses mbox ls
       case resp of
         Right resp -> return (reverse ls, resp)
         _          -> fail "illegal server response"

sendAndReceive :: Stream s => IMAPConnection s -> String -> IO ()
sendAndReceive conn cmd =
    do (_, resp) <- sendCommand conn cmd
       case resp of
         OK _ _    -> return ()
         NO _ msg  -> fail msg
         BAD _ msg -> fail msg
         

mboxUpdateResponses :: IORef MailboxInfo -> [String] -> IO ()
mboxUpdateResponses mbox ls =
    mapM_ (doParser (numberedResponse resps)) ls
    where resps = [ ("EXISTS", updateExists mbox)
                  , ("RECENT", updateRecent mbox)]
          updateExists mbox n = do mboxv <- readIORef mbox
                                   writeIORef mbox (mboxv { _exists = n })
          updateRecent mbox n = do mboxv <- readIORef mbox
                                   writeIORef mbox (mboxv { _recent = n })

noop :: Stream s => IMAPConnection s -> IO ()
noop conn@(IMAPC s mbox _) = sendCommand conn "NOOP" >> return ()

capability :: Stream s => IMAPConnection s -> IO [String]
capability conn =
    do (ls, resp) <- sendCommand conn "CAPABILITY"
       return $ concatMap (either (const []) id .
                             parse (capabilityResponse <|> return []) "") ls

logout :: Stream s => IMAPConnection s -> IO ()
logout conn@(IMAPC s _ _) =
    do (ls, resp) <- sendCommand conn "LOGOUT"
       S.close s

login :: Stream s => IMAPConnection s -> String -> String -> IO ()
login conn user pass =
    sendAndReceive conn $ "LOGIN " ++ user ++ " " ++ pass

select, examine, create, delete :: Stream s =>
                                   IMAPConnection s -> Mailbox -> IO ()
_select cmd conn@(IMAPC s mbox _) mboxName =
    do (ls, resp) <- sendCommand conn (cmd ++ mboxName)
       case resp of
         OK writable _ ->
             do let oks = oklist ls
                mapM_ flags ls
                mapM_ pflags oks
                mapM_ uidnext oks
                mapM_ uidvalidity oks
                mboxv <- readIORef mbox
                writeIORef mbox (mboxv  { _mailbox = mboxName
                                        , _isWritable = isJust writable && (fromJust writable == READ_WRITE) })
                
         _ -> fail ("cannot select mailbox: " ++ mboxName)
    where flags l = let flags = parse flagsResponse "" l in
                    case flags of
                      Right fs -> do mboxv <- readIORef mbox
                                     writeIORef mbox (mboxv { _flags = fs })
                      _ -> return ()
          oklist = map $ parse (normalResponse [("OK", OK)]) ""
          pflags (Right (OK (Just (PERMANENTFLAGS flags)) _)) =
              do mboxv <- readIORef mbox
                 writeIORef mbox (mboxv { _isFlagWritable = Keyword "*" `elem` flags, _permanentFlags = filter (/= Keyword "*") flags })
          pflags _ = return ()
          uidnext (Right (OK (Just (UIDNEXT_sc n)) _)) =
              do mboxv <- readIORef mbox
                 writeIORef mbox (mboxv { _uidNext = n })
          uidnext _ = return ()
          uidvalidity (Right (OK (Just (UIDVALIDITY_sc n)) _)) =
              do mboxv <- readIORef mbox
                 writeIORef mbox (mboxv { _uidValidity = n })
          uidvalidity _ = return ()

select = _select "SELECT "
examine = _select "EXAMINE "
create conn mboxname = sendAndReceive conn $ "CREATE " ++ mboxname
delete conn mboxname = sendAndReceive conn $ "DELETE " ++ mboxname

rename :: Stream s => IMAPConnection s -> Mailbox -> Mailbox -> IO ()
rename conn mboxorg mboxnew =
    sendAndReceive conn $ "RENAME " ++ mboxorg ++ " " ++ mboxnew

subscribe, unsubscribe :: Stream s => IMAPConnection s -> Mailbox -> IO ()
subscribe conn mboxname = sendAndReceive conn $ "SUBSCRIBE " ++ mboxname
unsubscribe conn mboxname = sendAndReceive conn $ "UNSUBSCRIBE " ++ mboxname

list, lsub :: Stream s => IMAPConnection s -> IO [([Attribute], Mailbox)]
list conn = fmap (map (\(a, _, m) -> (a, m))) $ listFull conn "\"\"" "*"
lsub conn = fmap (map (\(a, _, m) -> (a, m))) $ lsubFull conn "\"\"" "*"

listPat, lsubPat :: Stream s => IMAPConnection s -> String -> IO [([Attribute], String, Mailbox)]
listPat conn pat = listFull conn "\"\"" pat
lsubPat conn pat = lsubFull conn "\"\"" pat

listFull, lsubFull :: Stream s => IMAPConnection s -> String -> String -> IO [([Attribute], String, Mailbox)]
listFull conn ref pat = listFull_ conn "LIST" ref pat
lsubFull conn ref pat = listFull_ conn "LSUB" ref pat

listFull_ conn list ref pat =
    do (ls, resp) <- sendCommand conn (list ++ " " ++ ref ++ " " ++ pat)
       case resp of
         NO _ msg  -> fail msg
         BAD _ msg -> fail msg
         OK _ _    -> return $ rights $ map (parse (listResponse list) "") ls


status :: Stream s => IMAPConnection s -> Mailbox -> [MailboxStatus] -> IO [(MailboxStatus, Integer)]
status conn mbox stats =
    do (ls, resp) <- sendCommand conn ("STATUS " ++ mbox ++ " (" ++ (unwords $ map show stats) ++ ")")
       case resp of
         NO _ msg  -> fail msg
         BAD _ msg -> fail msg
         OK _ _    -> return $ either (const []) id $ head $ map (parse statusResponse "") ls

append :: Stream s => IMAPConnection s -> Mailbox -> String -> IO ()
append conn mbox mailData = appendFull conn mbox mailData [] Nothing

appendFull :: Stream s => IMAPConnection s -> Mailbox -> String -> [Flag] -> Maybe CalendarTime -> IO ()
appendFull conn@(IMAPC s _ nr) mbox mailData flags time = 
    do num <- readIORef nr
       (r, _) <- sendCommand' conn (sep ["APPEND", mbox
                                        , fstr, tstr,  "{" ++ show len ++ "}"])
       unless (null r || (head r /= '+')) $ fail "illegal server response"
       writeBlock s mailData'
       doneStr <- fmap (either (const "") id) $ readLine s
       let resp = parse (responseDone (show6 num)) "" doneStr
       case resp of
         Right resp -> return ()
         _          -> fail "illegal server response"
    where mailLines = map (++crlf) $ lines mailData
          len       = sum $ map length mailLines
          mailData' = concat mailLines
          tstr      = maybe "" show time
          sep       = concat . intersperse " "
          fstr      = sep $ map show flags

check :: Stream s => IMAPConnection s -> IO ()
check conn = do (_, resp) <- sendCommand conn "CHECK"
                case resp of
                  BAD _ msg -> fail msg
                  OK _ _    -> return ()

close :: Stream s => IMAPConnection s -> IO ()
close conn = do (_, resp) <- sendCommand conn "CLOSE"
                case resp of
                  BAD _ msg -> fail msg
                  OK _ _    -> return ()

expunge :: Stream s => IMAPConnection s -> IO [Integer]
expunge conn = do (ls, resp) <- sendCommand conn "EXPUNGE"
                  case resp of
                    NO _ msg  -> fail msg
                    BAD _ msg -> fail msg
                    OK _ _    -> return $ parse' ls
    where expungeParser = numberedResponse [("EXPUNGE", id)]
          parse' = catRights . map (parse expungeParser "")

search :: Stream s => IMAPConnection s -> [SearchQuery] -> IO [UID]
search conn queries = searchCharset conn "" queries

searchCharset :: Stream s => IMAPConnection s -> String -> [SearchQuery] -> IO [UID]
searchCharset conn charset queries =
    do (ls, resp) <- sendCommand conn
                     ("UID SEARCH " ++ charsetstr ++ " " ++ qstr)
       case resp of
         OK _ _ -> return $ parse' ls
         BAD _ msg -> fail msg
         NO _ msg  -> fail msg
      where qstr = "(" ++ (concat $ intersperse " " $ map show queries)++ ")"
            charsetstr = if null charset then "" else "CHARSET " ++ charset
            parse' = concat . catRights . map (parse searchResponse "")

fetchByString :: Stream s => IMAPConnection s -> (Int, Int) -> String -> IO [(Int, String)]
fetchByString = undefined

fetch, fetchHeader, fetchEnvelope :: Stream s => IMAPConnection s -> (Int, Int) -> IO [(Int, String)]
fetch = undefined
fetchHeader = undefined
fetchEnvelope = undefined
fetchSize :: Stream s => IMAPConnection s -> (Int, Int) -> IO [(Int, Int)]
fetchSize = undefined
fetchHeaderIf, fetchHeaderNotIf :: Stream s => IMAPConnection s -> (Int, Int) -> String -> IO [(Int, String)]
fetchHeaderIf = undefined
fetchHeaderNotIf = undefined
fetchFlags :: Stream s => IMAPConnection s -> (Int, Int) -> IO [(Int, [Flag])]
fetchFlags = undefined

storeFull conn uidstr query isSilent =
    do (ls, resp) <- sendCommand conn ("UID STORE " ++ uidstr ++ flags query)
       case resp of
         NO _ msg  -> fail msg
         BAD _ msg -> fail msg
         OK _ _    -> return []
    where fstrs fs = "(" ++ (concat $ intersperse " " $ map show fs) ++ ")"
          toFStr s fstrs =
              s ++ (if isSilent then ".SILENT" else "") ++ " " ++ fstrs
          flags (ReplaceFlags fs) = toFStr "FLAGS" $ fstrs fs
          flags (PlusFlags fs)    = toFStr "+FLAGS" $ fstrs fs
          flags (MinusFlags fs)   = toFStr "-FLAGS" $ fstrs fs


store :: Stream s => IMAPConnection s -> UID -> FlagsQuery -> IO ()
storeR :: Stream s => IMAPConnection s -> (UID, UID) -> FlagsQuery -> IO ()
store conn i q       = storeFull conn (show i) q True >> return ()
storeR conn (s, e) q = storeFull conn (show s++":"++show e) q True >> return ()
-- storeResults is used without .SILENT, so that its response contains its result flags
storeResults :: Stream s => IMAPConnection s -> UID -> FlagsQuery -> IO [(UID, [Flag])]
storeResultsR :: Stream s => IMAPConnection s -> (UID, UID) -> FlagsQuery -> IO [(UID, [Flag])]
storeResults conn i q       = storeFull conn (show i) q False
storeResultsR conn (s, e) q = storeFull conn (show s++":"++show e) q False

copy :: Stream s => IMAPConnection s -> UID -> Mailbox -> IO ()
copyR :: Stream s => IMAPConnection s -> (UID, UID) -> Mailbox -> IO ()
copyFull conn uidStr mbox =
    do (_, resp) <- sendCommand conn ("UID COPY " ++ uidStr ++ " " ++ mbox)
       case resp of
         BAD _ msg -> fail msg
         NO _ msg  -> fail msg
         OK _ _    -> return ()

copy conn uid mbox     = copyFull conn (show uid) mbox
copyR conn (s, e) mbox = copyFull conn (show s++":"++show e) mbox
