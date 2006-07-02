----------------------------------------------------------------------
-- |
-- Module      :  Network.IMAP
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  implementing
-- Portability :  portable
-- 
-- IMAP client implementation
-- 

module Network.IMAP
where

import Network (PortNumber)
import Network.Stream hiding (close)
import qualified Network.Stream as S (close)
import Network.TCP

import Data.Digest.MD5
import Control.Monad

import System.IO
import System.Time

import Text.ParserCombinators.Parsec hiding (space)

import Data.IORef
import Data.Word
import Data.Maybe

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
                   | RECENTNum    -- ^ the number of messages with the \Recent flag set
                   | UIDNEXT      -- ^ the next unique identifier value of the mailbox
                   | UIDVALIDITY  -- ^ the unique identifier validity value of the mailbox
                  deriving Show

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
                 | UIDs [Integer]

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
                    | LISTr [Attribute] String Mailbox
                    | LSUBr [Attribute] String Mailbox
                    | STATUSr Mailbox [(MailboxStatus, Integer)]
                    | SEARCHr [Integer]
                    | EXPUNGEr Integer
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

listLikeResponse :: String -> ([Attribute] -> String -> Mailbox -> ServerResponse) -> ResponseParser st
listLikeResponse list listCons = 
    do string list
       attrs <- parseAttrs
       sep <- parseSep
       mbox <- parseMailbox
       return $ listCons attrs sep mbox
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

listResponse, lsubResponse :: ResponseParser st
listResponse = listLikeResponse "LIST" LISTr
lsubResponse = listLikeResponse "LSUB" LSUBr

normalResponse :: [(String, Maybe StatusCode -> String -> ServerResponse)] -> ResponseParser st
normalResponse list = 
    do respcode <- parseCode
       space
       stat <- option Nothing (parseStatusCode >>= \s -> space >> return (Just s))
       body <- anyChar `manyTill` string crlf
       return $ respcode stat body
    where parseCode = choice $ map (\(s, c) -> try (string s) >> return c) list

statusResponse :: ResponseParser st
statusResponse =
    do string "STATUS"
       space
       mbox <- anyChar `manyTill` space
       stats <- between (char '(') (char ')') (parseStat `sepBy1` space)
       string crlf
       return $ STATUSr mbox stats
    where parseStat =
              do cons <- choice [ string "MESSAGES" >> return MESSAGES
                                , string "RECENT" >> return RECENTNum
                                , try (string "UIDNEXT") >> return UIDNEXT
                                , string "UIDVALIDITY" >> return UIDVALIDITY
                                ]
                 space
                 num <- many1 digit >>= return . read
                 return (cons, num)

searchResponse :: ResponseParser st
searchResponse =
    do string "SEARCH"
       space
       nums <- (many1 digit) `sepBy` space
       string crlf
       return $ SEARCHr $ map read nums

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

expungeResponse :: ResponseParser st
expungeResponse = numberedResponse [("EXPUNGE", EXPUNGEr)]

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

sendCommand :: Stream s => IMAPConnection s -> String -> IO ([String], ServerResponse)
sendCommand (IMAPC s mbox nr) cmdstr =
    do num <- readIORef nr 
       writeBlock s (show6 num ++ " " ++ cmdstr ++ crlf)
       (done, ls) <- readLines [] s
       let resp = parse (responseDone (show6 num)) "" done
       modifyIORef nr (+1)
       mboxUpdateResponses mbox ls
       case resp of
         Right resp -> return (reverse ls, resp)
         _          -> fail "illegal server response"
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
login conn@(IMAPC s _ _) user pass =
    do (ls, resp) <- sendCommand conn $ "LOGIN " ++ user ++ " " ++ pass
       case resp of
         OK _ _ -> return ()
         NO _ _ -> fail $ "cannot login for " ++ user
         BAD _ _ -> fail "illegal format of username of password"

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
create = undefined
delete = undefined

rename :: Stream s => IMAPConnection s -> Mailbox -> Mailbox -> IO ()
rename = undefined

subscribe, unsubscribe :: Stream s => IMAPConnection s -> Mailbox -> IO ()
subscribe = undefined
unsubscribe = undefined

list, lsub :: Stream s => IMAPConnection s -> IO [([Attribute], Mailbox)]
list = undefined
lsub = undefined

status :: Stream s => IMAPConnection s -> Mailbox -> IO [(MailboxStatus, Integer)]
status = undefined

append :: Stream s => IMAPConnection s -> Mailbox -> String -> IO ()
append = undefined

appendFull :: Stream s => IMAPConnection s -> Mailbox -> [Flag] -> CalendarTime -> IO ()
appendFull = undefined

check :: Stream s => IMAPConnection s -> IO ()
check = undefined

close :: Stream s => IMAPConnection s -> IO ()
close = undefined

expunge :: Stream s => IMAPConnection s -> IO [Int]
expunge = undefined

search :: Stream s => IMAPConnection s -> [SearchQuery] -> IO [Int]
search = undefined

searchCharset :: Stream s => IMAPConnection s -> String -> [SearchQuery] -> IO [Int]
searchCharset = undefined

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


store :: Stream s => IMAPConnection s -> (Int, Int) -> FlagsQuery -> IO ()
store = undefined
-- storeResults is used without .SILENT, so that its response contains its result flags
storeResults :: Stream s => IMAPConnection s -> (Int, Int) -> FlagsQuery -> IO [(Int, [Flag])]
storeResults = undefined

copy :: Stream s => IMAPConnection s -> (Int, Int) -> Mailbox -> IO ()
copy = undefined

