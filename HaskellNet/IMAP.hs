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

import Network
-- import HaskellNet.Stream hiding (close)
-- import qualified HaskellNet.Stream as S (close)
import HaskellNet.BSStream

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Data.Digest.MD5
import Control.Monad
import Control.Monad.Writer

import System.IO
import System.Time

import Text.ParserCombinators.Parsec hiding (space)

import Data.IORef
import Data.Word
import Data.Maybe
import Data.List
import Data.Char (isSpace, isDigit, toUpper)

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
                | BADCHARSET [Charset]
                | CAPABILITY_sc [String]
                | PARSE
                | PERMANENTFLAGS [Flag]
                | READ_ONLY
                | READ_WRITE
                | TRYCREATE
                | UIDNEXT_sc UID
                | UIDVALIDITY_sc UID
                | UNSEEN_sc Integer
                  deriving (Eq, Show)


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
                      deriving Show

type ResponseParser st = CharParser st ServerResponse

crlf :: String
crlf = "\r\n"

crlf' :: ByteString
crlf' = BS.pack crlf

crP :: CharParser st Char
crP = char '\r'

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

isLeft, isRight :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
isRight (Right _) = True
isRight _         = False

getLeft :: Either a b -> a
getLeft (Left l) = l
getLeft _        = error "not left"
getRight :: Either a b -> b
getRight (Right r) = r
getRight _         = error "not right"


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
          parseMailbox = space >> anyChar `manyTill` crP

normalResponse :: [(String, Maybe StatusCode -> String -> ServerResponse)] -> ResponseParser st
normalResponse list = 
    do respcode <- parseCode
       space
       stat <- option Nothing (parseStatusCode >>= \s -> space >> return (Just s))
       body <- anyChar `manyTill` crP
       return $ respcode stat body
    where parseCode = choice $ map (\(s, c) -> try (string s) >> return c) list

statusResponse :: CharParser st [(MailboxStatus, Integer)] 
statusResponse =
    do string "STATUS"
       space
       mbox <- anyChar `manyTill` space
       stats <- between (char '(') (char ')') (parseStat `sepBy1` space)
       crP
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


fetchResponse :: BSStream s => ByteString -> s -> WriterT [ByteString] IO [(Int, [(String, ByteString)])]
fetchResponse tag s =
    do l <- liftIO $ bsGetLine s
       liftIO $ putStr "fetchResponse: "
       liftIO $ print l
       if BS.null l || (BS.head l == '*' && not (isFetch l))
          then tell [l] >> fetchResponse tag s
          else if not (BS.null l) && (BS.head l /= '*') && tagged l
               then tell [l] >> return []
               else fetchMain $ parse singleLine "" $ BS.unpack l
    where isFetch l = BS.pack "FETCH" == (BS.take 5 $ BS.dropWhile isSpace $ BS.dropWhile isDigit $ BS.dropWhile isSpace $ BS.tail l)
          tagged l =
              BS.length l >= BS.length tag && (and $ BS.zipWith (==) tag l)
          fetchMain (Left _) = liftIO (putStrLn "fetchMain1") >> fetchResponse tag s
          fetchMain (Right (n, ps, Nothing)) =
              liftIO (putStrLn "fetchMain2") >> (fmap ((n, ps):) $ fetchResponse tag s)
          fetchMain (Right (n, ps, Just len)) =
              do liftIO $ putStrLn "fetchMain3"
                 v <- liftIO $ bsGet s len
                 let ps' = init ps ++ [(fst $ last ps, v)]
                 l' <- liftIO $ bsGetLine s
                 v <- fetchMain (parse (getPairs n) "" $ BS.unpack l')
                 return $ (n, ps' ++ snd (head v)) : tail v
          singleLine = do spaces
                          n <- fmap read $ many1 digit
                          spaces >> string "FETCH" >> spaces >> char '('
                          getPairs n
          getPairs n = (getPair `sepBy` spaces) >>= \ps ->
                       if isLeft $ snd $ last ps
                       then do char ')' >> crP
                               return (n, map fixEither ps, Nothing) 
                       else do crP
                               return ( n
                                      , map fixEither ps
                                      , Just $ getRight $ snd $ last ps)
          fixEither (k, v) = (k, either id (const BS.empty) v)
          getPair = liftM2 (,) (anyChar `manyTill` many1 space) getValues
          getValues = do char '('
                         s <- anyChar `manyTill` char ')'
                         return $ Left $ BS.pack $ '(':s++")"
                  <|> fmap (Right . read) (between (char '{') (char '}') (many1 digit))
                  <|> fmap (Left . BS.pack) (anyChar `manyTill` space)

searchResponse :: CharParser st [UID]
searchResponse =
    do string "SEARCH"
       space
       nums <- (many1 digit) `sepBy` space
       crP
       return $ map read nums

flagsResponse :: CharParser st [Flag]
flagsResponse =
    do string "FLAGS"
       space
       flags <- between (char '(') (char ')') (parseFlag `sepBy` space)
       crP
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
       crP
       return $ cons num


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


connectIMAPPort :: String -> PortNumber -> IO (IMAPConnection Handle)
connectIMAPPort hostname port = connectTo hostname (PortNumber port) >>= connectStream

connectIMAP :: String -> IO (IMAPConnection Handle)
connectIMAP hostname = connectIMAPPort hostname 143

connectStream :: BSStream s => s -> IO (IMAPConnection s)
connectStream s =
    do msg <- bsGetLine s
       unless (and $ BS.zipWith (==) msg (BS.pack "* OK")) $ fail "cannot connect to the server"
       mbox <- newIORef (MboxInfo "" 0 0 [] [] False False 0 0)
       c <- newIORef 0
       return $ IMAPC s mbox c

sendCommand' :: BSStream s => IMAPConnection s -> String -> IO (ByteString, [ByteString])
sendCommand' (IMAPC s mbox nr) cmdstr =
    do num <- readIORef nr 
       bsPut s $ BS.pack $ show6 num ++ " " ++ cmdstr ++ crlf
       modifyIORef nr (+1)
       readLines [] s
    where readLines ls s =
              do l <- bsGetLine s
                 if (and $ BS.zipWith (==) l $ BS.pack "* ")
                    then readLines (BS.drop 2 l:ls) s
                    else return (l, ls)

show6 n | n > 100000 = show n
        | n > 10000  = '0' : show n
        | n > 1000   = "00" ++ show n
        | n > 100    = "000" ++ show n
        | n > 10     = "0000" ++ show n
        | otherwise  = "00000" ++ show n

sendCommand :: BSStream s => IMAPConnection s -> String -> IO ([ByteString], ServerResponse)
sendCommand imapc@(IMAPC _ mbox nr) cmdstr =
    do num <- readIORef nr
       (done, ls) <- sendCommand' imapc cmdstr
       let resp = parse (responseDone (show6 num)) "" $ BS.unpack done
       mboxUpdateResponses mbox $ map BS.unpack ls
       case resp of
         Right resp -> return (reverse ls, resp)
         _          -> fail "illegal server response"

sendAndReceive :: BSStream s => IMAPConnection s -> String -> IO ()
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

noop :: BSStream s => IMAPConnection s -> IO ()
noop conn@(IMAPC s mbox _) = sendCommand conn "NOOP" >> return ()

capability :: BSStream s => IMAPConnection s -> IO [String]
capability conn =
    do (ls, resp) <- sendCommand conn "CAPABILITY"
       return $ concatMap (either (const []) id .
                           parse (capabilityResponse <|> return []) "" .
                           BS.unpack) ls

logout :: BSStream s => IMAPConnection s -> IO ()
logout conn@(IMAPC s _ _) =
    do (ls, resp) <- sendCommand conn "LOGOUT"
       bsClose s

login :: BSStream s => IMAPConnection s -> String -> String -> IO ()
login conn user pass =
    sendAndReceive conn $ "LOGIN " ++ user ++ " " ++ pass

select, examine, create, delete :: BSStream s =>
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
    where flags l = case parse flagsResponse "" $ BS.unpack l of
                      Right fs -> do mboxv <- readIORef mbox
                                     writeIORef mbox (mboxv { _flags = fs })
                      _ -> return ()
          oklist = map $ (parse (normalResponse [("OK", OK)]) "" . BS.unpack)
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

rename :: BSStream s => IMAPConnection s -> Mailbox -> Mailbox -> IO ()
rename conn mboxorg mboxnew =
    sendAndReceive conn $ "RENAME " ++ mboxorg ++ " " ++ mboxnew

subscribe, unsubscribe :: BSStream s => IMAPConnection s -> Mailbox -> IO ()
subscribe conn mboxname = sendAndReceive conn $ "SUBSCRIBE " ++ mboxname
unsubscribe conn mboxname = sendAndReceive conn $ "UNSUBSCRIBE " ++ mboxname

list, lsub :: BSStream s => IMAPConnection s -> IO [([Attribute], Mailbox)]
list conn = fmap (map (\(a, _, m) -> (a, m))) $ listFull conn "\"\"" "*"
lsub conn = fmap (map (\(a, _, m) -> (a, m))) $ lsubFull conn "\"\"" "*"

listPat, lsubPat :: BSStream s => IMAPConnection s -> String -> IO [([Attribute], String, Mailbox)]
listPat conn pat = listFull conn "\"\"" pat
lsubPat conn pat = lsubFull conn "\"\"" pat

listFull, lsubFull :: BSStream s => IMAPConnection s -> String -> String -> IO [([Attribute], String, Mailbox)]
listFull conn ref pat = listFull_ conn "LIST" ref pat
lsubFull conn ref pat = listFull_ conn "LSUB" ref pat

listFull_ conn list ref pat =
    do (ls, resp) <- sendCommand conn (list ++ " " ++ ref ++ " " ++ pat)
       case resp of
         NO _ msg  -> fail msg
         BAD _ msg -> fail msg
         OK _ _    -> return $ rights $ map (parse (listResponse list) "") $ map BS.unpack ls


status :: BSStream s => IMAPConnection s -> Mailbox -> [MailboxStatus] -> IO [(MailboxStatus, Integer)]
status conn mbox stats =
    do (ls, resp) <- sendCommand conn ("STATUS " ++ mbox ++ " (" ++ (unwords $ map show stats) ++ ")")
       case resp of
         NO _ msg  -> fail msg
         BAD _ msg -> fail msg
         OK _ _    -> return $ either (const []) id $ head $ map (parse statusResponse "") $ map BS.unpack ls

append :: BSStream s => IMAPConnection s -> Mailbox -> ByteString -> IO ()
append conn mbox mailData = appendFull conn mbox mailData [] Nothing

appendFull :: BSStream s => IMAPConnection s -> Mailbox -> ByteString -> [Flag] -> Maybe CalendarTime -> IO ()
appendFull conn@(IMAPC s _ nr) mbox mailData flags time = 
    do num <- readIORef nr
       (r, _) <- sendCommand' conn (sep ["APPEND", mbox
                                        , fstr, tstr,  "{" ++ show len ++ "}"])
       unless (BS.null r || (BS.head r /= '+')) $ fail "illegal server response"
       mapM_ (\l -> bsPut s l >> bsPut s crlf') mailLines
       doneStr <- bsGetLine s
       case parse (responseDone (show6 num)) "" $ BS.unpack doneStr of
         Right resp -> return ()
         _          -> fail "illegal server response"
    where mailLines = BS.lines mailData
          len       = sum $ map ((2+) . BS.length) mailLines
          tstr      = maybe "" show time
          sep       = concat . intersperse " "
          fstr      = sep $ map show flags

check :: BSStream s => IMAPConnection s -> IO ()
check conn = do (_, resp) <- sendCommand conn "CHECK"
                case resp of
                  BAD _ msg -> fail msg
                  OK _ _    -> return ()

close :: BSStream s => IMAPConnection s -> IO ()
close conn@(IMAPC s _ _) = do (_, resp) <- sendCommand conn "CLOSE"
                              case resp of
                                BAD _ msg -> fail msg
                                OK _ _    -> return ()

expunge :: BSStream s => IMAPConnection s -> IO [Integer]
expunge conn = do (ls, resp) <- sendCommand conn "EXPUNGE"
                  case resp of
                    NO _ msg  -> fail msg
                    BAD _ msg -> fail msg
                    OK _ _    -> return $ parse' $ map BS.unpack ls
    where expungeParser = numberedResponse [("EXPUNGE", id)]
          parse' = catRights . map (parse expungeParser "")

search :: BSStream s => IMAPConnection s -> [SearchQuery] -> IO [UID]
search conn queries = searchCharset conn "" queries

searchCharset :: BSStream s => IMAPConnection s -> Charset -> [SearchQuery] -> IO [UID]
searchCharset conn charset queries =
    do (ls, resp) <- sendCommand conn
                     ("UID SEARCH " ++ charsetstr ++ " " ++ qstr)
       case resp of
         OK _ _ -> return $ parse' $ map BS.unpack ls
         BAD _ msg -> fail msg
         NO _ msg  -> fail msg
      where qstr = "(" ++ (concat $ intersperse " " $ map show queries)++ ")"
            charsetstr = if null charset then "" else "CHARSET " ++ charset
            parse' = concat . catRights . map (parse searchResponse "")

fetch, fetchHeader :: BSStream s => IMAPConnection s -> UID -> IO ByteString
fetch conn uid =
    do lst <- fetchByString conn uid "BODY[]"
       return $ maybe BS.empty id $ lookup "BODY[]" lst
fetchHeader conn uid =
    do lst <- fetchByString conn uid "BODY[HEADER]"
       return $ maybe BS.empty id $ lookup "BODY[HEADER]" lst
fetchSize :: BSStream s => IMAPConnection s -> UID -> IO Int
fetchSize conn uid =
    do lst <- fetchByString conn uid "RFC822.SIZE"
       return $ maybe 0 (read . BS.unpack) $ lookup "RFC822.SIZE" lst
fetchHeaderFields, fetchHeaderFieldsNot :: BSStream s => IMAPConnection s -> UID -> [String] -> IO ByteString
fetchHeaderFields conn uid hs =
    do lst <- fetchByString conn uid ("BODY[HEADER.FIELDS "++unwords hs++"]")
       return $ maybe BS.empty id $
              lookup ("BODY[HEADER.FIELDS "++unwords hs++"]") lst
fetchHeaderFieldsNot conn uid hs = 
    do lst <- fetchByString conn uid ("BODY[HEADER.FIELDS.NOT "++unwords hs++"]")
       return $ maybe BS.empty id $ lookup ("BODY[HEADER.FIELDS.NOT "++unwords hs++"]") lst
fetchFlags :: BSStream s => IMAPConnection s -> UID -> IO [Flag]
fetchFlags conn uid =
    do lst <- fetchByString conn uid "FLAGS"
       return $ getFlags $ lookup "FLAGS" lst
    where getFlags Nothing  = []
          getFlags (Just s) = either (const []) id $ parse (between (char '(') (char ')') (parseFlag `sepBy` space)) "" $ BS.unpack s

fetchR :: BSStream s => IMAPConnection s -> (UID, UID) -> IO [(UID, ByteString)]
fetchR conn r =
    do lst <- fetchByStringR conn r "BODY[]"
       return $ map (\(uid, vs) -> (uid, maybe BS.empty id $ lookup "BODY[]" vs)) lst
fetchByString :: BSStream s => IMAPConnection s -> UID -> String -> IO [(String, ByteString)]
fetchByString conn uid command =
    do lst <- fetchCommand conn ("UID FETCH "++show uid++" "++command) id
       return $ snd $ head lst
fetchByStringR :: BSStream s => IMAPConnection s -> (UID, UID) -> String -> IO [(UID, [(String, ByteString)])]
fetchByStringR conn (s, e) command =
    fetchCommand conn ("UID FETCH "++show s++":"++show e++" "++command) proc
    where proc (n, ps) =
              (maybe (toEnum n) (read . BS.unpack) (lookup "UID" ps), ps)

fetchCommand conn@(IMAPC s mbox nr) command proc =
    do num <- readIORef nr
       bsPut s $ BS.pack command
       modifyIORef nr (+1)
       (fetched, others) <- runWriterT $ fetchResponse (BS.pack (show6 num)) s
       mboxUpdateResponses mbox $ map BS.unpack $ init others
       case parse (responseDone (show6 num)) "" (BS.unpack $ last others) of
         Left _    -> fail "illegal server response"
         Right (NO _ msg)  -> fail msg
         Right (BAD _ msg) -> fail msg
         Right (OK _ _)    -> return $ map proc fetched

storeFull :: BSStream s => IMAPConnection s -> String -> FlagsQuery -> Bool -> IO [(UID, [Flag])]
storeFull conn uidstr query isSilent =
    fetchCommand conn ("UID STORE " ++ uidstr ++ flags query) procStore
    where fstrs fs = "(" ++ (concat $ intersperse " " $ map show fs) ++ ")"
          toFStr s fstrs =
              s ++ (if isSilent then ".SILENT" else "") ++ " " ++ fstrs
          flags (ReplaceFlags fs) = toFStr "FLAGS" $ fstrs fs
          flags (PlusFlags fs)    = toFStr "+FLAGS" $ fstrs fs
          flags (MinusFlags fs)   = toFStr "-FLAGS" $ fstrs fs
          procStore (n, ps) = (maybe (toEnum n) (read . BS.unpack)
                                         (lookup "UID" ps)
                              ,maybe [] pflags (lookup "FLAG" ps))
          pflags = either (const []) id .
                     parse (between (char '(') (char ')')
                                        (parseFlag `sepBy` space)) "" .
                     BS.unpack


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
    do (_, resp) <- sendCommand conn ("UID COPY " ++ uidStr ++ " " ++ mbox)
       case resp of
         BAD _ msg -> fail msg
         NO _ msg  -> fail msg
         OK _ _    -> return ()

copy conn uid mbox     = copyFull conn (show uid) mbox
copyR conn (s, e) mbox = copyFull conn (show s++":"++show e) mbox
