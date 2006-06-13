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

import Network.BSD
import Network

import Data.Digest.MD5
import Control.Monad

import System.IO
import System.Time

import Text.ParserCombinators.Parsec hiding (space)


newtype IMAPConnection = IMAPConnection Handle
    deriving Show

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
                 | STATUS Mailbox [MailboxStatus] -- ^ query the status of the mailbox
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
type Mailbox = String
type UID = Int
type Charset = String
data Flag = Seen
          | Answered
          | Flagged
          | Deleted
          | Draft
          | Recent
          | Keyword String
data Attribute = Noinferiors
               | Noselect
               | Marked
               | Unmarked
data StatusCode = ALERT
                | BADCHARSET [String]
                | CAPABILITY_sc [String]
                | PARSE
                | PERMANENTFLAGS [String]
                | READ_ONLY
                | READ_WRITE
                | TRYCREATE
                | UIDNEXT_sc Integer
                | UIDVALIDITY_sc Integer
                | UNSEEN_sc Integer


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

data FlagsQuery = ReplaceFlags [Flag] Bool
                | PlusFlags [Flag] Bool
                | MinusFlags [Flag] Bool


-- server responses
data ServerResponse = OK (Maybe StatusCode) String
                    | NO (Maybe StatusCode) String
                    | BAD (Maybe StatusCode) String
                    | PREAUTH (Maybe StatusCode) String
                    | BYE (Maybe StatusCode) String
                    | CAPABILITYr [String]
                    | LISTr [Attribute] String Mailbox
                    | LSUBr [Attribute] String Mailbox
                    | STATUSr Mailbox [(MailboxStatus, Integer)]
                    | SEARCHr [Integer]
                    | FLAGSr [Flag]
                    | EXISTSr Integer
                    | RECENTr Integer
                    | EXPUNGEr Integer
                    | FETCHr Integer [(MessageQuery, String)]


type ResponseParser st = CharParser st ServerResponse

crlf :: String
crlf = "\r\n"

space :: CharParser st Char
space = char ' '

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

flagsResponse :: ResponseParser st
flagsResponse =
    do string "FLAGS"
       space
       flags <- between (char '(') (char ')') (parseFlag `sepBy` space)
       string crlf
       return $ FLAGSr flags
    where parseFlag =
              do char '\\'
                 choice [ string "Seen" >> return Seen
                        , string "Answered" >> return Answered
                        , string "Flagged" >> return Flagged
                        , string "Deleted" >> return Deleted
                        , string "Draft" >> return Draft
                        , string "Recent" >> return Recent
                        , many1 (noneOf " )") >>= return . Keyword ]

numberedResponse :: [(String, Integer -> ServerResponse)] -> ResponseParser st
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
                         , string "PERMANENTFLAGS" >> parenWords >>= return . PERMANENTFLAGS 
                         , try (string "READ-ONLY") >> return READ_ONLY
                         , string "READ-WRITE" >> return READ_WRITE 
                         , string "TRYCREATE" >> return TRYCREATE
                         , try (string "UNSEEN") >> space >> many1 digit >>= return . UNSEEN_sc . read
                         , try (string "UIDNEXT") >> space >> many1 digit >>= return . UIDNEXT_sc . read
                         , string "UIDVALIDITY" >> space >> many1 digit >>= return . UIDVALIDITY_sc . read ]
    where parenWords = space >> between (char '(') (char ')') ((many1 $ noneOf " )") `sepBy1` space)


connectIMAPPort :: Integral a => String -> a -> IO IMAPConnection
connectIMAPPort hostname port = undefined
