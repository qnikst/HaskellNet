{-# LANGUAGE ScopedTypeVariables #-}
{- |

This module provides functions client side of the SMTP protocol.

A basic usage example:

@
\{\-\# LANGUAGE OverloadedStrings \#\-\}
import "Network.HaskellNet.SMTP"
import "Network.HaskellNet.Auth"
import qualified "Data.Text.Lazy" as T
import "System.Exit" (die)

main :: IO ()
main = 'doSMTP' "your.smtp.server.com" $ \\conn -> do -- (1)
   authSucceed <- 'authenticate' 'PLAIN' "username" "password" conn -- (2)
   if authSucceed
   then 'sendPlainTextMail'
          "receiver\@server.com"
          "sender\@server.com"
          "subject"
          "Hello! This is the mail body!"
          conn
   else die "Authentication failed."
@

Notes for the above example:

   * @(1)@ First the @conn@::'SMTPConnection' is opened with the 'doSMTP' function,
     we can use it to communicate with @SMTP@ server. See managing connection for more options..
   * @(2)@ The 'authenticate' function authenticates to the server with the specified 'AuthType'.
     It returns a 'Bool' indicating either the authentication succeed or not.
     See authentication section..
   * @(3)@ The 'sendPlainTextMail' is used to send a email a plain text email. See sending emails to see all
     the functions.
   * __N.B.__ For /SSL\/TLS/ support you may establish the connection using
     the functions (such as @connectSMTPSSL@) provided by the @Network.HaskellNet.SMTP.SSL@ module
     of the <http://hackage.haskell.org/package/HaskellNet-SSL HaskellNet-SSL> package.
-}
module Network.HaskellNet.SMTP
    ( -- * Workflow
      -- $workflow

      -- ** Controlling connections
      -- $controlling-connections
      SMTPConnection
      -- $controlling-connections-1
    , doSMTPPort
    , doSMTP
    , doSMTPStream
      -- $controlling-connections-2

      -- ** Authentication
    , authenticate
      -- $authentication
    , AuthType(..)

      -- ** Sending emails
      -- $sending-mail
    , sendPlainTextMail
    , sendMimeMail
    , sendMimeMail'
    , sendMimeMail2
      -- * Low level commands
      -- ** Establishing Connection
      -- $low-level-connection
    , connectSMTPPort
    , connectSMTP
    , connectStream
    , closeSMTP
    , sendMail
      -- ** Operation to a Connection
    , sendCommand
    , Command(..)
    , Response(..)
    )
    where

import Network.HaskellNet.BSStream
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.BSD (getHostName)
import Network.Socket
import Network.Compat

import Control.Applicative
import Control.Exception
import Control.Monad (unless, when)

import Data.Char (isDigit)

import Network.HaskellNet.Auth

import Network.Mail.Mime
import qualified Data.ByteString.Lazy as B

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

import GHC.Stack
import Prelude


-- $workflow
-- The common workflow while working with the library is:
--
--   1. Establish a new connection
--   2. Authenticate to the server
--   3. Perform message sending
--   4. Close connections
--
-- Steps 1 and 4 are combined together using @bracket@-like API, other than that
-- the documentation sections are structured according to this workflow.

-- $controlling-connections

-- $controlling-connections-1
-- The library encourages creation of 'SMTPConnection' using the @doSMTP@-family functions.
-- These functions provide @bracket@-like pattern that manages connection state:
-- creates a connection, passes it to the user defined @IO@ action and frees connection
-- when the action exits. This approach is simple and exception safe.
--
-- __N.B.__ It should be noted that none of these functions implements keep alive of any kind,
-- so the server is free to close the connection by timeout even the end of before the users
-- action exits.
--

-- $controlling-connections-2
--
-- __NOTE:__ For /SSL\/TLS/ support you may establish the connection using
-- the functions (such as @connectSMTPSSL@) provided by the @Network.HaskellNet.SMTP.SSL@ module
-- of the <http://hackage.haskell.org/package/HaskellNet-SSL HaskellNet-SSL> package.
--
-- @bracket-@ style is not the only possible style for resource management,
-- it's possible to use <http://hackage.haskell.org/package/resourcet resourcet> or
-- <http://hackage.haskell.org/package/resource-pool resource-pool> as well. In both of the
-- approaches you need to use low-level 'connectSTM*' and 'closeSMTP' functions.
--
-- Basic example using @resourcet@.
--
-- @
-- \{\-\# LANGUAGE OverloadedStrings \#\-\}
-- import "Network.HaskellNet.SMTP"
-- import "Network.HaskellNet.Auth"
-- import "Control.Monad.Trans.Resource"
-- import qualified "Data.Text.Lazy" as T
-- import "System.Exit" (die)
--
-- main :: IO ()
-- main = 'Control.Monad.Trans.Resource.runResourceT' $ do
--    (key, conn)
--        <- 'Control.Monad.Trans.Resource.allocate'
--               ('connectSMTP' "your.smtp.server.com")
--               (closeSMTP)
--    ... conn
-- @
-- This approach allows resource management even if the code does not form
-- a stack.
--


-- The response field seems to be unused. It's saved at one place, but never
-- retrieved.

-- | All communication with server is done using @SMTPConnection@ value.
data SMTPConnection = SMTPC { bsstream :: !BSStream, _response :: ![ByteString] }

data Command = HELO String
             | EHLO String
             | MAIL String
             | RCPT String
             | DATA ByteString
             | EXPN String
             | VRFY String
             | HELP String
             | AUTH AuthType UserName Password
             | NOOP
             | RSET
             | QUIT
               deriving (Show, Eq)

type ReplyCode = Int

data Response = Ok
              | SystemStatus
              | HelpMessage
              | ServiceReady
              | ServiceClosing
              | UserNotLocal
              | CannotVerify
              | StartMailInput
              | ServiceNotAvailable
              | MailboxUnavailable
              | ErrorInProcessing
              | InsufficientSystemStorage
              | SyntaxError
              | ParameterError
              | CommandNotImplemented
              | BadSequence
              | ParameterNotImplemented
              | MailboxUnavailableError
              | UserNotLocalError
              | ExceededStorage
              | MailboxNotAllowed
              | TransactionFailed
                deriving (Show, Eq)

-- | connecting SMTP server with the specified name and port number.
connectSMTPPort :: String     -- ^ name of the server
                -> PortNumber -- ^ port number
                -> IO SMTPConnection
connectSMTPPort hostname port =
    (handleToStream <$> connectTo hostname port)
    >>= connectStream

-- | connecting SMTP server with the specified name and port 25.
connectSMTP :: String     -- ^ name of the server
            -> IO SMTPConnection
connectSMTP = flip connectSMTPPort 25

tryCommand :: SMTPConnection -> Command -> Int -> [ReplyCode]
           -> IO ByteString
tryCommand conn cmd tries expectedReplies = do
    (code, msg) <- sendCommand conn cmd
    case () of
        _ | code `elem` expectedReplies -> return msg
        _ | tries > 1 ->
            tryCommand conn cmd (tries - 1) expectedReplies
          | otherwise -> do
            bsClose (bsstream conn)
            fail $ "cannot execute command " ++ show cmd ++
                ", " ++ prettyExpected expectedReplies ++
                ", " ++ prettyReceived code msg

  where
    prettyReceived :: Int -> ByteString -> String
    prettyReceived co ms = "but received" ++ show co ++ " (" ++ BS.unpack ms ++ ")"

    prettyExpected :: [ReplyCode] -> String
    prettyExpected [x] = "expected reply code of " ++ show x
    prettyExpected xs = "expected any reply code of " ++ show xs

-- | create SMTPConnection from already connected Stream
connectStream :: BSStream -> IO SMTPConnection
connectStream st =
    do (code1, _) <- parseResponse st
       unless (code1 == 220) $
              do bsClose st
                 fail "cannot connect to the server"
       senderHost <- getHostName
       msg <- tryCommand (SMTPC st []) (EHLO senderHost) 3 [250]
       return (SMTPC st (tail $ BS.lines msg))

parseResponse :: BSStream -> IO (ReplyCode, ByteString)
parseResponse st =
    do (code, bdy) <- readLines
       return (read $ BS.unpack code, BS.unlines bdy)
    where readLines =
              do l <- bsGetLine st
                 let (c, bdy) = BS.span isDigit l
                 if not (BS.null bdy) && BS.head bdy == '-'
                    then do (c2, ls) <- readLines
                            return (c2, BS.tail bdy:ls)
                    else return (c, [BS.tail bdy])


-- | send a method to a server
sendCommand :: SMTPConnection -> Command -> IO (ReplyCode, ByteString)
sendCommand (SMTPC conn _) (DATA dat) =
    do bsPutCrLf conn $ BS.pack "DATA"
       (code, msg) <- parseResponse conn
       unless (code == 354) $ fail $ "this server cannot accept any data. code: " ++ show code ++ ", msg: " ++ BS.unpack msg
       mapM_ (sendLine . stripCR) $ BS.lines dat ++ [BS.pack "."]
       parseResponse conn
    where sendLine = bsPutCrLf conn
          stripCR bs = case BS.unsnoc bs of
                         Just (line, '\r') -> line
                         _                 -> bs
sendCommand (SMTPC conn _) (AUTH LOGIN username password) =
    do bsPutCrLf conn command
       (_, _) <- parseResponse conn
       bsPutCrLf conn $ BS.pack userB64
       (_, _) <- parseResponse conn
       bsPutCrLf conn $ BS.pack passB64
       parseResponse conn
    where command = BS.pack "AUTH LOGIN"
          (userB64, passB64) = login username password
sendCommand (SMTPC conn _) (AUTH at username password) =
    do bsPutCrLf conn command
       (code, msg) <- parseResponse conn
       unless (code == 334) $ fail $ "authentication failed. code: " ++ show code ++ ", msg: " ++ BS.unpack msg
       bsPutCrLf conn $ BS.pack $ auth at (BS.unpack msg) username password
       parseResponse conn
    where command = BS.pack $ unwords ["AUTH", show at]
sendCommand (SMTPC conn _) meth =
    do bsPutCrLf conn $ BS.pack command
       parseResponse conn
    where command = case meth of
                      (HELO param) -> "HELO " ++ param
                      (EHLO param) -> "EHLO " ++ param
                      (MAIL param) -> "MAIL FROM:<" ++ param ++ ">"
                      (RCPT param) -> "RCPT TO:<" ++ param ++ ">"
                      (EXPN param) -> "EXPN " ++ param
                      (VRFY param) -> "VRFY " ++ param
                      (HELP msg)   -> if null msg
                                        then "HELP\r\n"
                                        else "HELP " ++ msg
                      NOOP         -> "NOOP"
                      RSET         -> "RSET"
                      QUIT         -> "QUIT"
                      (DATA _)     ->
                          error "BUG: DATA pattern should be matched by sendCommand patterns"
                      (AUTH {})     ->
                          error "BUG: AUTH pattern should be matched by sendCommand patterns"

-- | close the connection.  This function send the QUIT method, so you
-- do not have to QUIT method explicitly.
closeSMTP :: SMTPConnection -> IO ()
closeSMTP (SMTPC conn _) = bsClose conn

{-
I must be being stupid here

I can't seem to be able to catch the exception arising from the
connection already being closed this would be the correct way to do it
but instead we're being naughty above by just closes the connection
without first sending QUIT

closeSMTP c@(SMTPC conn _) =
    do sendCommand c QUIT
       bsClose conn `catch` \(_ :: IOException) -> return ()
-}


-- $authentication

{- |
Authenticates user on the remote server. Returns 'True' if the authentication succeeds,
otherwise returns 'False'.

Usage example:

@
\{\-\# LANGUAGE OverloadedStrings \#\-\}
authSucceed <- 'authenticate' 'PLAIN' "username" "password" conn
if authSucceed
then 'sendPlainTextMail' "receiver\@server.com" "sender\@server.com" "subject" "Hello!" conn
else 'print' "Authentication failed."
@
-}
authenticate :: AuthType -> UserName -> Password -> SMTPConnection -> IO Bool
authenticate at username password conn  = do
        (code, _) <- sendCommand conn $ AUTH at username password
        return (code == 235)

-- $authentication
-- __N.B.__ The choice of the authentication method is currently explicit and the library
-- does not analyze server capabilities reply for choosing the right method.
--


-- | sending a mail to a server. This is achieved by sendMessage.  If
-- something is wrong, it raises an IOexception.
sendMail :: String     -- ^ sender mail
         -> [String]   -- ^ receivers
         -> ByteString -- ^ data
         -> SMTPConnection
         -> IO ()
sendMail sender receivers dat conn = do
                 sendAndCheck (MAIL sender)
                 mapM_ (sendAndCheck . RCPT) receivers
                 sendAndCheck (DATA dat)
                 return ()
  where
    -- Try the command once and @fail@ if the response isn't 250.
    sendAndCheck cmd = tryCommand conn cmd 1 [250, 251]

-- | 'doSMTPPort' opens a connection to the given port server and
-- performs an IO action with the connection, and then close it.
--
-- 'SMTPConnection' is freed once 'IO' action scope is finished, it means that
-- 'SMTPConnection' value should not escape the action scope.
doSMTPPort :: String -> PortNumber -> (SMTPConnection -> IO a) -> IO a
doSMTPPort host port =
    bracket (connectSMTPPort host port) closeSMTP

-- | 'doSMTP' is similar to 'doSMTPPort', except that it does not require
-- port number and connects to the default SMTP port — 25.
doSMTP :: String -> (SMTPConnection -> IO a) -> IO a
doSMTP host = doSMTPPort host 25

-- | 'doSMTPStream' is similar to 'doSMTPPort', except that its argument
-- is a Stream data instead of hostname and port number. Using this function
-- you can embed connections maintained by the other libraries or add debug info
-- in a common  way.
--
-- Using this function you can create an 'SMTPConnection' from an already
-- opened connection stream. See more info on the 'BStream' abstraction in the
-- "Network.HaskellNet.BSStream" module.
doSMTPStream :: BSStream -> (SMTPConnection -> IO a) -> IO a
doSMTPStream s = bracket (connectStream s) closeSMTP

-- $sending-mail
-- For sending emails there is a family of @sendMime@ functions, that wraps
-- the low-level mime-mail ones.
--
-- +------------------------+------------+----------+-------------+
-- | Method                 | Plain text | Html body| Attachments |
-- |                        | body       |          |             |
-- +========================+============+==========+=============+
-- | 'sendPlainTextMail'    |     ✓      |    ✗     |     ✗       |
-- +------------------------+------------+----------+-------------+
-- | 'sendMimeMail'         |     ✓      |    ✓     | ✓ (filepath)|
-- +------------------------+------------+----------+-------------+
-- | 'sendMimeMail''        |    ✓       |    ✓     | ✓  (memory) |
-- +------------------------+------------+----------+-------------+
-- | 'sendMimeMail2'        |      Uses internal 'Mail' type      |
-- +------------------------+-------------------------------------+

-- | Send a plain text mail.
sendPlainTextMail :: String  -- ^ receiver
                  -> String  -- ^ sender
                  -> String  -- ^ subject
                  -> LT.Text -- ^ body
                  -> SMTPConnection -- ^ the connection
                  -> IO ()
sendPlainTextMail to from subject body con = do
    renderedMail <- renderMail' myMail
    sendMail from [to] (B.toStrict renderedMail) con
    where
        myMail = simpleMail' (address to) (address from) (T.pack subject) body
        address = Address Nothing . T.pack


-- | Send a mime mail. The attachments are included with the file path.
sendMimeMail :: String               -- ^ receiver
             -> String               -- ^ sender
             -> String               -- ^ subject
             -> LT.Text              -- ^ plain text body
             -> LT.Text              -- ^ html body
             -> [(T.Text, FilePath)] -- ^ attachments: [(content_type, path)]
             -> SMTPConnection
             -> IO ()
sendMimeMail to from subject plainBody htmlBody attachments con = do
  myMail <- simpleMail (address to) (address from) (T.pack subject)
            plainBody htmlBody attachments
  renderedMail <- renderMail' myMail
  sendMail from [to] (B.toStrict renderedMail) con
  where
    address = Address Nothing . T.pack

-- | Send a mime mail. The attachments are included with in-memory 'ByteString'.
sendMimeMail' :: String                         -- ^ receiver
              -> String                         -- ^ sender
              -> String                         -- ^ subject
              -> LT.Text                        -- ^ plain text body
              -> LT.Text                        -- ^ html body
              -> [(T.Text, T.Text, B.ByteString)] -- ^ attachments: [(content_type, file_name, content)]
              -> SMTPConnection
              -> IO ()
sendMimeMail' to from subject plainBody htmlBody attachments con = do
  let myMail = simpleMailInMemory (address to) (address from) (T.pack subject)
                                  plainBody htmlBody attachments
  sendMimeMail2 myMail con
  where
    address = Address Nothing . T.pack

-- | Sends email in generated using 'mime-mail' package.
--
-- Throws 'UserError' @::@ 'IOError' if recipient address not specified.
sendMimeMail2 :: HasCallStack => Mail -> SMTPConnection -> IO ()
sendMimeMail2 mail con = do
    let (Address _ from) = mailFrom mail
        recps = map (T.unpack . addressEmail)
                     $ (mailTo mail ++ mailCc mail ++ mailBcc mail)
    when (null recps) $ fail "no receiver specified."
    renderedMail <- renderMail' $ mail { mailBcc = [] }
    sendMail (T.unpack from) recps (B.toStrict renderedMail) con

crlf :: BS.ByteString
crlf = BS.pack "\r\n"

bsPutCrLf :: BSStream -> ByteString -> IO ()
bsPutCrLf h s = bsPut h s >> bsPut h crlf >> bsFlush h
