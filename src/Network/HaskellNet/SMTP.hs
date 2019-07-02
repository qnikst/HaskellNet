{-# LANGUAGE ScopedTypeVariables #-}
{- |

This module provides functions for working with the SMTP protocol in the client side,
including /opening/ and /closing/ connections, /sending commands/ to the server,
/authenticate/ and /sending mails/.

Here's a basic usage example:

>
> import Network.HaskellNet.SMTP
> import Network.HaskellNet.Auth
> import qualified Data.Text.Lazy as T
>
> main = doSMTP "your.smtp.server.com" $ \conn ->
>    authSucceed <- authenticate PLAIN "username" "password" conn
>    if authSucceed
>        then sendPlainTextMail "receiver@server.com" "sender@server.com" "subject" (T.pack "Hello! This is the mail body!") conn
>        else print "Authentication failed."

Notes for the above example:

   * First the 'SMTPConnection' is opened with the 'doSMTP' function.
     The connection should also be established with functions such as 'connectSMTP',
     'connectSMTPPort' and 'doSMTPPort'.
     With the @doSMTP*@ functions the connection is opened, then executed an action
     with it and then closed automatically.
     If the connection is opened with the @connectSMTP*@ functions you may want to
     close it with the 'closeSMTP' function after using it.
     It is also possible to create a 'SMTPConnection' from an already opened connection
     stream ('BSStream') using the 'connectStream' or 'doSMTPStream' functions.

     /NOTE:/ For /SSL\/TLS/ support you may establish the connection using
             the functions (such as @connectSMTPSSL@) provided in the
             @Network.HaskellNet.SMTP.SSL@ module of the
             <http://hackage.haskell.org/package/HaskellNet-SSL HaskellNet-SSL>
             package.

   * The 'authenticate' function authenticates to the server with the specified 'AuthType'.
     'PLAIN', 'LOGIN' and 'CRAM_MD5' 'AuthType's are available. It returns a 'Bool'
     indicating either the authentication succeed or not.


   * To send a mail you can use 'sendPlainTextMail' for plain text mail, or 'sendMimeMail'
     for mime mail.
-}
module Network.HaskellNet.SMTP
    ( -- * Types
      Command(..)
    , Response(..)
    , AuthType(..)
    , SMTPConnection
      -- * Establishing Connection
    , connectSMTPPort
    , connectSMTP
    , connectStream
      -- * Operation to a Connection
    , sendCommand
    , closeSMTP
      -- * Other Useful Operations
    , authenticate
    , sendMail
    , doSMTPPort
    , doSMTP
    , doSMTPStream
    , sendPlainTextMail
    , sendMimeMail
    , sendMimeMail'
    , sendMimeMail2
    )
    where

import Network.HaskellNet.BSStream
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.BSD (getHostName)
import Network.Socket
import Network.Compat

import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad (unless, when)

import Data.Char (isDigit)

import Network.HaskellNet.Auth

import Network.Mail.Mime
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

-- The response field seems to be unused. It's saved at one place, but never
-- retrieved.
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

tryCommand :: SMTPConnection -> Command -> Int -> ReplyCode
           -> IO ByteString
tryCommand conn cmd tries expectedReply = do
  (code, msg) <- sendCommand conn cmd
  case () of
    _ | code == expectedReply   -> return msg
    _ | tries > 1               ->
          tryCommand conn cmd (tries - 1) expectedReply
      | otherwise               -> do
          bsClose (bsstream conn)
          fail $ "cannot execute command " ++ show cmd ++
                 ", expected reply code " ++ show expectedReply ++
                 ", but received " ++ show code ++ " " ++ BS.unpack msg

-- | create SMTPConnection from already connected Stream
connectStream :: BSStream -> IO SMTPConnection
connectStream st =
    do (code1, _) <- parseResponse st
       unless (code1 == 220) $
              do bsClose st
                 fail "cannot connect to the server"
       senderHost <- getHostName
       msg <- tryCommand (SMTPC st []) (EHLO senderHost) 3 250
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
       (code, _) <- parseResponse conn
       unless (code == 354) $ fail "this server cannot accept any data."
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
       unless (code == 334) $ fail $ "authentication failed: " ++ (BS.unpack msg)
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

{- |
This function will return 'True' if the authentication succeeds.
Here's an example of sending a mail with a server that requires
authentication:

>    authSucceed <- authenticate PLAIN "username" "password" conn
>    if authSucceed
>        then sendPlainTextMail "receiver@server.com" "sender@server.com" "subject" (T.pack "Hello!") conn
>        else print "Authentication failed."
-}
authenticate :: AuthType -> UserName -> Password -> SMTPConnection -> IO Bool
authenticate at username password conn  = do
        (code, _) <- sendCommand conn $ AUTH at username password
        return (code == 235)

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
    sendAndCheck cmd = tryCommand conn cmd 1 250

-- | doSMTPPort open a connection, and do an IO action with the
-- connection, and then close it.
doSMTPPort :: String -> PortNumber -> (SMTPConnection -> IO a) -> IO a
doSMTPPort host port =
    bracket (connectSMTPPort host port) closeSMTP

-- | doSMTP is similar to doSMTPPort, except that it does not require
-- port number but connects to the server with port 25.
doSMTP :: String -> (SMTPConnection -> IO a) -> IO a
doSMTP host = doSMTPPort host 25

-- | doSMTPStream is similar to doSMTPPort, except that its argument
-- is a Stream data instead of hostname and port number.
doSMTPStream :: BSStream -> (SMTPConnection -> IO a) -> IO a
doSMTPStream s = bracket (connectStream s) closeSMTP

-- | Send a plain text mail.
sendPlainTextMail :: String  -- ^ receiver
                  -> String  -- ^ sender
                  -> String  -- ^ subject
                  -> LT.Text -- ^ body
                  -> SMTPConnection -- ^ the connection
                  -> IO ()
sendPlainTextMail to from subject body con = do
    renderedMail <- renderMail' myMail
    sendMail from [to] (lazyToStrict renderedMail) con
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
  sendMail from [to] (lazyToStrict renderedMail) con
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

sendMimeMail2 :: Mail -> SMTPConnection -> IO ()
sendMimeMail2 mail con = do
    let (Address _ from) = mailFrom mail
        recps = map (T.unpack . addressEmail)
                     $ (mailTo mail ++ mailCc mail ++ mailBcc mail)
    when (null recps) $ fail "no receiver specified."
    renderedMail <- renderMail' $ mail { mailBcc = [] }
    sendMail (T.unpack from) recps (lazyToStrict renderedMail) con

-- haskellNet uses strict bytestrings
-- TODO: look at making haskellnet lazy
lazyToStrict :: B.ByteString -> S.ByteString
lazyToStrict = B.toStrict

crlf :: BS.ByteString
crlf = BS.pack "\r\n"

bsPutCrLf :: BSStream -> ByteString -> IO ()
bsPutCrLf h s = bsPut h s >> bsPut h crlf >> bsFlush h
