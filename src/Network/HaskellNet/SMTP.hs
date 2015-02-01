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
> main = runNewSMTP "your.smtp.server.com" $ do
>           authenticate PLAIN "username" "password"
>           sendPlainTextMail "receiver@server.com" "sender@server.com" "subject" (T.pack "Hello! This is the mail body!")

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
      -- * Operations inside a SMTP monad
    , runSMTP
    , runSMTPStream
    )
    where

import Network.HaskellNet.BSStream
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.BSD (getHostName)
import Network

import Control.Applicative ((<$>))
import Control.Exception
{-import Control.Monad (unless)-}

import Data.Char (isDigit)

import Network.HaskellNet.Auth

import Network.Mail.Mime
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

import Control.Monad.Reader
import Control.Monad.Error

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

data SMTPConnection m = SMTPC { bsstream :: !(BSStream m), _response :: ![ByteString] }

type SMTP m a = ReaderT (SMTPConnection m) (ErrorT String m) a

runSMTP :: Monad m => SMTPConnection m -> SMTP m a -> m (Either String a)
runSMTP conn smtp = runErrorT $ runReaderT smtp conn

runSMTPStream :: MonadIO m => BSStream m -> SMTP m a -> m (Either String a)
runSMTPStream st smtp = connectStream st >>= flip runSMTP smtp

tryCommand :: Monad m => Command -> Int -> ReplyCode -> SMTP m ByteString
tryCommand cmd tries expectedReply = do
    (code, msg) <- sendCommand cmd
    case () of
        _ | code == expectedReply   -> return msg
        _ | tries > 1               ->
                tryCommand cmd (tries - 1) expectedReply
            | otherwise               -> do
                asks bsstream >>= lift. lift . bsClose
                fail $ "cannot execute command " ++ show cmd ++
                        ", expected reply code " ++ show expectedReply ++
                        ", but received " ++ show code ++ " " ++ BS.unpack msg

-- | create SMTPConnection from already connected Stream
connectStream :: MonadIO m => BSStream m -> m (SMTPConnection m)
connectStream st =
    do (code1, _) <- parseResponse st
       unless (code1 == 220) $
              do bsClose st
                 fail "cannot connect to the server"
       senderHost <- liftIO getHostName
       Right msg <- runSMTP (SMTPC st []) $ tryCommand (EHLO senderHost) 3 250
       return (SMTPC st (tail $ BS.lines msg))

parseResponse :: Monad m => BSStream m -> m (ReplyCode, ByteString)
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
sendCommand :: Monad m => Command -> SMTP m (ReplyCode, ByteString)
sendCommand (DATA dat) =
    do conn <- asks bsstream
       lift $ lift $ do
        bsPutCrLf conn $ BS.pack "DATA"
        (code, _) <- parseResponse conn
        unless (code == 354) $ fail "this server cannot accept any data."
        mapM_ (sendLine conn) $ BS.lines dat ++ [BS.pack "."]
        parseResponse conn
    where sendLine = bsPutCrLf
sendCommand (AUTH LOGIN username password) =
    do conn <- asks bsstream
       lift $ lift $ do
        bsPutCrLf conn command
        (_, _) <- parseResponse conn
        bsPutCrLf conn $ BS.pack userB64
        (_, _) <- parseResponse conn
        bsPutCrLf conn $ BS.pack passB64
        parseResponse conn
    where command = BS.pack "AUTH LOGIN"
          (userB64, passB64) = login username password
sendCommand (AUTH at username password) =
    do conn <- asks bsstream
       lift $ lift $ do
        bsPutCrLf conn command
        (code, msg) <- parseResponse conn
        unless (code == 334) $ fail "authentication failed."
        bsPutCrLf conn $ BS.pack $ auth at (BS.unpack msg) username password
        parseResponse conn
    where command = BS.pack $ unwords ["AUTH", show at]
sendCommand meth =
    do conn <- asks bsstream
       lift $ lift $ do
        bsPutCrLf conn $ BS.pack command
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
closeSMTP :: Monad m => SMTP m ()
closeSMTP = asks bsstream >>= lift . lift . bsClose

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
authenticate :: Monad m => AuthType -> UserName -> Password -> SMTP m ()
authenticate at username password  = do
    (code, _) <- sendCommand $ AUTH at username password
    unless (code == 235) $ throwError "Auth failed"

-- | sending a mail to a server. This is achieved by sendMessage.  If
-- something is wrong, it raises an IOexception.
sendMail :: Monad m => String     -- ^ sender mail
         -> [String]   -- ^ receivers
         -> ByteString -- ^ data
         -> SMTP m ()
sendMail sender receivers dat = do
                 sendAndCheck (MAIL sender)
                 mapM_ (sendAndCheck . RCPT) receivers
                 sendAndCheck (DATA dat)
                 return ()
  where
    -- Try the command once and @fail@ if the response isn't 250.
    sendAndCheck cmd = tryCommand cmd 1 250

-- | connecting SMTP server with the specified name and port number.
connectSMTPPort :: String     -- ^ name of the server
                -> PortNumber -- ^ port number
                -> IO (SMTPConnection IO)
connectSMTPPort hostname port =
    (handleToStream <$> connectTo hostname (PortNumber port))
    >>= connectStream

-- | connecting SMTP server with the specified name and port 25.
connectSMTP :: String     -- ^ name of the server
            -> IO (SMTPConnection IO)
connectSMTP = flip connectSMTPPort 25

-- | doSMTPPort open a connection, and do an IO action with the
-- connection, and then close it.
doSMTPPort :: String -> PortNumber -> (SMTPConnection IO -> IO a) -> IO a
doSMTPPort host port =
    bracket (connectSMTPPort host port) (bsClose . bsstream)

-- | doSMTP is similar to doSMTPPort, except that it does not require
-- port number but connects to the server with port 25.
doSMTP :: String -> (SMTPConnection IO -> IO a) -> IO a
doSMTP host = doSMTPPort host 25

-- | doSMTPStream is similar to doSMTPPort, except that its argument
-- is a Stream data instead of hostname and port number.
doSMTPStream :: BSStream IO -> (SMTPConnection IO -> IO a) -> IO a
doSMTPStream s = bracket (connectStream s) (bsClose . bsstream)

-- | Send a plain text mail.
sendPlainTextMail :: MonadIO m => String  -- ^ receiver
                  -> String  -- ^ sender
                  -> String  -- ^ subject
                  -> LT.Text -- ^ body
                  -> SMTP m ()
sendPlainTextMail to from subject body = do
    renderedMail <- liftIO $ renderMail' myMail
    sendMail from [to] (lazyToStrict renderedMail)
    where
        myMail = simpleMail' (address to) (address from) (T.pack subject) body
        address = Address Nothing . T.pack

-- | Send a mime mail.
sendMimeMail :: MonadIO m => String               -- ^ receiver
             -> String               -- ^ sender
             -> String               -- ^ subject
             -> LT.Text              -- ^ plain text body
             -> LT.Text              -- ^ html body
             -> [(T.Text, FilePath)] -- ^ attachments: [(content_type, path)]
             -> SMTP m ()
sendMimeMail to from subject plainBody htmlBody attachments =
        let address = Address Nothing . T.pack
        in do
            renderedMail <- liftIO $ do
                myMail <- simpleMail (address to) (address from) (T.pack subject)
                                     plainBody htmlBody attachments
                renderMail' myMail
            sendMail from [to] (lazyToStrict renderedMail)

-- haskellNet uses strict bytestrings
-- TODO: look at making haskellnet lazy
lazyToStrict :: B.ByteString -> S.ByteString
lazyToStrict = S.concat . B.toChunks

crlf :: BS.ByteString
crlf = BS.pack "\r\n"

bsPutCrLf :: Monad m => BSStream m -> ByteString -> m ()
bsPutCrLf h s = bsPut h s >> bsPut h crlf >> bsFlush h
