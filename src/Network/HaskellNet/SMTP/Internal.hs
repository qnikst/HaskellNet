{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
-- |
-- Internal functions that are used in the SMTP protocol,
-- you may need these module in case if you want to implement additional
-- functionality that does not exist in the "Network.HaskellNet.SMTP".
--
-- __Example__.
--
-- One example could be sending multiple emails over the same stream
-- in order to use that you may want to use 'RSET' command, so you can implement:
--
-- @
-- import "Network.HaskellNet.SMTP.Internal"
--
-- resetConnection :: SMTPConnection -> IO ()
-- resetConnection conn = do
--    (code, _) <- 'sendCommand' conn 'RSET'
--    'unless' (code == 250) $ 'throwIO' $ 'UnexpectedReply' 'RSET' [250] code ""
-- @
--
module Network.HaskellNet.SMTP.Internal
  ( SMTPConnection(..)
  , Command(..)
  , Response(..)
  , SMTPException(..)
  , ReplyCode
  , tryCommand
  , parseResponse
  , sendCommand
  , sendMail
  , closeSMTP
    -- * Reexports
  , Address(..)
  ) where

import Control.Exception
import Control.Monad (unless)
import Data.Char (isDigit)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable

import Network.HaskellNet.Auth
import Network.HaskellNet.BSStream

import Network.Mail.Mime

import Prelude

-- | All communication with server is done using @SMTPConnection@ value.
data SMTPConnection = SMTPC {
  -- | Connection communication channel.
  bsstream :: !BSStream,
  -- | Server properties as per reply to the 'EHLO' request.
  _response :: ![ByteString]
  }

-- | SMTP commands.
--
-- Supports basic and extended SMTP protocol without TLS support.
data Command
  = -- | The @HELO@ command initiates the SMTP session conversation. The client greets the server and introduces itself.
    -- As a rule, HELO is attributed with an argument that specifies the domain name or IP address of the SMTP client.
    --
    -- Success: 250
    -- Failure: 504, 550
    HELO T.Text
  | -- | @EHLO@ is an alternative to HELO for servers that support the SMTP service extensions (ESMTP)
    --
    -- Success: 250
    -- Failure: 502, 504, 550
    EHLO T.Text
  | -- | @MAIL FROM@ command initiates a mail transfer. As an argument, MAIL FROM includes a sender mailbox (reverse-path)
    -- can accept optional parameters.
    --
    -- Success: 250
    --
    -- Failure: 451, 452, 455, 503, 550, 552, 553, 555
    MAIL T.Text
  | -- | The @RCPT TO@ command specifies exactly one recipient.
    --
    -- Success: 250 251
    --
    -- Failure: 450 451 452 455 503 550 551 552 553 555
    RCPT T.Text
  | -- | With the @DATA@ command, the client asks the server for permission to transfer the mail data.
    --
    -- Success: 250, 354
    --
    -- Failure: 450 451 452 503 550 552 554
    --
    -- Client just sends data and after receiving 354 starts streaming email, terminating transfer by
    -- sending @\r\n.\r\n@.
    DATA ByteString
  | -- |
    -- @EXPN@ is used to verify whether a mailing list in the argument exists on the local host.
    -- The positive response will specify the membership of the recipients.
    --
    -- Success: 250 252
    --
    -- Failure: 502 504 550
    EXPN T.Text
  | -- |
    -- @VRFY@ is used to verify whether a mailbox in the argument exists on the local host.
    -- The server response includes the user’s mailbox and may include the user’s full name.
    --
    -- Success: 250 251 252
    --
    -- Failure: 502 504 550 551 553
    VRFY T.Text
  | -- |
    -- With the @HELP@ command, the client requests a list of commands the server supports, may request
    -- help for specific command
    --
    -- Success: 211 214
    --
    -- Failure: 502 504
    HELP T.Text
  | -- | Authorization support
    AUTH AuthType UserName Password
  | -- | @NOOP@  can be used to verify if the connection is alive
    --
    -- Success: 250
    NOOP
  | -- | @RSET@ Resets the state
    --
    -- Success: 250
    RSET
  | -- | @QUIT@ asks server to close connection. Client should terminate the connection when receives
    -- status.
    --
    -- Success: 221
    QUIT
    deriving (Show, Eq)

-- | Code reply from the server. It's always 3 digit integer.
type ReplyCode = Int

-- | Possible server response.
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

-- | Exceptions that can happen during communication.
data SMTPException
  = UnexpectedReply Command [ReplyCode] ReplyCode BS.ByteString
  | NotConfirmed ReplyCode BS.ByteString
  | AuthNegotiationFailed ReplyCode BS.ByteString
  deriving (Show)
  deriving (Typeable)

instance Exception SMTPException where
  displayException (UnexpectedReply cmd expected code msg) =
    "Cannot execute command " ++ show cmd ++
       ", " ++ prettyExpected expected ++
       ", " ++ prettyReceived code msg
    where
      prettyReceived :: Int -> ByteString -> String
      prettyReceived co ms = "but received" ++ show co ++ " (" ++ BS.unpack ms ++ ")"
      prettyExpected :: [ReplyCode] -> String
      prettyExpected [x] = "expected reply code of " ++ show x
      prettyExpected xs = "expected any reply code of " ++ show xs
  displayException (NotConfirmed code msg) =
    "This server cannot accept any data. code: " ++ show code ++ ", msg: " ++ BS.unpack msg
  displayException (AuthNegotiationFailed code msg) =
    "authentication failed. code: " ++ show code ++ ", msg: " ++ BS.unpack msg


-- | Safe wrapper for running a client command over the SMTP
-- connection.
--
-- /Note on current behavior/
--
-- We allow the command to fail several times, retry
-- happens in case if we have received unexpected status code.
-- In this case message will be sent again. However in case
-- of other synchronous or asynchronous exceptions there will
-- be no retries.
--
-- It case if number of retries were exceeded connection will
-- be closed automatically.
--
-- The behaviors in notes will likely be changed in the future
-- and should not be relied upon, see issues 76, 77.
tryCommand
  :: SMTPConnection -- ^ Connection
  -> Command -- ^ Supported command
  -> Int -- ^ Number of allowed retries
  -> [ReplyCode] -- ^ List of accepted codes
  -> IO ByteString -- ^ Resulting data
tryCommand conn cmd tries expectedReplies = do
    (code, msg) <- sendCommand conn cmd
    case () of
        _ | code `elem` expectedReplies -> return msg
        _ | tries > 1 ->
            tryCommand conn cmd (tries - 1) expectedReplies
          | otherwise ->
            throwIO $ UnexpectedReply cmd expectedReplies code msg

-- | Read response from the stream. Response consists of the code
-- and one or more lines of data.
--
-- In case if it's not the last line of reply the code is followed
-- by the '-' sign. We return the code and all the data with the code
-- stripped.
--
-- Eg.:
--
-- @
-- "250-8BITMIME\r"
-- "250-PIPELINING\r"
-- "250-SIZE 42991616\r"
-- "250-AUTH LOGIN PLAIN XOAUTH2\r"
-- "250-DSN\r"
-- "250 ENHANCEDSTATUSCODES\r"
-- @
--
-- Returns:
--
-- @
-- (250, "8BITMIME\\nPIPELINING\nSIZE 42991616\\nAUTH LOGIN PLAIN XOAUTH2\\nDSN\\nENHANCEDSTATUSCODES")
-- @
--
-- Throws 'SMTPException'.
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

-- | Sends a 'Command' to the server. Function that performs all the logic
-- for sending messages. Throws an exception if something goes wrong.
--
-- Throws 'SMTPException'.
sendCommand
  :: SMTPConnection
  -> Command
  -> IO (ReplyCode, ByteString)
sendCommand (SMTPC conn _) (DATA dat) =
    do bsPutCrLf conn "DATA"
       (code, msg) <- parseResponse conn
       unless (code == 354) $ throwIO $ NotConfirmed code msg
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
    where command = "AUTH LOGIN"
          (userB64, passB64) = login username password
sendCommand (SMTPC conn _) (AUTH at username password) =
    do bsPutCrLf conn $ T.encodeUtf8 command
       (code, msg) <- parseResponse conn
       unless (code == 334) $ throwIO $ AuthNegotiationFailed code msg
       bsPutCrLf conn $ BS.pack $ auth at (BS.unpack msg) username password
       parseResponse conn
    where command = T.unwords ["AUTH", T.pack (show at)]
sendCommand (SMTPC conn _) meth =
    do bsPutCrLf conn $! T.encodeUtf8 command
       parseResponse conn
    where command = case meth of
                      (HELO param) -> "HELO " <> param
                      (EHLO param) -> "EHLO " <> param
                      (MAIL param) -> "MAIL FROM:<" <> param <> ">"
                      (RCPT param) -> "RCPT TO:<" <> param <> ">"
                      (EXPN param) -> "EXPN " <> param
                      (VRFY param) -> "VRFY " <> param
                      (HELP msg)   -> if T.null msg
                                      then "HELP\r\n"
                                      else "HELP " <> msg
                      NOOP         -> "NOOP"
                      RSET         -> "RSET"
                      QUIT         -> "QUIT"
                      (DATA _)     ->
                          error "BUG: DATA pattern should be matched by sendCommand patterns"
                      (AUTH {})     ->
                          error "BUG: AUTH pattern should be matched by sendCommand patterns"

-- | Closes the connection.  This function send the QUIT method, so you
-- do not have to QUIT method explicitly.
closeSMTP :: SMTPConnection -> IO ()
closeSMTP (SMTPC conn _) = bsClose conn

-- | Sends a mail to the server.
--
-- Throws 'SMTPException'.
sendMail :: Address -- ^ sender mail
         -> [Address] -- ^ receivers
         -> ByteString -- ^ data
         -> SMTPConnection
         -> IO ()
sendMail sender receivers dat conn = do
                 sendAndCheck (MAIL (addressEmail sender))
                 mapM_ (sendAndCheck . RCPT . addressEmail) receivers
                 sendAndCheck (DATA dat)
                 return ()
  where
    -- Try the command once and @fail@ if the response isn't 250.
    sendAndCheck cmd = tryCommand conn cmd 1 [250, 251]

-- | Just a crlf constant.
crlf :: BS.ByteString
crlf = BS.pack "\r\n"

-- | Write a message ending with ctlf.
bsPutCrLf :: BSStream -> ByteString -> IO ()
bsPutCrLf h s = bsPut h (s <> crlf)
