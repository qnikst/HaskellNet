-- |
-- Internal functions that are used in the SMTP protocol,
-- you may need these module in case if you want to implement additional
-- functionality that does not exist in the "Network.HaskellNet.SMTP".
module Network.HaskellNet.SMTP.Internal
  ( SMTPConnection(..)
  , Command(..)
  , Response(..)
  , ReplyCode
  , tryCommand
  , parseResponse
  , sendCommand
  , sendMail
  , closeSMTP
  ) where

import Control.Monad (unless)
import Data.Char (isDigit)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Network.HaskellNet.Auth
import Network.HaskellNet.BSStream

import Prelude

-- | All communication with server is done using @SMTPConnection@ value.
data SMTPConnection = SMTPC {
  -- | Connection communication channel.
  bsstream :: !BSStream,
  -- | Server properties as per reply to the 'EHLO' request.
  _response :: ![ByteString]
  }

-- | SMT commands.
--
-- Supports basic and extended SMTP protocol without TLS support.
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
-- and should not be relied upon, see issues #76, #77.
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
sendCommand
  :: SMTPConnection
  -> Command
  -> IO (ReplyCode, ByteString)
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

-- | Closes the connection.  This function send the QUIT method, so you
-- do not have to QUIT method explicitly.
closeSMTP :: SMTPConnection -> IO ()
closeSMTP (SMTPC conn _) = bsClose conn

-- | Sends a mail to the server. This is achieved by the 'tryCommand'.
-- If something is wrong, it raises an 'IOException'.
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

-- | Just a crlf constant.
crlf :: BS.ByteString
crlf = BS.pack "\r\n"

-- | Write a message ending with ctlf.
bsPutCrLf :: BSStream -> ByteString -> IO ()
bsPutCrLf h s = bsPut h (s <> crlf)
