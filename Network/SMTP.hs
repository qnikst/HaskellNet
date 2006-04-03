----------------------------------------------------------------------
-- |
-- Module      :  Network.SMTP
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  unstable
-- Portability :  portable
-- 
-- SMTP client implementation
-- 

module Network.SMTP
    ( SMTPMethods(..)
    , SMTPResponse(..)
    , Connect
    , connectSMTPPort
    , connectSMTP
    , sendCommand
    , closeSMTP
    , sendMail
    , doSMTPPort
    , doSMTP
    )
    where

import Network.BSD
import Network.Socket

import Control.Exception
import Control.Monad

import Data.List

import Base64

import Prelude hiding (catch)

type Connect = Socket

data AuthType = PLAIN_AUTH

data Command = HELO String
             | EHLO String
             | MAIL String
             | RCPT String
             | DATA String
             | EXPN String
             | VRFY String
             | HELP String
             | AUTH AuthType String {-^ user name ^-} String {-^ password -}
             | NOOP
             | RSET
             | QUIT
               deriving Show

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

codeToResponse :: Num a => a -> Response
codeToResponse 211 = SystemStatus
codeToResponse 214 = HelpMessage
codeToResponse 220 = ServiceReady
codeToResponse 221 = ServiceClosing
codeToResponse 250 = Ok
codeToResponse 251 = UserNotLocal
codeToResponse 252 = CannotVerify
codeToResponse 354 = StartMailInput
codeToResponse 421 = ServiceNotAvailable
codeToResponse 450 = MailboxUnavailable
codeToResponse 451 = ErrorInProcessing
codeToResponse 452 = InsufficientSystemStorage
codeToResponse 500 = SyntaxError
codeToResponse 501 = ParameterError
codeToResponse 502 = CommandNotImplemented
codeToResponse 503 = BadSequence
codeToResponse 504 = ParameterNotImplemented
codeToResponse 550 = MailboxUnavailableError
codeToResponse 551 = UserNotLocalError
codeToResponse 552 = ExceededStorage
codeToResponse 553 = MailboxNotAllowed
codeToResponse 554 = TransactionFailed

crlf = "\r\n"

connectSMTPPort :: Integral a => String -> a -> IO Connect
connectSMTPPort hostname port =
    do sock <- socket AF_INET Stream 6
       host <- inet_addr hostname `catch`
               (\_ -> getHostByName hostname >>= return . head . hostAddresses)
       connect sock (SockAddrInet (fromInteger $ toInteger port) host)
       (code, msg) <- parseResponse sock
       unless (code == 220) $
              do sClose sock
                 fail "cannot connect to server"
       senderHost <- getHostName
       (code, msg) <- sendCommand (Connection sock []) (EHLO senderHost)
       unless (code == 250) $
              do (code, msg) <- sendCommand (Connection sock [])
                                  (HELO senderHost)
                 unless (code == 250) $
                        do sClose sock
                           fail "cannot connect to server"
       return (Connection sock (tail $ lines msg))

connectSMTP :: String -> IO Connect
connectSMTP = flip connectSMTPPort 25

sendMessage :: Connect -> SMTPMethods -> IO (SMTPResponse, String)
sendMessage sock (DATA dat) =
    do ssize <- send sock $ "DATA\r\n"
       unless (ssize == 6) $ fail "cannot send method DATA"
       (code, msg) <- parseResponse sock
       unless (code == 354) $ fail "this server cannot accept any data."
       mapM_ sendLine $ lines dat ++ ["."]
       parseResponse sock
    where sendLine l = do ssize <- send sock (l ++ crlf)
                          unless (ssize == length l + 2) $
                                 fail "cannot send data."
sendCommand (Connection sock _) (AUTH PLAIN_AUTH username password) =
    do ssize <- send sock command
       unless (ssize == length command) $ fail "cannot send data."
       parseResponse sock
    where command = "AUTH PLAIN " ++ b64Encode (concat $ intersperce "\0" [username, username, password])
sendCommand (Connection sock _) meth =
    do ssize <- send sock command
       unless (ssize == length command) $ fail "cannot send data."
       parseResponse sock
    where command = case meth of
                      (HELO param) -> "HELO " ++ param ++ crlf
                      (EHLO param) -> "EHLO " ++ param ++ crlf
                      (MAIL param) -> "MAIL FROM:<" ++ param ++ ">" ++ crlf
                      (RCPT param) -> "RCPT TO:<" ++ param ++ ">" ++ crlf
                      (EXPN param) -> "EXPN " ++ param ++ crlf
                      (VRFY param) -> "VRFY " ++ param ++ crlf
                      (HELP msg)   -> if null msg
                                        then "HELP\r\n"
                                        else "HELP " ++ msg ++ crlf
                      NOOP         -> "NOOP\r\n"
                      RSET         -> "RSET\r\n"
                      QUIT          -> "QUIT\r\n"

closeSMTP :: Connect -> IO ()
closeSMTP conn = do sendMessage conn QUIT
                    sClose conn

-- | 
-- sending a mail to a server. This is achieved by sendMessage.  If
-- something is wrong, it raises an IOexception.
sendMail :: String   -- ^ sender mail
         -> [String] -- ^ receivers
         -> String   -- ^ data
         -> Connect
         -> IO ()
sendMail sender receivers dat conn =
    catcher `handle` mainProc
    where mainProc =  do (250, _) <- sendCommand conn (MAIL sender)
                         vals <- mapM (sendCommand conn . RCPT) receivers
                         unless (all ((==250) . fst) vals) $ fail "sendMail error"
                         (250, _) <- sendCommand conn (DATA dat)
                         return ()
          catcher e@(PatternMatchFail _) = fail "sendMail error"
          catcher e = throwIO e

-- | 
-- doSMTPPort open a connection, and do an IO action with the
-- connection, and then close it.
doSMTPPort :: Integral a => String -> a -> (Connect -> IO b) -> IO b
doSMTPPort host port execution =
    bracket (connectSMTPPort host port) closeSMTP execution

doSMTP :: String -> (Connect -> IO a) -> IO a
doSMTP host execution = doSMTPPort host 25 execution
