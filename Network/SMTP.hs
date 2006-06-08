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
    , SMTPConnection
    , connectSMTPPort
    , connectSMTP
    , sendMessage
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

import Prelude hiding (catch)

type SMTPConnection = Socket

data SMTPMethods = HELO String -- ^ String means a hostname
                 | MAIL String -- ^ String means the sender name (does not required `from:')
                 | RCPT String -- ^ String means the receiver name
                 | DATA String -- ^ String means the mail data
                 | RSET
                 | QUIT

data SMTPResponse = Ok
                  | Quit
                  | NotImplemented
                    deriving (Show, Eq)

instance Enum SMTPResponse where
    fromEnum Ok = 250
    fromEnum Quit = 221
    fromEnum NotImplemented = 502
    toEnum 250 = Ok
    toEnum 221 = Quit
    toEnum 502 = NotImplemented
    toEnum _   = undefined

crlf = "\r\n"

-- | connecting SMTP server with the specified name and port number.
connectSMTPPort :: Integral a => 
                   String     -- ^ name of the server
                -> a          -- ^ port number
                -> IO SMTPConnection
connectSMTPPort hostname port =
    do sock <- socket AF_INET Stream 6
       host <- inet_addr hostname `catch`
               (\_ -> getHostByName hostname >>= return . head . hostAddresses)
       connect sock (SockAddrInet (fromInteger $ toInteger port) host)
       resp <- recv sock 4096
       unless (read (takeWhile (/=' ') resp) == 220) $
              do sClose sock
                 fail "cannot connect to server"
       return sock

-- | connecting SMTP server with the specified name and port 25.
connectSMTP :: String     -- ^ name of the server
            -> IO SMTPConnection
connectSMTP = flip connectSMTPPort 25

-- | send a method to a server
sendMessage :: SMTPConnection -> SMTPMethods -> IO (SMTPResponse, String)
sendMessage sock (DATA dat) =
    do ssize <- send sock $ "DATA\r\n"
       unless (ssize == 6) $ fail "cannot send method DATA"
       (code, _:msg) <- liftM (span (/=' ')) $ recv sock 4096
       unless (read code == 354) $ fail "this server cannot accept any data."
       mapM_ sendLine $ lines dat ++ ["."]
       (code, _:msg) <- liftM (span (/=' ')) $ recv sock 4096
       return (toEnum $ read code, msg)
    where sendLine l = do ssize <- send sock (l ++ crlf)
                          unless (ssize == length l + 2) $
                                 fail "cannot send data."
sendMessage sock meth =
    do ssize <- send sock $ methodToMsg meth
       unless (ssize == length (methodToMsg meth)) $ fail "cannot send data."
       (code, _:msg) <- liftM (span (/=' ')) $ recv sock 4096
       return (toEnum $ read code, msg)
    where methodToMsg (HELO host)   = "HELO " ++ host ++ crlf
          methodToMsg (MAIL sender) = "MAIL FROM:" ++ sender ++ crlf
          methodToMsg (RCPT receiver) = "RCPT TO:" ++ receiver ++ crlf
          methodToMsg RSET = "RSET\r\n"
          methodToMsg QUIT = "QUIT\r\n"

-- | 
-- close the connection.  This function send the QUIT method, so you
-- do not have to QUIT method explicitly.
closeSMTP :: SMTPConnection -> IO ()
closeSMTP conn = do sendMessage conn QUIT
                    sClose conn

-- | 
-- sending a mail to a server. This is achieved by sendMessage.  If
-- something is wrong, it raises an IOexception.
sendMail :: String   -- ^ sender mail
         -> [String] -- ^ receivers
         -> String   -- ^ data
         -> SMTPConnection
         -> IO ()
sendMail sender receivers dat conn = catcher `handle` mainProc
    where mainProc =  do host <- getHostName
                         (Ok, _) <- sendMessage conn (HELO host)
                         (Ok, _) <- sendMessage conn (MAIL sender)
                         vals <- mapM (sendMessage conn . RCPT) receivers
                         unless (all ((==Ok) . fst) vals) $ fail "sendMail error"
                         (Ok, _) <- sendMessage conn (DATA dat)
                         return ()
          catcher e@(PatternMatchFail _) = fail "sendMail error"
          catcher e = throwIO e

-- | 
-- doSMTPPort open a connection, and do an IO action with the
-- connection, and then close it.
doSMTPPort :: Integral a => String -> a -> (SMTPConnection -> IO b) -> IO b
doSMTPPort host port execution =
    bracket (connectSMTPPort host port) closeSMTP execution

-- | 
-- doSMTP is the similar to doSMTPPort, except that it does not
-- require port number but connects to the server with port 25.
doSMTP :: String -> (SMTPConnection -> IO a) -> IO a
doSMTP host execution = doSMTPPort host 25 execution
