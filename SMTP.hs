module SMTP
    ( SMTPMethods(..)
    , SMTPResponse(..)
    , Connect
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

type Connect = Socket

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

connectSMTPPort :: Integral a => String -> a -> IO Connect
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

connectSMTP :: String -> IO Connect
connectSMTP = flip connectSMTPPort 25

sendMessage :: Connect -> SMTPMethods -> IO (SMTPResponse, String)
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

closeSMTP :: Connect -> IO ()
closeSMTP conn = do sendMessage conn QUIT
                    sClose conn

sendMail :: String   -- ^ sender mail
         -> [String] -- ^ receivers
         -> String   -- ^ data
         -> Connect
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
       
doSMTPPort :: Integral a => String -> a -> (Connect -> IO b) -> IO b
doSMTPPort host port execution =
    bracket (connectSMTPPort host port) closeSMTP execution

doSMTP :: String -> (Connect -> IO a) -> IO a
doSMTP host execution = doSMTPPort host 25 execution
