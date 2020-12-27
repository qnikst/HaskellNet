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

   * @(1)@ The connection (@conn::@'SMTPConnection') is opened using the 'doSMTP' function.
     We can use this connection to communicate with @SMTP@ server.
   * @(2)@ The 'authenticate' function authenticates to the server with the specified 'AuthType'.
     It returns a 'Bool' indicating either the authentication succeed or not.
   * @(3)@ The 'sendPlainTextMail' is used to send a email a plain text email.

__N.B.__ For /SSL\/TLS/ support you may establish the connection using
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
    , gracefullyCloseSMTP
    , sendMail
    )
    where

import Network.HaskellNet.BSStream
import qualified Data.ByteString.Char8 as BS
import Network.BSD (getHostName)
import Network.Socket
import Network.Compat

import Control.Applicative
import Control.Exception
import Control.Monad (unless, when)

import Network.HaskellNet.Auth

import Network.Mail.Mime
import qualified Data.ByteString.Lazy as B

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

import GHC.Stack
import Prelude
import Network.HaskellNet.SMTP.Internal


-- $workflow
-- The common workflow while working with the library is:
--
--   1. Establish a new connection
--   2. Authenticate to the server
--   3. Perform message sending
--   4. Close connections
--
-- Steps 1 and 4 are combined together using @bracket@-like API. Other than that
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
--               ('closeSMTP')
--    ... conn
-- @
--
-- This approach allows resource management even if the code does not form
-- a stack, so is more general.
--
-- __NOTE__. SMTP protocol advices to use 'QUIT' command for graceful connection
-- close. Before version 0.6 the library never sent it, so does 'closeSMTP' call.
--
-- Starting from 0.6 'doSMTP'-family uses graceful exit and sends 'QUIT' before terminating
-- a connection. This way of termination is exposed as 'gracefullyCloseSTMP' function,
-- however it's not a default method because it requires a connection to be in
-- a valid state. So it's not possible to guarantee backwards compatibility.

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

-- | create SMTPConnection from already connected Stream
connectStream :: BSStream -> IO SMTPConnection
connectStream st =
    do (code1, _) <- parseResponse st
       unless (code1 == 220) $
              do bsClose st
                 fail "cannot connect to the server"
       senderHost <- T.pack <$> getHostName
       msg <- tryCommand (SMTPC st []) (EHLO senderHost) 3 [250]
       return (SMTPC st (tail $ BS.lines msg))


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

-- | 'doSMTPPort' opens a connection to the given port server and
-- performs an IO action with the connection, and then close it.
--
-- 'SMTPConnection' is freed once 'IO' action scope is finished, it means that
-- 'SMTPConnection' value should not escape the action scope.
doSMTPPort :: String -> PortNumber -> (SMTPConnection -> IO a) -> IO a
doSMTPPort host port f =
    bracket (connectSMTPPort host port)
            (\(SMTPC conn _) -> bsClose conn)
            (\c -> f c >>= \x -> quitSMTP c >> pure x)

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
doSMTPStream s f =
  bracket (connectStream s)
          (\(SMTPC conn _) -> bsClose conn)
          (\c -> f c >>= \x -> quitSMTP c >> pure x)

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
sendPlainTextMail :: Address -- ^ receiver
                  -> Address -- ^ sender
                  -> T.Text  -- ^ subject
                  -> LT.Text -- ^ body
                  -> SMTPConnection -- ^ the connection
                  -> IO ()
sendPlainTextMail to from subject body con = do
    renderedMail <- renderMail' $! simpleMail' to from subject body
    sendMail from [to] (B.toStrict renderedMail) con


-- | Send a mime mail. The attachments are included with the file path.
sendMimeMail :: Address              -- ^ receiver
             -> Address              -- ^ sender
             -> T.Text               -- ^ subject
             -> LT.Text              -- ^ plain text body
             -> LT.Text              -- ^ html body
             -> [(T.Text, FilePath)] -- ^ attachments: [(content_type, path)]
             -> SMTPConnection
             -> IO ()
sendMimeMail to from subject plainBody htmlBody attachments con = do
  myMail <- simpleMail to from subject plainBody htmlBody attachments
  renderedMail <- renderMail' myMail
  sendMail from [to] (B.toStrict renderedMail) con

-- | Send a mime mail. The attachments are included with in-memory 'ByteString'.
sendMimeMail' :: Address                        -- ^ receiver
              -> Address                        -- ^ sender
              -> T.Text                         -- ^ subject
              -> LT.Text                        -- ^ plain text body
              -> LT.Text                        -- ^ html body
              -> [(T.Text, T.Text, B.ByteString)] -- ^ attachments: [(content_type, file_name, content)]
              -> SMTPConnection
              -> IO ()
sendMimeMail' to from subject plainBody htmlBody attachments con = do
  let myMail = simpleMailInMemory to from subject plainBody htmlBody attachments
  sendMimeMail2 myMail con

-- | Sends email in generated using 'mime-mail' package.
--
-- Throws 'UserError' @::@ 'IOError' if recipient address not specified.
sendMimeMail2 :: HasCallStack => Mail -> SMTPConnection -> IO ()
sendMimeMail2 mail con = do
    let recps = mailTo mail ++ mailCc mail ++ mailBcc mail
    when (null recps) $ fail "no receiver specified."
    renderedMail <- renderMail' $ mail { mailBcc = [] }
    sendMail (mailFrom mail) recps (B.toStrict renderedMail) con
