{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

import           Network.HaskellNet.SMTP
import           Network.HaskellNet.Auth
import           Network.Mail.Mime
import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B

-- | Your settings
server       = "smtp.test.com"
port         = toEnum 25
username     = "username"
password     = "password"
authType     = PLAIN
from         = "test@test.com"
to           = "to@test.com"
subject      = "Network.HaskellNet.SMTP Test :)"
plainBody    = "Hello world!"
htmlBody     = "<html><head></head><body><h1>Hello <i>world!</i></h1></body></html>"
attachments  = [] -- example [("application/octet-stream", "/path/to/file1.tar.gz), ("application/pdf", "/path/to/file2.pdf")]

-- | Send plain text mail
example1 = doSMTP server $ \conn -> do
    let mail = simpleMail' to from subject plainBody
    sendMail mail conn

-- | With custom port number
example2 = doSMTPPort server port $ \conn -> do
    let mail = simpleMail' to from subject plainBody
    sendMail mail conn

-- | Manually open and close the connection
example3 = do
    conn <- connectSMTP server
    let mail = simpleMail' to from subject plainBody
    sendMail mail conn
    closeSMTP conn

-- | Send mime mail
example4 = doSMTP server $ \conn -> do
    mail <-  simpleMail to from subject plainBody htmlBody []
    sendMail mail conn

-- | With file attachments (modify the `attachments` binding)
example5 = doSMTP server $ \conn -> do
    mail <-  simpleMail to from subject plainBody htmlBody attachments
    sendMail mail conn

-- | With ByteString attachments
bsContent = B.pack [43,43,43,43]
example5_2 = doSMTP server $ \conn -> do
    let mail = simpleMailInMemory to from subject plainBody htmlBody [("application/zip", "filename.zip", bsContent)]
    sendMail mail conn

-- | With authentication
example6 = doSMTP server $ \conn -> do
    authSuccess <- authenticate authType username password conn
    if authSuccess
        then do mail <- simpleMail to from subject plainBody htmlBody []
                sendMail mail conn
        else putStrLn "Authentication failed."

-- | Custom
example7 = do
    conn <- connectSMTPPort server port
    let newMail = Mail (Address (Just "My Name") "from@test.org")
                       [(Address Nothing "to@test.org")]
                       [(Address Nothing "cc1@test.org"), (Address Nothing "cc2@test.org")]
                       []
                       [("Subject", subject)]
                       [[htmlPart htmlBody, plainPart plainBody]]
    newMail' <- addAttachments attachments newMail
    sendMail newMail' conn
    closeSMTP conn

main = example1
