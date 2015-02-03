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
example1 = doSMTP server $ \conn ->
    sendPlainTextMail to from subject plainBody conn

-- | With custom port number
example2 = doSMTPPort server port $ \conn ->
    sendPlainTextMail to from subject plainBody conn

-- | Manually open and close the connection
example3 = do
    conn <- connectSMTP server
    sendPlainTextMail to from subject plainBody conn
    closeSMTP conn

-- | Send mime mail
example4 = doSMTP server $ \conn ->
    sendMimeMail to from subject plainBody htmlBody [] conn

-- | With file attachments (modify the `attachments` binding)
example5 = doSMTP server $ \conn ->
    sendMimeMail to from subject plainBody htmlBody attachments conn

-- | With ByteString attachments
bsContent = B.pack [43,43,43,43]
example5_2 = doSMTP server $ \conn ->
    sendMimeMail' to from subject plainBody htmlBody [("application/zip", "filename.zip", bsContent)] conn

-- | With authentication
example6 = doSMTP server $ \conn -> do
    authSuccess <- authenticate authType username password conn
    if authSuccess
        then sendMimeMail to from subject plainBody htmlBody [] conn
        else putStrLn "Authentication failed."

-- | Custom
example7 = do
    conn <- connectSMTPPort server port
    let newMail = Mail (Address (Just "My Name") "from@test.org")
                       [(Address Nothing "to@test.org")]
                       [(Address Nothing "cc1@test.org"), (Address Nothing "cc2@test.org")]
                       []
                       [("Subject", T.pack subject)]
                       [[htmlPart htmlBody, plainPart plainBody]]
    newMail' <- addAttachments attachments newMail
    renderedMail <- renderMail' newMail'
    sendMail from [to] (S.concat . B.toChunks $ renderedMail) conn
    closeSMTP conn

main = example1
