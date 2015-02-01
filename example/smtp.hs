{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

import           Network.HaskellNet.SMTP
import           Network.HaskellNet.Auth
import           Network.Mail.Mime
import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import           Control.Monad.Trans (lift, liftIO)

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
    runSMTP conn $
        sendPlainTextMail to from subject plainBody

-- | With custom port number
example2 = doSMTPPort server port $ \conn ->
    runSMTP conn $
        sendPlainTextMail to from subject plainBody

-- | Manually open and close the connection
example3 = do
    conn <- connectSMTP server
    runSMTP conn $ do
        sendPlainTextMail to from subject plainBody
        closeSMTP

-- | Send mime mail
example4 = doSMTP server $ \conn ->
    runSMTP conn $
        sendMimeMail to from subject plainBody htmlBody []

-- | With attachments (modify the `attachments` binding)
example5 = doSMTP server $ \conn ->
    runSMTP conn $
        sendMimeMail to from subject plainBody htmlBody attachments

-- | With authentication
example6 = doSMTP server $ \conn ->
    runSMTP conn $ do
        authenticate authType username password
        sendMimeMail to from subject plainBody htmlBody []

-- | Custom
example7 = do
    conn <- connectSMTPPort server port
    runSMTP conn $ do
        let newMail = Mail (Address (Just "My Name") "from@test.org")
                        [(Address Nothing "to@test.org")]
                        [(Address Nothing "cc1@test.org"), (Address Nothing "cc2@test.org")]
                        []
                        [("Subject", T.pack subject)]
                        [[htmlPart htmlBody, plainPart plainBody]]
        renderedMail <- lift $ lift $ do
            addAttachments attachments newMail
              >>= renderMail'
        sendMail from [to] (S.concat . B.toChunks $ renderedMail)
        closeSMTP

main = example1
