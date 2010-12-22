import System.IO
import HaskellNet.SMTP
import Text.Mime
import qualified Data.ByteString.Char8 as BS
import Codec.Binary.Base64.String


smtpServer = "outmail.f2s.com"
sendFrom = "test@test.org"
sendTo = ["wrwills@gmail.com"]


main = do
  con <- connectSMTP smtpServer
  message <- BS.readFile "example/message.txt"
  messageHtml <- BS.readFile "example/message.html"
  let textP = SinglePart [("Content-Type", "text/plain; charset=utf-8")] message
  let htmlP = SinglePart [("Content-Type", "text/html; charset=utf-8")] messageHtml
  let msg = MultiPart [("From", "HaskellNet <" ++ sendFrom ++ ">"),("Subject","Test")] [htmlP, textP]
  sendMail sendFrom sendTo (BS.pack $ show $ showMime "utf-8" msg) con
  closeSMTP con
         


  --let msg = ([("From", "HaskellNet <" ++ sendFrom ++ ">"),("Subject","Test")], MultiPart [] [textP, htmlP])
  --sendMail sendFrom sendTo (BS.pack $ show $ showMessage "utf-8" msg) con
--  let msg = ([("From", "HaskellNet <" ++ sendFrom ++ ">"),("Subject","Test")], BS.pack "\r\nhello 算法是指完成一个任from haskellnet")

--htmlP = SinglePart [("Content-Type", "text/html; charset=utf-8", "Content-Transfer-Encoding", ""] BS.pack $ encode $  



