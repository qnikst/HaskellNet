import System.IO
import HaskellNet.IMAP
import Text.Mime
import qualified Data.ByteString.Char8 as BS
import Control.Monad

imapServer = "mail.ukfsn.org"
user = ""
pass = ""

main = do
  con <- connectIMAP imapServer
  login con user pass
  mboxes <- list con
  mapM print mboxes
  select con "INBOX"
  msgs <- search con [ALLs]
  mapM_ (\x -> print x) (take 4 msgs)
  forM_ (take 4msgs) (\x -> fetch con x >>= print)
         
