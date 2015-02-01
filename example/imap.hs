import System.IO
import Network.HaskellNet.IMAP
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import           Control.Monad.Trans (liftIO)

imapServer = "imap.mail.org"
username = ""
password = ""

main = do
    conn <- connectIMAP imapServer
    runIMAP conn $ do
        login username password
        mboxes <- list
        liftIO $ mapM print mboxes
        select "INBOX"
        msgs <- search [ALLs]
        liftIO $ mapM_ (\x -> print x) (take 4 msgs)
        forM_ (take 4 msgs) (\x -> fetch x >>= liftIO . print)
