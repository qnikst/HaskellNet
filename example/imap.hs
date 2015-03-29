{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Network.HaskellNet.IMAP
import Control.Monad

imapServer = "imap.mail.org"
username = ""
password = ""

main = do
  con <- connectIMAP imapServer
  login con username password
  mboxes <- list con
  mapM_ print mboxes
  select con "INBOX"
  msgs <- search con [ALLs]
  mapM_ print (take 4 msgs)
  forM_ (take 4msgs) (fetch con >=> print)
