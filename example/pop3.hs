import System.IO
import Network.HaskellNet.POP3

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Char8 as BS
import Data.Char 
import Data.ByteString (ByteString)

popServer = "pop3.mail.org"
user = ""
pass = ""

main = do
  con <- connectPop3 popServer
  print "connected"
  userPass con user pass
  num <- list con 4
  print $ "num " ++ (show num)
  msg <- retr con 1
  print msg
  closePop3 con

