import System.IO
import HaskellNet.POP3

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Char8 as BS
import Data.Char 
import Data.ByteString (ByteString)

popServer = "pop3.ukfsn.org"
user = ""
pass = ""

main = do
  let rs = strp $ BS.pack "+OK Dovecot ready.\r"
  con <- connectPop3 popServer
  print "connected"
  userPass con user pass
  num <- list con 4
  print $ "num " ++ (show num)
  msg <- retr con 1
  print msg
  closePop3 con

--"+OK Dovecot ready.\r"

strp :: ByteString -> ByteString
strp s = head $ dropWhile (isSpace . BS.last) $ BS.inits $ BS.dropWhile isSpace s
{-
trim :: ByteString -> ByteString  
trim = trimR . trimR  
     where  
         trimR s = let rs = BS.reverse s in
                   BS.dropWhile blank rs

blank :: Char -> Bool
blank a = isSpace a || isControl a
-}