----------------------------------------------------------------------
-- |
-- Module      :  HaskellNet.POP3
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  unstable
-- Portability :  portable
-- 
-- POP3 client implementation
-- 

module HaskellNet.POP3
    ( -- * Types
      Command(..)
    , POP3Connection(..)
    , Response(..)
      -- * Establishing Connection
    , connectPop3Port
    , connectPop3
    , connectStream
      -- * Send Command
    , sendCommand
      -- * More Specific Operations
    , closePop3
    , user
    , pass
    , userPass
    , apop
    , auth
    , stat
    , dele
    , retr
    , top
    , rset
    , allList
    , list
    , allUIDLs
    , uidl
      -- * Other Useful Operations
    , doPop3Port
    , doPop3
    , doPop3Stream
    )
    where

import HaskellNet.BSStream
import Network
import HaskellNet.Auth hiding (auth, login)
import qualified HaskellNet.Auth as A

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Digest.MD5
import Numeric (showHex)

import Control.Exception
import Control.Monad (when, unless)

import Data.List
import Data.Char (isSpace)

import System.IO

import qualified Data.ByteString.Char8 as BSC

import Prelude hiding (catch)

data BSStream s => POP3Connection s = POP3C !s !ByteString -- ^ APOP key

data Command = USER String
             | PASS String
             | APOP String String
             | NOOP
             | QUIT
             | STAT
             | LIST (Maybe Int)
             | DELE Int
             | RETR Int
             | RSET
             | TOP Int Int
             | UIDL (Maybe Int)

data Response = Ok | Err 
                deriving (Eq, Show)

crlf = "\r\n"

hexDigest = concatMap (flip showHex "") . hash . map (toEnum.fromEnum) 

strip :: ByteString -> ByteString
strip s = head $ dropWhile (isSpace . BS.last) $ BS.inits $ BS.dropWhile isSpace s

-- |
-- connecting to the pop3 server specified by the hostname and port number
connectPop3Port :: String -> PortNumber -> IO (POP3Connection Handle)
connectPop3Port hostname port = connectTo hostname (PortNumber port) >>= connectStream

-- |
-- connecting to the pop3 server specified by the hostname. 110 is used for the port number.
connectPop3 :: String -> IO (POP3Connection Handle)
connectPop3 = flip connectPop3Port 110

-- |
-- connecting to the pop3 server via a stream
connectStream :: BSStream s => s -> IO (POP3Connection s)
connectStream st =
    do (resp, msg) <- response st
       when (resp == Err) $ fail "cannot connect"
       let code = last $ BS.words msg
       if BS.head code == '<' && BS.last code == '>'
         then return $ POP3C st code
         else return $ POP3C st BS.empty


response :: BSStream s => s -> IO (Response, ByteString)
response st =
    do reply <- fmap strip $ bsGetLine st
       if reply `BS.isPrefixOf` (BS.pack "+OK ")
         then return (Ok, BS.drop 4 reply)
         else return (Err, BS.drop 5 reply)

-- | parse mutiline of response
responseML :: BSStream s => s -> IO (Response, ByteString)
responseML st =
    do reply <- fmap strip $ bsGetLine st
       if reply `BS.isPrefixOf` (BS.pack "+OK ")
         then do rest <- getRest
                 return (Ok, BS.unlines (BS.drop 4 reply : rest))
         else return (Err, BS.drop 5 reply)
    where getRest = do l <- fmap strip $ bsGetLine st
                       if l == BS.singleton '.'
                         then return []
                         else fmap (l:) getRest

-- | sendCommand sends a pop3 command via a pop3 connection.  This
-- action is too generic. Use more specific actions
sendCommand :: BSStream s => POP3Connection s -> Command -> IO (Response, ByteString)
sendCommand (POP3C conn msg_id) (LIST Nothing) =
    bsPut conn (BS.pack "LIST\r\n") >> responseML conn
sendCommand (POP3C conn msg_id) (UIDL Nothing) =
    bsPut conn (BS.pack "UIDL\r\n") >> responseML conn
sendCommand (POP3C conn msg_id) (RETR msg) =
    bsPut conn (BS.pack $ "RETR " ++ show msg ++ crlf) >> responseML conn
sendCommand (POP3C conn msg_id) (TOP msg n) =
    bsPut conn (BS.pack $ "TOP " ++ show msg ++ " " ++ show n ++ crlf) >> responseML conn
sendCommand (POP3C conn msg_id) command =
    bsPut conn (BS.pack $ commandStr ++ crlf) >> response conn
    where commandStr = case command of
                         (USER name) -> "USER " ++ name
                         (PASS pass) -> "PASS " ++ pass
                         NOOP        -> "NOOP"
                         QUIT        -> "QUIT"
                         STAT        -> "STAT"
                         (DELE msg)  -> "DELE " ++ show msg
                         RSET        -> "RSET"
                         (LIST msg)  -> "LIST " ++ maybe "" show msg
                         (UIDL msg)  -> "UIDL " ++ maybe "" show msg
                         (APOP user pass) -> "APOP " ++ user ++ " " ++ hexDigest pass

user :: BSStream s => POP3Connection s -> String -> IO ()
user conn name = do (resp, _) <- sendCommand conn (USER name)
                    when (resp == Err) $ fail "cannot send user name"

pass :: BSStream s => POP3Connection s -> String -> IO ()
pass conn pwd = do (resp, _) <- sendCommand conn (PASS pwd)
                   when (resp == Err) $ fail "cannot send password"

userPass :: BSStream s => POP3Connection s -> UserName -> Password -> IO ()
userPass conn name pwd = user conn name >> pass conn pwd

auth :: BSStream s => POP3Connection s -> AuthType -> UserName -> Password -> IO ()
auth (POP3C conn _) LOGIN user pass =
    do bsPut conn $ BS.pack "AUTH LOGIN\r\n"
       bsGetContents conn
       bsPut conn $ BS.pack (userB64++crlf)
       bsGetContents conn
       bsPut conn $ BS.pack (passB64++crlf)
       (resp, msg) <- response conn
       unless (resp == Ok) $ fail ("authentication failed: " ++ BS.unpack msg)
    where (userB64, ' ':passB64) = break isSpace $ A.login user pass
auth (POP3C conn _) at user pass =
    do bsPut conn $ BS.pack authCmd
       challenge <- getChallenge conn
       bsPut conn $ BS.pack $ (A.auth at challenge user pass ++ crlf)
       (resp, msg) <- response conn
       unless (resp == Ok) $ fail ("authentication failed: " ++ BS.unpack msg)
    where authCmd = unwords ["AUTH", show at] ++ crlf
          getChallenge conn =
              do c <- bsGetContents conn
                 if BS.take 2 c == BS.pack "+ "
                   then let c' = BS.drop 2 c
                        in return $ b64Decode $ BS.unpack $ head $ dropWhile (isSpace . BS.last) $ BS.inits c'
                   else return ""

             

apop :: BSStream s => POP3Connection s -> String -> String -> IO ()
apop conn name pwd = do (resp, _) <- sendCommand conn (APOP name pwd)
                        when (resp == Err) $ fail "cannot authenticate"

stat :: BSStream s => POP3Connection s -> IO (Int, Int)
stat conn = do (resp, msg) <- sendCommand conn STAT
               when (resp == Err) $ fail "cannot get stat info"
               let (nn, mm) = BS.span (/=' ') msg
               return (read $ BS.unpack nn, read $ BS.unpack $ BS.tail mm)

dele :: BSStream s => POP3Connection s -> Int -> IO ()
dele conn n = do (resp, _) <- sendCommand conn (DELE n)
                 when (resp == Err) $ fail "cannot delete"

retr :: BSStream s => POP3Connection s -> Int -> IO ByteString
retr conn n = do (resp, msg) <- sendCommand conn (RETR n)
                 when (resp == Err) $ fail "cannot retrieve"
                 return $ BS.tail $ BS.dropWhile (/='\n') msg

top :: BSStream s => POP3Connection s -> Int -> Int -> IO ByteString
top conn n m = do (resp, msg) <- sendCommand conn (TOP n m)
                  when (resp == Err) $ fail "cannot retrieve"
                  return $ BS.tail $ BS.dropWhile (/='\n') msg

rset :: BSStream s => POP3Connection s -> IO ()
rset conn = do (resp, _) <- sendCommand conn RSET
               when (resp == Err) $ fail "cannot reset"

allList :: BSStream s => POP3Connection s -> IO [(Int, Int)]
allList conn = do (resp, lst) <- sendCommand conn (LIST Nothing)
                  when (resp == Err) $ fail "cannot retrieve the list"
                  return $ map f $ tail $ BS.lines lst
    where f s = let (n1, n2) = BS.span (/=' ') s
                in (read $ BS.unpack n1, read $ BS.unpack $ BS.tail n2)

list :: BSStream s => POP3Connection s -> Int -> IO Int
list conn n = do (resp, lst) <- sendCommand conn (LIST (Just n))
                 when (resp == Err) $ fail "cannot retrieve the list"
                 let (_, n2) = BS.span (/=' ') lst
                 return $ read $ BS.unpack $ BS.tail n2

allUIDLs :: BSStream s => POP3Connection s -> IO [(Int, ByteString)]
allUIDLs conn = do (resp, lst) <- sendCommand conn (UIDL Nothing)
                   when (resp == Err) $ fail "cannot retrieve the uidl list"
                   return $ map f $ tail $ BS.lines lst
    where f s = let (n1, n2) = BS.span (/=' ') s in (read $ BS.unpack n1, n2)

uidl :: BSStream s => POP3Connection s -> Int -> IO ByteString
uidl conn n = do (resp, msg) <- sendCommand conn (UIDL (Just n))
                 when (resp == Err) $ fail "cannot retrieve the uidl data"
                 return $ BS.tail $ BS.dropWhile (/=' ') msg

closePop3 :: BSStream s => POP3Connection s -> IO ()
closePop3 c@(POP3C conn _) = do sendCommand c QUIT
                                bsClose conn

doPop3Port :: String -> PortNumber -> (POP3Connection Handle -> IO a) -> IO a
doPop3Port host port execution =
    bracket (connectPop3Port host port) closePop3 execution

doPop3 :: String -> (POP3Connection Handle -> IO a) -> IO a
doPop3 host execution = doPop3Port host 110 execution

doPop3Stream :: BSStream s => s -> (POP3Connection s -> IO b) -> IO b
doPop3Stream conn execution = bracket (connectStream conn) closePop3 execution