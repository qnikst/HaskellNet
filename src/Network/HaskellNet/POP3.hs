module Network.HaskellNet.POP3
    ( -- * Establishing Connection
      connectPop3Port
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
      -- * Other types
    , A.AuthType(..)
    )
    where

import Network.HaskellNet.BSStream
import Network.Socket
import Network.Compat
import qualified Network.HaskellNet.Auth as A

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Crypto.Hash.MD5
import Numeric (showHex)

import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad (when, unless)

import Data.List
import Data.Char (isSpace, isControl)

import System.IO

import Prelude hiding (catch)

import Network.HaskellNet.POP3.Types
import Network.HaskellNet.POP3.Connection

hexDigest :: [Char] -> [Char]
hexDigest = concatMap (flip showHex "") . B.unpack . hash . B.pack . map (toEnum.fromEnum)

blank :: Char -> Bool
blank a = isSpace a || isControl a

trimR :: ByteString -> ByteString
trimR s = let rs = BS.reverse s in
        BS.dropWhile blank rs

strip :: ByteString -> ByteString
strip = trimR . trimR

stripEnd :: ByteString -> ByteString
stripEnd = BS.reverse . trimR

-- | connecting to the pop3 server specified by the hostname and port
-- number
connectPop3Port :: String -> PortNumber -> IO POP3Connection
connectPop3Port hostname port =
    handleToStream <$> (connectTo hostname port)
    >>= connectStream

-- | connecting to the pop3 server specified by the hostname. 110 is
-- used for the port number.
connectPop3 :: String -> IO POP3Connection
connectPop3 = flip connectPop3Port 110

-- | connecting to the pop3 server via a stream
connectStream :: BSStream -> IO POP3Connection
connectStream st =
    do (resp, msg) <- response st
       when (resp == Err) $ fail "cannot connect"
       let code = last $ BS.words msg
       if BS.head code == '<' && BS.last code == '>'
         then return $ newConnection st (BS.unpack code)
         else return $ newConnection st ""

response :: BSStream -> IO (Response, ByteString)
response st =
    do reply <- strip <$> bsGetLine st
       if (BS.pack "+OK") `BS.isPrefixOf` reply
         then return (Ok, BS.drop 4 reply)
         else return (Err, BS.drop 5 reply)

-- | parse mutiline of response
responseML :: POP3Connection -> IO (Response, ByteString)
responseML conn =
    do reply <- strip <$> bsGetLine st
       if (BS.pack "+OK") `BS.isPrefixOf` reply
         then do rest <- getRest
                 return (Ok, BS.unlines (BS.drop 4 reply : rest))
         else return (Err, BS.drop 5 reply)
    where st = stream conn
          getRest = do l <- stripEnd <$> bsGetLine st
                       if l == BS.singleton '.'
                         then return []
                         else (l:) <$> getRest

-- | sendCommand sends a pop3 command via a pop3 connection.  This
-- action is too generic. Use more specific actions
sendCommand :: POP3Connection -> Command -> IO (Response, ByteString)
sendCommand conn (LIST Nothing) =
    bsPutCrLf (stream conn) (BS.pack "LIST") >> responseML conn
sendCommand conn (UIDL Nothing) =
    bsPutCrLf (stream conn) (BS.pack "UIDL") >> responseML conn
sendCommand conn (RETR msg) =
    bsPutCrLf (stream conn) (BS.pack $ "RETR " ++ show msg) >> responseML conn
sendCommand conn (TOP msg n) =
    bsPutCrLf (stream conn) (BS.pack $ "TOP " ++ show msg ++ " " ++ show n) >>
    responseML conn
sendCommand conn (AUTH A.LOGIN username password) =
    do bsPutCrLf (stream conn) $ BS.pack "AUTH LOGIN"
       bsGetLine (stream conn)
       bsPutCrLf (stream conn) $ BS.pack userB64
       bsGetLine (stream conn)
       bsPutCrLf (stream conn) $ BS.pack passB64
       response (stream conn)
    where (userB64, passB64) = A.login username password
sendCommand conn (AUTH at username password) =
    do bsPutCrLf (stream conn) $ BS.pack $ unwords ["AUTH", show at]
       c <- bsGetLine (stream conn)
       let challenge =
               if BS.take 2 c == BS.pack "+ "
               then A.b64Decode $ BS.unpack $ head $
                    dropWhile (isSpace . BS.last) $ BS.inits $ BS.drop 2 c
               else ""
       bsPutCrLf (stream conn) $ BS.pack $ A.auth at challenge username password
       response (stream conn)
sendCommand conn command =
    bsPutCrLf (stream conn) (BS.pack commandStr) >> response (stream conn)
    where commandStr = case command of
                         (USER name)  -> "USER " ++ name
                         (PASS passw) -> "PASS " ++ passw
                         NOOP         -> "NOOP"
                         QUIT         -> "QUIT"
                         STAT         -> "STAT"
                         (DELE msg)   -> "DELE " ++ show msg
                         RSET         -> "RSET"
                         (LIST msg)   -> "LIST " ++ maybe "" show msg
                         (UIDL msg)   -> "UIDL " ++ maybe "" show msg
                         (APOP usern passw) -> "APOP " ++ usern ++ " " ++
                                             hexDigest (apopKey conn ++ passw)
                         (AUTH _ _ _) -> error "BUG: AUTH should not get matched here"
                         (RETR _) -> error "BUG: RETR should not get matched here"
                         (TOP _ _) -> error "BUG: TOP should not get matched here"

user :: POP3Connection -> String -> IO ()
user conn name = do (resp, _) <- sendCommand conn (USER name)
                    when (resp == Err) $ fail "cannot send user name"

pass :: POP3Connection -> String -> IO ()
pass conn pwd = do (resp, _) <- sendCommand conn (PASS pwd)
                   when (resp == Err) $ fail "cannot send password"

userPass :: POP3Connection -> A.UserName -> A.Password -> IO ()
userPass conn name pwd = user conn name >> pass conn pwd

auth :: POP3Connection -> A.AuthType -> A.UserName -> A.Password
     -> IO ()
auth conn at username password =
    do (resp, msg) <- sendCommand conn (AUTH at username password)
       unless (resp == Ok) $ fail $ "authentication failed: " ++ BS.unpack msg

apop :: POP3Connection -> String -> String -> IO ()
apop conn name pwd =
    do (resp, msg) <- sendCommand conn (APOP name pwd)
       when (resp == Err) $ fail $ "authentication failed: " ++ BS.unpack msg

stat :: POP3Connection -> IO (Int, Int)
stat conn = do (resp, msg) <- sendCommand conn STAT
               when (resp == Err) $ fail "cannot get stat info"
               let (nn, mm) = BS.span (/=' ') msg
               return (read $ BS.unpack nn, read $ BS.unpack $ BS.tail mm)

dele :: POP3Connection -> Int -> IO ()
dele conn n = do (resp, _) <- sendCommand conn (DELE n)
                 when (resp == Err) $ fail "cannot delete"

retr :: POP3Connection -> Int -> IO ByteString
retr conn n = do (resp, msg) <- sendCommand conn (RETR n)
                 when (resp == Err) $ fail "cannot retrieve"
                 return $ BS.tail $ BS.dropWhile (/='\n') msg

top :: POP3Connection -> Int -> Int -> IO ByteString
top conn n m = do (resp, msg) <- sendCommand conn (TOP n m)
                  when (resp == Err) $ fail "cannot retrieve"
                  return $ BS.tail $ BS.dropWhile (/='\n') msg

rset :: POP3Connection -> IO ()
rset conn = do (resp, _) <- sendCommand conn RSET
               when (resp == Err) $ fail "cannot reset"

allList :: POP3Connection -> IO [(Int, Int)]
allList conn = do (resp, lst) <- sendCommand conn (LIST Nothing)
                  when (resp == Err) $ fail "cannot retrieve the list"
                  return $ map f $ tail $ BS.lines lst
    where f s = let (n1, n2) = BS.span (/=' ') s
                in (read $ BS.unpack n1, read $ BS.unpack $ BS.tail n2)

list :: POP3Connection -> Int -> IO Int
list conn n = do (resp, lst) <- sendCommand conn (LIST (Just n))
                 when (resp == Err) $ fail "cannot retrieve the list"
                 let (_, n2) = BS.span (/=' ') lst
                 return $ read $ BS.unpack $ BS.tail n2

allUIDLs :: POP3Connection -> IO [(Int, ByteString)]
allUIDLs conn = do (resp, lst) <- sendCommand conn (UIDL Nothing)
                   when (resp == Err) $ fail "cannot retrieve the uidl list"
                   return $ map f $ tail $ BS.lines lst
    where f s = let (n1, n2) = BS.span (/=' ') s in (read $ BS.unpack n1, n2)

uidl :: POP3Connection -> Int -> IO ByteString
uidl conn n = do (resp, msg) <- sendCommand conn (UIDL (Just n))
                 when (resp == Err) $ fail "cannot retrieve the uidl data"
                 return $ BS.tail $ BS.dropWhile (/=' ') msg

closePop3 :: POP3Connection -> IO ()
closePop3 c = do sendCommand c QUIT
                 bsClose (stream c)

doPop3Port :: String -> PortNumber -> (POP3Connection -> IO a) -> IO a
doPop3Port host port execution =
    bracket (connectPop3Port host port) closePop3 execution

doPop3 :: String -> (POP3Connection -> IO a) -> IO a
doPop3 host execution = doPop3Port host 110 execution

doPop3Stream :: BSStream -> (POP3Connection -> IO b) -> IO b
doPop3Stream conn execution = bracket (connectStream conn) closePop3 execution

crlf :: BS.ByteString
crlf = BS.pack "\r\n"

bsPutCrLf :: BSStream -> ByteString -> IO ()
bsPutCrLf h s = bsPut h s >> bsPut h crlf >> bsFlush h
