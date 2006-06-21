----------------------------------------------------------------------
-- |
-- Module      :  Network.POP3
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  unstable
-- Portability :  portable
-- 
-- POP3 client implementation
-- 

module Network.POP3
    ( Command(..)
    , POP3Connection(..)
    , Response(..)
    , connectPop3Port
    , connectPop3
    , connectStream
    , sendCommand
    , closePop3
    , doPop3Port
    , doPop3
    , doPop3Stream
    , user
    , pass
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
    )
    where

import Network.TCP
import Network.Stream

import Data.Digest.MD5
import Numeric (showHex)

import Control.Exception
import Control.Monad

import Data.List

import System.IO

import Prelude hiding (catch)

data Stream a => POP3Connection a = POP3C !a !String -- ^ APOP key

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

e2s = either (\_ -> "") init

connectPop3Port :: String -> Int -> IO (POP3Connection Connection)
connectPop3Port hostname port = openTCPPort hostname port >>= connectStream

connectPop3 :: String -> IO (POP3Connection Connection)
connectPop3 = flip connectPop3Port 110

connectStream :: Stream a => a -> IO (POP3Connection a)
connectStream conn =
    do (resp, msg) <- response conn
       when (resp == Err) $ fail "cannot connect"
       let code = last $ words msg
       if head code == '<' && last code == '>'
         then return $ POP3C conn code
         else return $ POP3C conn ""


response :: Stream a => a -> IO (Response, String)
response conn =
    do reply <- liftM e2s $ readLine conn
       if reply `isPrefixOf` "+OK "
         then return (Ok, drop 4 reply)
         else return (Err, drop 5 reply)

-- | parse mutiline of response
responseML :: Stream a => a -> IO (Response, String)
responseML conn = do reply <- liftM e2s $ readLine conn
                     if reply `isPrefixOf` "+OK "
                       then do rest <- getRest
                               return (Ok, unlines (drop 4 reply : rest))
                       else return (Err, drop 5 reply)
    where getRest = do l <- liftM e2s $ readLine conn
                       if l == "." then return [] else liftM (l:) getRest

sendCommand :: Stream a => POP3Connection a -> Command -> IO (Response, String)
sendCommand (POP3C conn msg_id) (LIST Nothing) =
    writeBlock conn ("LIST" ++ crlf) >> responseML conn
sendCommand (POP3C conn msg_id) (UIDL Nothing) =
    writeBlock conn ("UIDL" ++ crlf) >> responseML conn
sendCommand (POP3C conn msg_id) (RETR msg) =
    writeBlock conn ("RETR " ++ show msg ++ crlf) >> responseML conn
sendCommand (POP3C conn msg_id) (TOP msg n) =
    writeBlock conn ("TOP " ++ show msg ++ " " ++ show n ++ crlf) >> responseML conn
sendCommand (POP3C conn msg_id) command =
    writeBlock conn (commandStr ++ crlf) >> response conn
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

user :: Stream a => POP3Connection a -> String -> IO ()
user conn name = do (resp, _) <- sendCommand conn (USER name)
                    when (resp == Err) $ fail "cannot send user name"

pass :: Stream a => POP3Connection a -> String -> IO ()
pass conn pwd = do (resp, _) <- sendCommand conn (PASS pwd)
                   when (resp == Err) $ fail "cannot send password"

auth :: Stream a => POP3Connection a -> String -> String -> IO ()
auth conn name pwd = user conn name >> pass conn pwd

apop :: Stream a => POP3Connection a -> String -> String -> IO ()
apop conn name pwd = do (resp, _) <- sendCommand conn (APOP name pwd)
                        when (resp == Err) $ fail "cannot authenticate"

stat :: Stream a => POP3Connection a -> IO (Int, Int)
stat conn = do (resp, msg) <- sendCommand conn STAT
               when (resp == Err) $ fail "cannot get stat info"
               let (nn, _:mm) = span (/=' ') msg
               return (read nn, read mm)

dele :: Stream a => POP3Connection a -> Int -> IO ()
dele conn n = do (resp, _) <- sendCommand conn (DELE n)
                 when (resp == Err) $ fail "cannot delete"

retr :: Stream a => POP3Connection a -> Int -> IO String
retr conn n = do (resp, msg) <- sendCommand conn (RETR n)
                 when (resp == Err) $ fail "cannot retrieve"
                 return $ tail $ dropWhile (/='\n') msg

top :: Stream a => POP3Connection a -> Int -> Int -> IO String
top conn n m = do (resp, msg) <- sendCommand conn (TOP n m)
                  when (resp == Err) $ fail "cannot retrieve"
                  return $ tail $ dropWhile (/='\n') msg

rset :: Stream a => POP3Connection a -> IO ()
rset conn = do (resp, _) <- sendCommand conn RSET
               when (resp == Err) $ fail "cannot reset"

allList :: Stream a => POP3Connection a -> IO [(Int, Int)]
allList conn = do (resp, lst) <- sendCommand conn (LIST Nothing)
                  when (resp == Err) $ fail "cannot retrieve the list"
                  return $ map f $ tail $ lines lst
    where f s = let (n1, _:n2) = span (/=' ') s in (read n1, read n2)

list :: Stream a => POP3Connection a -> Int -> IO Int
list conn n = do (resp, lst) <- sendCommand conn (LIST (Just n))
                 when (resp == Err) $ fail "cannot retrieve the list"
                 let (_, _:n2) = span (/=' ') lst
                 return $ read n2

allUIDLs :: Stream a => POP3Connection a -> IO [(Int, String)]
allUIDLs conn = do (resp, lst) <- sendCommand conn (UIDL Nothing)
                   when (resp == Err) $ fail "cannot retrieve the uidl list"
                   return $ map f $ tail $ lines lst
    where f s = let (n1, _:n2) = span (/=' ') s in (read n1, n2)

uidl :: Stream a => POP3Connection a -> Int -> IO String
uidl conn n = do (resp, msg) <- sendCommand conn (UIDL (Just n))
                 when (resp == Err) $ fail "cannot retrieve the uidl data"
                 return $ tail $ dropWhile (/=' ') msg

closePop3 :: Stream a => POP3Connection a -> IO ()
closePop3 c@(POP3C conn _) = do sendCommand c QUIT
                                close conn

doPop3Port :: String -> Int -> (POP3Connection Connection -> IO a) -> IO a
doPop3Port host port execution =
    bracket (connectPop3Port host port) closePop3 execution

doPop3 :: String -> (POP3Connection Connection -> IO a) -> IO a
doPop3 host execution = doPop3Port host 110 execution

doPop3Stream :: Stream a => a -> (POP3Connection a -> IO b) -> IO b
doPop3Stream conn execution = bracket (connectStream conn) closePop3 execution