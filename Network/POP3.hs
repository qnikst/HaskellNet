module Network.POP3
    ( Command(..)
    , Connection(..)
    , Response(..)
    , connectPop3Port
    , connectPop3
    , sendCommand
    , closePop3
    , doPop3Port
    , doPop3
    , user
    , pass
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

import Network.BSD
import Network

import Data.Digest.MD5
import Numeric (showHex)

import Control.Exception
import Control.Monad

import Data.List

import Base64

import System.IO

import Prelude hiding (catch)

data Connection = Connection !Handle !String -- ^ APOP key
                  deriving Show

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

startFrom s1 [] = True
startFrom [] s2 = False
startFrom (h1:t1) (h2:t2) = h1 == h2 && startFrom t1 t2

connectPop3Port :: Integral a => String -> a -> IO Connection
connectPop3Port hostname port =
    do sock <- connectTo hostname (PortNumber (fromInteger $ toInteger port))
       (resp, msg) <- response sock
       when (resp == Err) $ fail "cannot connect"
       let code = last $ words msg
       if head code == '<' && last code == '>'
         then return $ Connection sock code
         else return $ Connection sock ""

connectPop3 :: String -> IO Connection
connectPop3 = flip connectPop3Port 110

response :: Handle -> IO (Response, String)
response sock =
    do reply <- liftM init $ hGetLine sock
       if reply `startFrom` "+OK "
         then return (Ok, drop 4 reply)
         else return (Err, drop 5 reply)

-- | parse mutiline of response
responseML :: Handle -> IO (Response, String)
responseML sock = do reply <- liftM init $ hGetLine sock
                     if reply `startFrom` "+OK "
                       then do rest <- getRest
                               return (Ok, unlines (drop 4 reply : rest))
                       else return (Err, drop 5 reply)
    where getRest = do l <- liftM init $ hGetLine sock
                       if l == "." then return [] else liftM (l:) getRest

sendCommand :: Connection -> Command -> IO (Response, String)
sendCommand (Connection sock msg_id) (LIST Nothing) =
    hPutStr sock ("LIST" ++ crlf) >> responseML sock
sendCommand (Connection sock msg_id) (UIDL Nothing) =
    hPutStr sock ("UIDL" ++ crlf) >> responseML sock
sendCommand (Connection sock msg_id) (RETR msg) =
    hPutStr sock ("RETR " ++ show msg ++ crlf) >> responseML sock
sendCommand (Connection sock msg_id) (TOP msg n) =
    hPutStr sock ("TOP " ++ show msg ++ " " ++ show n ++ crlf) >> responseML sock
sendCommand (Connection sock msg_id) command =
    hPutStr sock (commandStr ++ crlf) >> response sock
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

user :: Connection -> String -> IO ()
user conn name = do (resp, _) <- sendCommand conn (USER name)
                    when (resp == Err) $ fail "cannot send user name"

pass :: Connection -> String -> IO ()
pass conn pwd = do (resp, _) <- sendCommand conn (PASS pwd)
                   when (resp == Err) $ fail "cannot send password"

auth :: Connection -> String -> String -> IO ()
auth conn name pwd = user conn name >> pass conn pwd

apop :: Connection -> String -> String -> IO ()
apop conn name pwd = do (resp, _) <- sendCommand conn (APOP name pwd)
                        when (resp == Err) $ fail "cannot authenticate"

stat :: Connection -> IO (Int, Int)
stat conn = do (resp, msg) <- sendCommand conn STAT
               when (resp == Err) $ fail "cannot get stat info"
               let (nn, _:mm) = span (/=' ') msg
               return (read nn, read mm)

dele :: Connection -> Int -> IO ()
dele conn n = do (resp, _) <- sendCommand conn (DELE n)
                 when (resp == Err) $ fail "cannot delete"

retr :: Connection -> Int -> IO String
retr conn n = do (resp, msg) <- sendCommand conn (RETR n)
                 when (resp == Err) $ fail "cannot retrieve"
                 return $ tail $ dropWhile (/='\n') msg

top :: Connection -> Int -> Int -> IO String
top conn n m = do (resp, msg) <- sendCommand conn (TOP n m)
                  when (resp == Err) $ fail "cannot retrieve"
                  return $ tail $ dropWhile (/='\n') msg

rset :: Connection -> IO ()
rset conn = do (resp, _) <- sendCommand conn RSET
               when (resp == Err) $ fail "cannot reset"

allList :: Connection -> IO [(Int, Int)]
allList conn = do (resp, lst) <- sendCommand conn (LIST Nothing)
                  when (resp == Err) $ fail "cannot retrieve the list"
                  return $ map f $ tail $ lines lst
    where f s = let (n1, _:n2) = span (/=' ') s in (read n1, read n2)

list :: Connection -> Int -> IO Int
list conn n = do (resp, lst) <- sendCommand conn (LIST (Just n))
                 when (resp == Err) $ fail "cannot retrieve the list"
                 let (_, _:n2) = span (/=' ') lst
                 return $ read n2

allUIDLs :: Connection -> IO [(Int, String)]
allUIDLs conn = do (resp, lst) <- sendCommand conn (UIDL Nothing)
                   when (resp == Err) $ fail "cannot retrieve the uidl list"
                   return $ map f $ tail $ lines lst
    where f s = let (n1, _:n2) = span (/=' ') s in (read n1, n2)

uidl :: Connection -> Int -> IO String
uidl conn n = do (resp, msg) <- sendCommand conn (UIDL (Just n))
                 when (resp == Err) $ fail "cannot retrieve the uidl data"
                 return $ tail $ dropWhile (/=' ') msg

closePop3 :: Connection -> IO ()
closePop3 conn@(Connection sock _) = do sendCommand conn QUIT
                                        hClose sock

doPop3Port :: Integral a => String -> a -> (Connection -> IO b) -> IO b
doPop3Port host port execution =
    bracket (connectPop3Port host port) closePop3 execution

doPop3 :: String -> (Connection -> IO a) -> IO a
doPop3 host execution = doPop3Port host 110 execution
