{-# OPTIONS -fglasgow-exts #-}
----------------------------------------------------------------------
-- |
-- Module      :  HaskellNet.Memcache
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  stable
-- Portability :  portable
-- 
-- memcached client
-- 

module HaskellNet.Memcache
    ( -- * Types
      Memcache(..)
    , Status(..)
    , Escapable(..)
      -- * create connections
    , connect
    , connectServer
    , connectStream
      -- * memcache commands
    , set
    , add
    , replace
    , setNoExpire
    , addNoExpire
    , replaceNoExpire
    , get
    , gets
    , getS
    , getsS
    , getFull
    , getsFull
    , delete
    , deleteExpire
    , incr
    , decr
    , stats
    , version
    , quit
    )
where

import HaskellNet.BSStream
import Network
import System.IO
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack, singleton)
import qualified Data.ByteString.Char8 as BS

import qualified Data.Map as M
import Data.Word

data Memcache s = Memcache s

data Status = Status { pid :: Word32
                     , uptime :: Word32
                     , time :: Word32
                     , ver :: String
                     , rusage_user :: Double
                     , rusage_system :: Double
                     , curr_items :: Word32
                     , total_items :: Word32
                     , bytes :: Word64
                     , curr_connections :: Word32
                     , total_connections :: Word32
                     , connection_structures :: Word32
                     , cmd_get :: Word32
                     , cmd_set :: Word32
                     , get_hits :: Word32
                     , get_misses :: Word32
                     , bytes_read :: Word64
                     , bytes_written :: Word64
                     , limit_maxbytes :: Word32
                     }
            deriving Show

type Flags = Integer
type Expire = Integer
type Bytes = Integer

class Escapable a where
    escape :: a -> ByteString
    unEscape :: ByteString -> a

instance Escapable ByteString where
    escape s | crlf `BS.isSubstringOf` s || '\\' `BS.elem` s =
                    BS.concatMap f s
                | otherwise = s
        where f '\\' = pack "\\\\"
              f '\r' = pack "\\r"
              f c    = singleton c
    unEscape s = BS.concat $ concat $ map f $ BS.split '\\' s
        where f s | s == BS.empty = [s]
                  | BS.head s == '\\' = [singleton '\\', BS.tail s]
                  | BS.head s == 'r'  = [singleton '\r', BS.tail s]
                  | otherwise         = [s]

instance Escapable String where
    escape = pack . concatMap f
        where f '\\' = "\\\\"
              f '\r' = "\\r"
              f c    = [c]
    unEscape = main . unpack
        where main [] = []
              main ('\\':'\\':cs) = '\\' : main cs
              main ('\\':'r':cs)  = '\r' : main cs
              main (c:cs)         = c : main cs


instance Escapable Int where
    escape = pack . show
    unEscape = read . unpack
instance Escapable Word32 where
    escape = pack . show
    unEscape = read . unpack
instance Escapable Word64 where
    escape = pack . show
    unEscape = read . unpack
instance Escapable Integer where
    escape = pack . show
    unEscape = read . unpack
instance Escapable Char where
    escape = pack . show
    unEscape = read . unpack
instance Escapable Float where
    escape = pack . show
    unEscape = read . unpack
instance Escapable Double where
    escape = pack . show
    unEscape = read . unpack

crlf = pack "\r\n"
sp   = singleton ' '


connect :: PortNumber -> IO (Memcache Handle)
connect port = do h <- connectTo "localhost" (PortNumber port)
                  connectStream h
connectServer :: String -> PortNumber -> IO (Memcache Handle)
connectServer server port = do h <- connectTo server (PortNumber port)
                               connectStream h
connectStream :: BSStream s => s -> IO (Memcache s)
connectStream = return . Memcache


examineKey, examineVal :: ByteString -> Bool
examineKey s = BS.all (flip elem " ") s || crlf `BS.isSubstringOf` s
examineVal s = crlf `BS.isSubstringOf` s

setInner cmd (Memcache s) key flag expr val =
    do bsPutCrLf s $ BS.unwords [cmd, bskey, fs, es, bytes]
       bsPutCrLf s val
       parse [] s
    where fs = pack $ show flag
          es = pack $ show expr
          bytes = pack $ show $ BS.length val
          bskey = pack $ key
          parse e s = do l <- bsGetLine s
                         case unpack l of
                           "STORED\r" -> return True
                           "NOT STORED\r" -> return False
                           "ERROR\r" -> fail $ unlines e
                           bs -> parse (bs:e) s
                       

set, add, replace :: BSStream s =>
                     Memcache s
                  -> String
                  -> Flags
                  -> Expire
                  -> ByteString
                  -> IO Bool
set = setInner (pack "set")
add = setInner (pack "add")
replace = setInner (pack "replace")
    
setNoExpire, addNoExpire, replaceNoExpire :: BSStream s =>
                                             Memcache s
                                          -> String
                                          -> Flags
                                          -> ByteString
                                          -> IO Bool
setNoExpire m k f d = set m k f 0 d
addNoExpire m k f d = add m k f 0 d
replaceNoExpire m k f d = replace m k f 0 d

getS :: BSStream s => Memcache s -> String -> IO ByteString
getS m k = fmap head (getsS m [k])
getsS :: BSStream s => Memcache s -> [String] -> IO [ByteString]
getsS m ks = fmap (map f) $ getsFull m ks
    where f (Just (_,_,bs)) = bs
          f _               = BS.empty

get :: (BSStream s, Escapable v) => Memcache s -> String -> IO (Maybe v)
get m k = fmap head $ gets m [k]
gets :: (BSStream s, Escapable v) => Memcache s -> [String] -> IO [Maybe v]
gets m ks = fmap (map (>>= f)) $ getsFull m ks
    where f (_,_,bs) = Just $ unEscape bs

getFull :: BSStream s => Memcache s -> String -> IO (Maybe (Flags, Expire, ByteString))
getFull m k = fmap head (getsFull m [k])
getsFull :: BSStream s => Memcache s -> [String] -> IO [Maybe (Flags, Expire, ByteString)]
getsFull (Memcache s) keys = 
    do bsPut s (pack "get ")
       bsPutCrLf s $ BS.unwords $ map pack keys
       m <- parse M.empty s
       return $ map (flip M.lookup m . pack) keys
    where parse m s =
              do l <- bsGetLine s
                 case () of
                   _ | l == pack "END\r" -> return m
                     | BS.pack "VALUE " `BS.isPrefixOf` l ->
                         do l2 <- bsGetLine s
                            let (k:fl:expr:_) = tail $ BS.words l
                            parse (M.insert k (read $ unpack fl, read $ unpack expr, BS.init l2) m) s
                     | otherwise -> fail "invalid server response"

delete :: BSStream s => Memcache s -> String -> IO ()
delete (Memcache s) key = do bsPut s $ pack "delete "
                             bsPutCrLf s $ pack key
                             bsGetLine s
                             return ()
deleteExpire :: BSStream s => Memcache s -> String -> Expire -> IO ()
deleteExpire (Memcache s) key expr = do bsPut s $ pack "delete "
                                        bsPut s $ pack key
                                        bsPut s sp
                                        bsPutCrLf s $ pack $ show expr
                                        bsGetLine s
                                        return ()

incrDecr cmd (Memcache s) key i = do bsPut s cmd
                                     bsPut s $ pack key
                                     bsPut s sp
                                     bsPutCrLf s $ pack $ show i
                                     l <- bsGetLine s
                                     if l == pack "NOT_FOUND\r"
                                       then fail "not such key"
                                       else return $ read $ unpack $ BS.init l

incr :: BSStream s => Memcache s -> String -> Integer -> IO Integer
incr = incrDecr (pack "incr ")
decr :: BSStream s => Memcache s -> String -> Integer -> IO Integer
decr = incrDecr (pack "decr ")

stats :: BSStream s => Memcache s -> IO Status
stats (Memcache s) =
    do bsPutCrLf s $ pack "stats"
       parse e s
    where e = Status 0 0 0 "" 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
          parse e s = do l <- bsGetLine s
                         case () of
                           _ | pack "END\r" == l -> return e
                             | pack "STAT " `BS.isPrefixOf` l ->
                                 let (_:k:v:_) = BS.words l
                                 in parse (update e (unpack k) (unpack v)) s
                             | otherwise -> fail "invalid server response"
          update e "pid"           v = e { pid = read v }
          update e "uptime"        v = e { uptime = read v }
          update e "time"          v = e { time = read v }
          update e "version"       v = e { ver = v }
          update e "rusage_user"   v = e { rusage_user = read v }
          update e "rusage_system" v = e { rusage_system = read v }
          update e "curr_items"    v = e { curr_items = read v }
          update e "total_items"   v = e { total_items = read v }
          update e "bytes"         v = e { bytes = read v }
          update e "curr_connections" v =
              e { curr_connections = read v }
          update e "total_connections" v =
              e { total_connections = read v }
          update e "connection_structures" v =
              e { connection_structures = read v }
          update e "cmd_get"        v = e { cmd_get = read v }
          update e "cmd_set"        v = e { cmd_set = read v }
          update e "get_hits"       v = e { get_hits = read v }
          update e "get_misses"     v = e { get_misses = read v }
          update e "bytes_read"     v = e { bytes_read = read v }
          update e "bytes_written"  v = e { bytes_written = read v }
          update e "limit_maxbytes" v = e { limit_maxbytes = read v }
          update e _ _ = e
       
version :: BSStream s => Memcache s -> IO String
version (Memcache s) = do bsPutCrLf s $ pack "version"
                          l <- bsGetLine s
                          if pack "VERSION " `BS.isSuffixOf` l
                            then return $ unpack $ BS.init $ BS.drop 8 l
                            else fail "illegal server response"

quit :: BSStream s => Memcache s -> IO ()
quit (Memcache s) = do bsPutCrLf s $ pack "quit"
                       bsClose s
