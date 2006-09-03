{-# OPTIONS_GHC -cpp -fglasgow-exts -package hsgnutls -package HaskellNet #-}
-- examples to connect server by hsgnutls

module TLSStream
    ( connectTLS
    , connectTLSPort
    , TlsSession
    , sess
    )
    where

import Network.GnuTLS
import Network
import HaskellNet.BSStream

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base as BSB

import System.IO

import Data.IORef
import Control.Monad

import Foreign.ForeignPtr
import Foreign.Ptr

data TlsSession t = TlsSession { sess :: Session t
                               , hdl  :: Handle
                               , buf  :: IORef ByteString }

fromSession :: Handle -> Session t -> IO (TlsSession t)
fromSession h s = do newbuf <- newIORef BS.empty
                     return $ TlsSession s h newbuf

connectTLS :: HostName -> PortNumber -> IO (TlsSession Client)
connectTLS host port = connectTLSPort host (PortNumber port)

connectTLSPort :: HostName -> PortID -> IO (TlsSession Client)
connectTLSPort host port =
    do h <- connectTo host port
       s <- tlsClient [ handle :=> return h
                      , priorities := [CrtX509, CrtOpenpgp]]
       cred <- certificateCredentials
       set s [ credentials := cred]
       handshake s
       fromSession h s



bufLen = 4096
waiting = 500 -- miliseconds

extendBuf sess@(TlsSession s _ buf) =
    do res <- mallocForeignPtrBytes bufLen
       len <- withForeignPtr res (\p -> tlsRecv s p bufLen)
       modifyIORef buf (flip BS.append $ BSB.fromForeignPtr res len)
       return len

doWhile cond execute =
    do f <- cond
       when f $ (execute >> doWhile cond execute)
          

instance BSStream (TlsSession t) where
#if defined(__GLASGOW_HASKELL__)
    bsGetLine sess@(TlsSession s _ buf) =
        do doWhile (readIORef buf >>= return . BS.notElem '\n')
                       (extendBuf sess)
           bufstr' <- readIORef buf
           let (line, rest) =  BS.span (/='\n') bufstr'
           writeIORef buf $ BS.tail rest
           return line
    bsGetLines sess = fmap BS.lines $ bsGetContents sess
    bsGetNonBlocking sess@(TlsSession s h buf) len =
        do doWhile (do r <- hWaitForInput h waiting
                       l <- fmap BS.length $ readIORef buf
                       return (r && l < len))
                   (extendBuf sess)
           bufstr' <- readIORef buf
           let (r, bufstr'') = BS.splitAt len bufstr'
           writeIORef buf bufstr''
           return r
#endif
    bsGetContents sess@(TlsSession s h buf) =
        do doWhile (hWaitForInput h waiting) (extendBuf sess)
           bufstr <- readIORef buf
           writeIORef buf BS.empty
           return bufstr
    bsGet sess@(TlsSession s _ buf) len =
        do doWhile (readIORef buf >>= return . (<len) . BS.length)
                   (extendBuf sess)
           bufstr' <- readIORef buf
           let (r, bufstr'') = BS.splitAt len bufstr'
           writeIORef buf bufstr''
           return r
    bsPut (TlsSession s h _) bs =
        do withForeignPtr fptr $ \ptr -> (tlsSend s (plusPtr ptr off) len)
           return ()
        where (fptr, off, len) = BSB.toForeignPtr bs
    bsPutNoFlush = bsPut
    bsFlush _ = return ()
    bsClose (TlsSession s _ _) = bye s ShutRdwr


