{-# OPTIONS_GHC -cpp -fglasgow-exts -package hsgnutls -package HaskellNet #-}
-- examples to connect server by hsgnutls

module DebugStream
    ( connectD
    , connectDPort
    , DebugStream
    , module BS
    )
    where

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

newtype (BSStream s) => DebugStream s = DS s

withDebug :: BSStream s => s -> DebugStream s
withDebug = DS

connectD :: HostName -> PortNumber -> IO (DebugStream Handle)
connectD host port = connectDPort host (PortNumber port)

connectDPort :: HostName -> PortID -> IO (DebugStream Handle)
connectDPort host port =
    do h <- connectTo host port
       hPutStrLn stderr "connected"
       return $ DS h


instance (BSStream s) => BSStream (DebugStream s) where
#if defined(__GLASGOW_HASKELL__)
    bsGetLine (DS h) =
        do hPutStr stderr "reading with bsGetLine..."
           hFlush stderr
           l <- bsGetLine h
           BS.hPutStrLn stderr l
           return l
    bsGetLines (DS h) =
        do hPutStr stderr "reading with bsGetLines..."
           hFlush stderr
           ls <- bsGetLines h
           mapM_ (BS.hPutStrLn stderr) ls
           return ls
    bsGetNonBlocking (DS h) len =
        do hPutStr stderr $ "reading with bsGetNonBlocking "++show len++"..."
           hFlush stderr
           bl <- bsGetNonBlocking h len
           BS.hPutStrLn stderr bl
           return bl
#endif
    bsGetContents (DS h) =
        do hPutStr stderr "reading with bsGetContents..."
           hFlush stderr
           cont <- bsGetContents h
           BS.hPutStrLn stderr cont
           return cont
    bsGet (DS h) len =
        do hPutStr stderr $ "reading with bsGet "++show len++"..."
           hFlush stderr
           chunk <- bsGet h len
           BS.hPutStrLn stderr chunk
           return chunk
    bsPut (DS h) s =
        do hPutStr stderr "putting with bsPut ("
           BS.hPutStrLn stderr s
           hPutStr stderr (")...")
           hFlush stderr
           bsPut h s
           bsFlush h
           hPutStrLn stderr "done"
           return ()
    bsPutStrLn (DS h) s =
        do hPutStr stderr "putting with bsPutStrLn("
           BS.hPutStrLn stderr s
           hPutStr stderr (")...")
           hFlush stderr
           bsPutStrLn h s
           bsFlush h
           hPutStrLn stderr "done"
           return ()
    bsPutCrLf (DS h) s =
        do hPutStr stderr "putting with bsPutCrLf("
           BS.hPutStrLn stderr s
           hPutStr stderr (")...")
           hFlush stderr
           bsPutCrLf h s
           bsFlush h
           hPutStrLn stderr "done"
           return ()
    bsPutNoFlush (DS h) s =
        do hPutStr stderr "putting with bsPutNoFlush ("
           BS.hPutStrLn stderr s
           hPutStr stderr (")...")
           hFlush stderr
           bsPut h s
           hPutStrLn stderr "done"
           return ()
    bsFlush (DS h) = bsFlush h
    bsClose (DS h) = bsClose h
