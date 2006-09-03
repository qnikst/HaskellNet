{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HaskellNet.Stream
-- Copyright   :  (c) Jun Mukai 2006
-- License     :  BSD
--
-- Maintainer  :  mukai@jmuk.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A library for abstracting sockets suitable to Streams.
--
--      
-----------------------------------------------------------------------------

module HaskellNet.BSStream
    ( BSStream(..)
    )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans
import System.IO
import Network



class BSStream h where
#if defined(__GLASGOW_HASKELL__)
    bsGetLine :: h -> IO ByteString
    bsGetLines :: h -> IO [ByteString]
    bsGetNonBlocking :: h -> Int -> IO ByteString
#endif
    bsGetContents :: h -> IO ByteString
    bsGet :: h -> Int -> IO ByteString
    bsPut :: h -> ByteString -> IO ()
    bsPutStrLn :: h -> ByteString -> IO ()
    bsPutCrLf  :: h -> ByteString -> IO ()
    bsPutNoFlush :: h -> ByteString -> IO ()
    bsFlush :: h -> IO ()
    bsClose :: h -> IO ()

    bsPutCrLf h s = bsPut h s >> bsPut h crlf
    bsPutStrLn h s = bsPut h s >> bsPut h lf

lf   = BS.singleton '\n' 
crlf = BS.pack "\r\n"

instance BSStream Handle where
#if defined(__GLASGOW_HASKELL__)
    bsGetLine = BS.hGetLine
    bsGetLines = BS.hGetLines
    bsGetNonBlocking = BS.hGetNonBlocking
#endif
    bsGetContents = BS.hGetContents
    bsGet = BS.hGet
    bsPut h s = BS.hPut h s >> bsFlush h
    bsPutStrLn  h s = BS.hPutStrLn h s >> bsFlush h
    bsPutNoFlush = BS.hPut
    bsFlush = hFlush
    bsClose = hClose
