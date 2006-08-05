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
import qualified Data.ByteString as BS
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
    bsClose :: h -> IO ()


instance BSStream Handle where
#if defined(__GLASGOW_HASKELL__)
    bsGetLine = BS.hGetLine
    bsGetLines = BS.hGetLines
    bsGetNonBlocking = BS.hGetNonBlocking
#endif
    bsGetContents = BS.hGetContents
    bsGet = BS.hGet
    bsPut = BS.hPut
    bsPutStrLn = BS.hPutStrLn
    bsClose = hClose
