-- |This module provides a byte string \"stream\" interface.  This
-- interface provides some common operations on a value which
-- supports reading and writing byte strings.
module Network.HaskellNet.BSStream
    ( BSStream(..)
    , handleToStream
    )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.IO

-- |A byte string stream.
data BSStream =
    BSStream { bsGetLine :: IO ByteString
             -- ^Read a line from the stream.  Should return the line
             -- which was read, including the newline.
             , bsGet :: Int -> IO ByteString
             -- ^Read the specified number of bytes from the stream.
             -- Should block until the requested bytes can be read.
             , bsPut :: ByteString -> IO ()
             -- ^Write the specified byte string to the stream.
             -- Should flush the stream after writing.
             , bsFlush :: IO ()
             -- ^Flush the stream.
             , bsClose :: IO ()
             -- ^Close the stream.
             , bsIsOpen :: IO Bool
             -- ^Is the stream open?
             , bsWaitForInput :: Int -> IO Bool
             -- ^Is data available?
             }

-- |Build a byte string stream which operates on a 'Handle'.
handleToStream :: Handle -> BSStream
handleToStream h =
    BSStream { bsGetLine = BS.hGetLine h
             , bsGet = BS.hGet h
             , bsPut = \s -> BS.hPut h s >> hFlush h
             , bsFlush = hFlush h
             , bsClose = do
                 op <- hIsOpen h
                 if op then (hClose h) else return ()
             , bsIsOpen = hIsOpen h
             , bsWaitForInput = hWaitForInput h
             }
