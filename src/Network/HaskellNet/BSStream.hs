-- |This module provides a byte string \"stream\" interface.  This
-- interface provides some common operations on a value which
-- supports reading and writing byte strings.
module Network.HaskellNet.BSStream
    ( BSStream(..)
    , handleToStream
    )
where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           System.IO
import           Control.Monad (when)

-- |A byte string stream.
data BSStream m =
    BSStream { bsGetLine :: m ByteString
             -- ^Read a line from the stream.  Should return the line
             -- which was read, including the newline.
             , bsGet :: Int -> m ByteString
             -- ^Read the specified number of bytes from the stream.
             -- Should block until the requested bytes can be read.
             , bsPut :: ByteString -> m ()
             -- ^Write the specified byte string to the stream.
             -- Should flush the stream after writing.
             , bsFlush :: m ()
             -- ^Flush the stream.
             , bsClose :: m ()
             -- ^Close the stream.
             , bsIsOpen :: m Bool
             -- ^Is the stream open?
             }

handleToStream :: Handle -> BSStream IO
handleToStream h =
    BSStream { bsGetLine = BS.hGetLine h
            , bsGet = BS.hGet h
            , bsPut = \s -> BS.hPut h s >> hFlush h
            , bsFlush = hFlush h
            , bsClose = do
                op <- hIsOpen h
                when op $ hClose h
            , bsIsOpen = hIsOpen h
            }
