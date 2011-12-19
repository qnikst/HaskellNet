-- |A library for abstracting sockets suitable to Streams.
module Network.HaskellNet.BSStream
    ( BSStream(..)
    )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.IO

class BSStream h where
    bsGetLine :: h -> IO ByteString
    bsGet :: h -> Int -> IO ByteString
    bsPut :: h -> ByteString -> IO ()
    bsPuts :: h -> [ByteString] -> IO ()
    bsPutStrLn :: h -> ByteString -> IO ()
    bsPutCrLf  :: h -> ByteString -> IO ()
    bsPutNoFlush :: h -> ByteString -> IO ()
    bsFlush :: h -> IO ()
    bsClose :: h -> IO ()
    bsIsOpen :: h -> IO Bool

    bsPuts h strs = mapM_ (bsPut h) strs
    bsPutCrLf h s = bsPut h s >> bsPut h crlf
    bsPutStrLn h s = bsPut h s >> bsPut h lf

lf, crlf :: BS.ByteString
lf   = BS.singleton '\n'
crlf = BS.pack "\r\n"

instance BSStream Handle where
    bsGetLine = BS.hGetLine
    bsGet = BS.hGet
    bsPut h s = BS.hPut h s >> bsFlush h
    bsPutStrLn  h s = BS.hPutStrLn h s >> bsFlush h
    bsPutNoFlush = BS.hPut
    bsFlush = hFlush
    bsClose h = do
      op <- hIsOpen h
      if op then (hClose h) else return ()
    bsIsOpen = hIsOpen
