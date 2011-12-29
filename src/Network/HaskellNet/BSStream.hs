module Network.HaskellNet.BSStream
    ( BSStream(..)
    , bsPutCrLf
    , bsPuts
    , bsPutStrLn
    , handleToStream
    )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.IO

data BSStream =
    BSStream { bsGetLine :: IO ByteString
             , bsGet :: Int -> IO ByteString
             , bsPut :: ByteString -> IO ()
             , bsFlush :: IO ()
             , bsClose :: IO ()
             , bsIsOpen :: IO Bool
             }

lf, crlf :: BS.ByteString
lf = BS.singleton '\n'
crlf = BS.pack "\r\n"

bsPutCrLf :: BSStream -> ByteString -> IO ()
bsPutCrLf h s = bsPut h s >> bsPut h crlf >> bsFlush h

bsPuts :: BSStream -> [ByteString] -> IO ()
bsPuts h strs = mapM_ (bsPut h) strs >> bsFlush h

bsPutStrLn :: BSStream -> ByteString -> IO ()
bsPutStrLn h s = bsPut h s >> bsPut h lf >> bsFlush h

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
             }
