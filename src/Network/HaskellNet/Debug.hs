module Network.HaskellNet.Debug
    ( debugStream
    )
    where

import Network.HaskellNet.BSStream

import qualified Data.ByteString.Char8 as BS

import System.IO

debugStream :: BSStream -> BSStream
debugStream inner =
    inner { bsGetLine = debugBsGetLine inner
          , bsGet = debugBsGet inner
          , bsPut = debugBsPut inner
          }

debugBsGetLine :: BSStream -> IO BS.ByteString
debugBsGetLine s = do
  hPutStr stderr "reading with bsGetLine..."
  hFlush stderr
  l <- bsGetLine s
  BS.hPutStrLn stderr l
  return l

debugBsGet :: BSStream -> Int -> IO BS.ByteString
debugBsGet s len = do
  hPutStr stderr $ "reading with bsGet "++show len++"..."
  hFlush stderr
  chunk <- bsGet s len
  BS.hPutStrLn stderr chunk
  return chunk

debugBsPut :: BSStream -> BS.ByteString -> IO ()
debugBsPut s str = do
  hPutStr stderr "putting with bsPut ("
  BS.hPutStrLn stderr str
  hPutStr stderr (")...")
  hFlush stderr
  bsPut s str
  bsFlush s
  hPutStrLn stderr "done"
  return ()