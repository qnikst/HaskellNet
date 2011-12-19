module Network.HaskellNet.POP3.Connection
    ( POP3Connection
    , newConnection
    , apopKey
    )
where

import Network.HaskellNet.BSStream

data BSStream s => POP3Connection s =
    POP3C { stream :: !s
          , apopKey :: !String -- ^ APOP key
          }

newConnection :: BSStream s => s -> String -> POP3Connection s
newConnection = POP3C

instance BSStream s => BSStream (POP3Connection s) where
    bsGetLine = bsGetLine . stream
    bsGet = bsGet . stream
    bsPut h s = bsPut (stream h) s
    bsPutStrLn h s = bsPutStrLn (stream h) s
    bsPutNoFlush = bsPutNoFlush . stream
    bsFlush = bsFlush . stream
    bsClose = bsClose . stream
    bsIsOpen = bsIsOpen . stream
