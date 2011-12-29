module Network.HaskellNet.POP3.Connection
    ( POP3Connection
    , stream
    , newConnection
    , apopKey
    )
where

import Network.HaskellNet.BSStream

data POP3Connection =
    POP3C { stream :: !BSStream
          , apopKey :: !String -- ^ APOP key
          }

newConnection :: BSStream -> String -> POP3Connection
newConnection = POP3C
