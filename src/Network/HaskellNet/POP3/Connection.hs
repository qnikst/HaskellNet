module Network.HaskellNet.POP3.Connection
    ( POP3Connection
    , stream
    , newConnection
    , apopKey
    )
where

import Network.HaskellNet.BSStream

data POP3Connection m =
    POP3C { stream :: !(BSStream m)
          , apopKey :: !String -- ^ APOP key
          }

newConnection :: BSStream m -> String -> POP3Connection m
newConnection = POP3C
