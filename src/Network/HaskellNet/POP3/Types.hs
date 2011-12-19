module Network.HaskellNet.POP3.Types
    ( Command(..)
    , Response(..)
    )
where

import Network.HaskellNet.Auth

data Command = USER UserName
             | PASS Password
             | APOP UserName Password
             | AUTH AuthType UserName Password
             | NOOP
             | QUIT
             | STAT
             | LIST (Maybe Int)
             | DELE Int
             | RETR Int
             | RSET
             | TOP Int Int
             | UIDL (Maybe Int)

data Response = Ok | Err
                deriving (Eq, Show)
