{-# LANGUAGE RankNTypes #-}
module Network.HaskellNet.IMAP.Connection
    ( IMAPConnection
    , withNextCommandNum
    , setMailboxInfo
    , modifyMailboxInfo
    , newConnection
    , mailbox
    , exists
    , recent
    , flags
    , permanentFlags
    , isWritable
    , isFlagWritable
    , uidNext
    , uidValidity
    , stream
    )
where

import Data.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    , modifyIORef
    )
import Control.Applicative
    ( (<$>)
    , (<*>)
    )

import Network.HaskellNet.BSStream
import Network.HaskellNet.IMAP.Types
    ( MailboxInfo(..)
    , emptyMboxInfo
    , MailboxName
    , Flag
    , UID
    )

data IMAPConnection m =
    IMAPC { stream :: BSStream m
          , mailboxManager :: MailboxManager m
          }

data MailboxManager m =
    MBManager { _mailboxGetMailboxInfo :: m MailboxInfo
              , _mailboxName :: m MailboxName
              , _mailboxExists :: m Integer
              , _mailboxRecent :: m Integer
              , _mailboxFlags  :: m [Flag]
              , _mailboxPermanentFlags :: m [Flag]
              , _mailboxIsWritable :: m Bool
              , _mailboxIsFlagWritable :: m Bool
              , _mailboxUidNext :: m UID
              , _mailboxUidValidity :: m UID
              , _mailboxWithNextCommandNum :: forall a. (Int -> m a) -> m (a, Int)
              , _mailboxSetMailboxInfo :: MailboxInfo -> m ()
              , _mailboxModifyMailboxInfo :: (MailboxInfo -> MailboxInfo) -> m ()
              , _mailboxNextCommandNum :: m Int
              }

getMailboxInfo :: IMAPConnection m -> m MailboxInfo
getMailboxInfo c = _mailboxGetMailboxInfo (mailboxManager c)

mailbox :: IMAPConnection m -> m MailboxName
mailbox c = _mailboxName (mailboxManager c)

exists :: IMAPConnection m -> m Integer
exists c = _mailboxExists (mailboxManager c)

recent :: IMAPConnection m -> m Integer
recent c = _mailboxRecent (mailboxManager c)

flags :: IMAPConnection m -> m [Flag]
flags c = _mailboxFlags (mailboxManager c)

permanentFlags :: IMAPConnection m -> m [Flag]
permanentFlags c = _mailboxPermanentFlags (mailboxManager c)

isWritable :: IMAPConnection m -> m Bool
isWritable c = _mailboxIsWritable (mailboxManager c)

isFlagWritable :: IMAPConnection m -> m Bool
isFlagWritable c = _mailboxIsFlagWritable (mailboxManager c)

uidNext :: IMAPConnection m -> m UID
uidNext c = _mailboxUidNext (mailboxManager c)

uidValidity :: IMAPConnection m -> m UID
uidValidity c = _mailboxUidValidity (mailboxManager c)

withNextCommandNum :: IMAPConnection m -> (Int -> m a) -> m (a, Int)
withNextCommandNum c = _mailboxWithNextCommandNum (mailboxManager c)

setMailboxInfo :: IMAPConnection m -> MailboxInfo -> m ()
setMailboxInfo c = _mailboxSetMailboxInfo (mailboxManager c)

modifyMailboxInfo :: IMAPConnection m -> (MailboxInfo -> MailboxInfo) -> m ()
modifyMailboxInfo c = _mailboxModifyMailboxInfo (mailboxManager c)

-- IO Monad specific.
newConnection :: BSStream IO -> IO (IMAPConnection IO)
newConnection s = IMAPC s <$> (ioRefMailboxManager <$> (newIORef emptyMboxInfo) <*> (newIORef 0))

ioRefMailboxManager :: IORef MailboxInfo -> IORef Int -> MailboxManager IO
ioRefMailboxManager mboxInfoIORef nextCommandNumIORef = undefined
