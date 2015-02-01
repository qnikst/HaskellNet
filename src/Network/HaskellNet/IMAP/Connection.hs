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
newConnection s = IMAPC s <$> (mailboxManagerIORef <$> (newIORef emptyMboxInfo) <*> (newIORef 0))

mailboxManagerIORef :: IORef MailboxInfo -> IORef Int -> MailboxManager IO
mailboxManagerIORef mboxInfoIORef nextCommandNumIORef =
        MBManager (getMailboxInfoIORef mboxInfoIORef)
                  (mailboxIORef mboxInfoIORef)
                  (existsIORef mboxInfoIORef)
                  (recentIORef mboxInfoIORef)
                  (flagsIORef mboxInfoIORef)
                  (permanentFlagsIORef mboxInfoIORef)
                  (isWritableIORef mboxInfoIORef)
                  (isFlagWritableIORef mboxInfoIORef)
                  (uidNextIORef mboxInfoIORef)
                  (uidValidityIORef mboxInfoIORef)
                  (withNextCommandNumIORef nextCommandNumIORef)
                  (setMailboxInfoIORef mboxInfoIORef)
                  (modifyMailboxInfoIORef mboxInfoIORef)

getMailboxInfoIORef :: IORef MailboxInfo -> IO MailboxInfo
getMailboxInfoIORef = readIORef

mailboxIORef :: IORef MailboxInfo -> IO MailboxName
mailboxIORef mboxInfo = _mailbox <$> getMailboxInfoIORef mboxInfo

existsIORef :: IORef MailboxInfo -> IO Integer
existsIORef mboxInfo = _exists <$> getMailboxInfoIORef mboxInfo

recentIORef :: IORef MailboxInfo -> IO Integer
recentIORef mboxInfo = _recent <$> getMailboxInfoIORef mboxInfo

flagsIORef :: IORef MailboxInfo -> IO [Flag]
flagsIORef mboxInfo = _flags <$> getMailboxInfoIORef mboxInfo

permanentFlagsIORef :: IORef MailboxInfo -> IO [Flag]
permanentFlagsIORef mboxInfo = _permanentFlags <$> getMailboxInfoIORef mboxInfo

isWritableIORef :: IORef MailboxInfo -> IO Bool
isWritableIORef mboxInfo = _isWritable <$> getMailboxInfoIORef mboxInfo

isFlagWritableIORef :: IORef MailboxInfo -> IO Bool
isFlagWritableIORef mboxInfo = _isFlagWritable <$> getMailboxInfoIORef mboxInfo

uidNextIORef :: IORef MailboxInfo -> IO UID
uidNextIORef mboxInfo = _uidNext <$> getMailboxInfoIORef mboxInfo

uidValidityIORef :: IORef MailboxInfo -> IO UID
uidValidityIORef mboxInfo = _uidValidity <$> getMailboxInfoIORef mboxInfo

withNextCommandNumIORef :: IORef Int -> (Int -> IO a) -> IO (a, Int)
withNextCommandNumIORef ref act = do
  num <- readIORef ref
  result <- act num
  modifyIORef ref (+1)
  return (result, num)

setMailboxInfoIORef :: IORef MailboxInfo -> MailboxInfo -> IO ()
setMailboxInfoIORef mboxInfo = writeIORef mboxInfo

modifyMailboxInfoIORef :: IORef MailboxInfo -> (MailboxInfo -> MailboxInfo) -> IO ()
modifyMailboxInfoIORef mboxInfo f = modifyIORef mboxInfo f
