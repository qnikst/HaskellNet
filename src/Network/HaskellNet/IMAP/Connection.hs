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

data IMAPConnection =
    IMAPC { stream :: BSStream
          , mboxInfo :: IORef MailboxInfo
          , nextCommandNum :: IORef Int
          }

newConnection :: BSStream -> IO IMAPConnection
newConnection s = IMAPC s <$> (newIORef emptyMboxInfo) <*> (newIORef 0)

getMailboxInfo :: IMAPConnection -> IO MailboxInfo
getMailboxInfo c = readIORef $ mboxInfo c

mailbox :: IMAPConnection -> IO MailboxName
mailbox c = _mailbox <$> getMailboxInfo c

exists :: IMAPConnection -> IO Integer
exists c = _exists <$> getMailboxInfo c

recent :: IMAPConnection -> IO Integer
recent c = _recent <$> getMailboxInfo c

flags :: IMAPConnection -> IO [Flag]
flags c = _flags <$> getMailboxInfo c

permanentFlags :: IMAPConnection -> IO [Flag]
permanentFlags c = _permanentFlags <$> getMailboxInfo c

isWritable :: IMAPConnection -> IO Bool
isWritable c = _isWritable <$> getMailboxInfo c

isFlagWritable :: IMAPConnection -> IO Bool
isFlagWritable c = _isFlagWritable <$> getMailboxInfo c

uidNext :: IMAPConnection -> IO UID
uidNext c = _uidNext <$> getMailboxInfo c

uidValidity :: IMAPConnection -> IO UID
uidValidity c = _uidValidity <$> getMailboxInfo c

withNextCommandNum :: IMAPConnection -> (Int -> IO a) -> IO (a, Int)
withNextCommandNum c act = do
  let ref = nextCommandNum c
  num <- readIORef ref
  result <- act num
  modifyIORef ref (+1)
  return (result, num)

setMailboxInfo :: IMAPConnection -> MailboxInfo -> IO ()
setMailboxInfo c = writeIORef (mboxInfo c)

modifyMailboxInfo :: IMAPConnection -> (MailboxInfo -> MailboxInfo) -> IO ()
modifyMailboxInfo c f = modifyIORef (mboxInfo c) f