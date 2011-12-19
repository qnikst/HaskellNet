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

data IMAPConnection s =
    IMAPC { stream :: s
          , mboxInfo :: IORef MailboxInfo
          , nextCommandNum :: IORef Int
          }

instance BSStream s => BSStream (IMAPConnection s) where
    bsGetLine = bsGetLine . stream
    bsGet = bsGet . stream
    bsPut h s = bsPut (stream h) s
    bsPutStrLn h s = bsPutStrLn (stream h) s
    bsPutNoFlush = bsPutNoFlush . stream
    bsFlush = bsFlush . stream
    bsClose = bsClose . stream
    bsIsOpen = bsIsOpen . stream

newConnection :: (BSStream s) => s -> IO (IMAPConnection s)
newConnection s = IMAPC s <$> (newIORef emptyMboxInfo) <*> (newIORef 0)

getMailboxInfo :: IMAPConnection s -> IO MailboxInfo
getMailboxInfo c = readIORef $ mboxInfo c

mailbox :: IMAPConnection s -> IO MailboxName
mailbox c = _mailbox <$> getMailboxInfo c

exists :: IMAPConnection s -> IO Integer
exists c = _exists <$> getMailboxInfo c

recent :: IMAPConnection s -> IO Integer
recent c = _recent <$> getMailboxInfo c

flags :: IMAPConnection s -> IO [Flag]
flags c = _flags <$> getMailboxInfo c

permanentFlags :: IMAPConnection s -> IO [Flag]
permanentFlags c = _permanentFlags <$> getMailboxInfo c

isWritable :: IMAPConnection s -> IO Bool
isWritable c = _isWritable <$> getMailboxInfo c

isFlagWritable :: IMAPConnection s -> IO Bool
isFlagWritable c = _isFlagWritable <$> getMailboxInfo c

uidNext :: IMAPConnection s -> IO UID
uidNext c = _uidNext <$> getMailboxInfo c

uidValidity :: IMAPConnection s -> IO UID
uidValidity c = _uidValidity <$> getMailboxInfo c

withNextCommandNum :: IMAPConnection s -> (Int -> IO a) -> IO (a, Int)
withNextCommandNum c act = do
  let ref = nextCommandNum c
  num <- readIORef ref
  result <- act num
  modifyIORef ref (+1)
  return (result, num)

setMailboxInfo :: IMAPConnection s -> MailboxInfo -> IO ()
setMailboxInfo c = writeIORef (mboxInfo c)

modifyMailboxInfo :: IMAPConnection s -> (MailboxInfo -> MailboxInfo) -> IO ()
modifyMailboxInfo c f = modifyIORef (mboxInfo c) f