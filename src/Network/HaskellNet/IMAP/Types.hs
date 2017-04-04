module Network.HaskellNet.IMAP.Types
    ( MailboxName
    , UID
    , Charset
    , MailboxInfo(..)
    , Flag(..)
    , Attribute(..)
    , MboxUpdate(..)
    , StatusCode(..)
    , ServerResponse(..)
    , MailboxStatus(..)
    , RespDerivs(..)
    , emptyMboxInfo
    )
where

import Data.Word
    ( Word64
    )
import Text.Packrat.Parse
    ( Result
    , Derivs(..)
    )
import Text.Packrat.Pos
    ( Pos
    )

type MailboxName = String
type UID = Word64
type Charset = String

data MailboxInfo = MboxInfo { _mailbox :: MailboxName
                            , _exists :: Integer
                            , _recent :: Integer
                            , _flags :: [Flag]
                            , _permanentFlags :: [Flag]
                            , _isWritable :: Bool
                            , _isFlagWritable :: Bool
                            , _uidNext :: UID
                            , _uidValidity :: UID
                            }
                 deriving (Show, Eq)

data Flag = Seen
          | Answered
          | Flagged
          | Deleted
          | Draft
          | Recent
          | Keyword String
            deriving Eq

instance Show Flag where
    showsPrec d f = showParen (d > app_prec) $ showString $ showFlag f
        where app_prec = 10
              showFlag Seen        = "\\Seen"
              showFlag Answered    = "\\Answered"
              showFlag Flagged     = "\\Flagged"
              showFlag Deleted     = "\\Deleted"
              showFlag Draft       = "\\Draft"
              showFlag Recent      = "\\Recent"
              showFlag (Keyword s) = "\\" ++ s

data Attribute = Noinferiors
               | Noselect
               | Marked
               | Unmarked
               | OtherAttr String
                 deriving (Show, Eq)

data MboxUpdate = MboxUpdate { exists :: Maybe Integer
                             , recent :: Maybe Integer }
                deriving (Show, Eq)

data StatusCode = ALERT
                | BADCHARSET [Charset]
                | CAPABILITY_sc [String]
                | PARSE
                | PERMANENTFLAGS [Flag]
                | READ_ONLY
                | READ_WRITE
                | TRYCREATE
                | UIDNEXT_sc UID
                | UIDVALIDITY_sc UID
                | UNSEEN_sc Integer
                  deriving (Eq, Show)

data ServerResponse = OK (Maybe StatusCode) String
                    | NO (Maybe StatusCode) String
                    | BAD (Maybe StatusCode) String
                    | PREAUTH (Maybe StatusCode) String
                      deriving (Eq, Show)


-- | the query data type for the status command
data MailboxStatus = MESSAGES     -- ^ the number of messages in the mailbox
                   | RECENT       -- ^ the number of messages with the \Recent flag set
                   | UIDNEXT      -- ^ the next unique identifier value of the mailbox
                   | UIDVALIDITY  -- ^ the unique identifier validity value of the mailbox
                   | UNSEEN       -- ^ the number of messages with the \Unseen flag set
                     deriving (Show, Read, Eq)




data RespDerivs =
    RespDerivs { dvFlags  :: Result RespDerivs [Flag]
               , advTag   :: Result RespDerivs String
               , advChar  :: Result RespDerivs Char
               , advPos   :: Pos
               }

instance Derivs RespDerivs where
    dvChar = advChar
    dvPos  = advPos

emptyMboxInfo :: MailboxInfo
emptyMboxInfo = MboxInfo "" 0 0 [] [] False False 0 0
