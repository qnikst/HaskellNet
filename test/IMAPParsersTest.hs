module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Network.HaskellNet.BSStream
import qualified Network.HaskellNet.IMAP as IMAP
import Network.HaskellNet.IMAP.Connection
import Network.HaskellNet.IMAP.Parsers
import Network.HaskellNet.IMAP.Types
import System.Exit

import Test.HUnit

data ReadStep = ReadLine ByteString | ReadBytes ByteString

scriptedStream :: [ReadStep] -> IO (BSStream, IO ByteString)
scriptedStream steps = do
    input <- newIORef steps
    output <- newIORef []
    return (BSStream
        { bsGetLine = popLine input
        , bsGet = popBytes input
        , bsPut = \bytes -> modifyIORef' output (bytes:)
        , bsFlush = return ()
        , bsClose = return ()
        , bsIsOpen = return True
        , bsWaitForInput = \_ -> return False
        }, B.concat . reverse <$> readIORef output)
  where
    popLine input = do
        steps' <- readIORef input
        case steps' of
            ReadLine line : rest -> writeIORef input rest >> return line
            ReadBytes _ : _ -> assertFailure "expected test stream line, got bytes"
            [] -> assertFailure "test stream exhausted while reading a line"

    popBytes input n = do
        steps' <- readIORef input
        case steps' of
            ReadBytes bytes : rest ->
                let (chunk, remainder) = BS.splitAt n bytes
                    next = if BS.null remainder then rest else ReadBytes remainder : rest
                in writeIORef input next >> return chunk
            ReadLine _ : _ -> assertFailure "expected test stream bytes, got a line"
            [] -> assertFailure "test stream exhausted while reading bytes"

scriptedConnection :: [ReadStep] -> IO (IMAPConnection, IO ByteString)
scriptedConnection steps = do
    (testStream, written) <- scriptedStream steps
    conn <- newConnection testStream
    return (conn, written)

line :: String -> ReadStep
line = ReadLine . BS.pack

bytes :: String -> ReadStep
bytes = ReadBytes . BS.pack

okLine :: String -> ReadStep
okLine = line . ("000000 OK " ++)

commandBytes :: String -> ByteString
commandBytes cmd = BS.pack (cmd ++ "\r\n")

assertCommand :: String -> ByteString -> [ReadStep] -> (IMAPConnection -> IO a) -> Test
assertCommand name expected steps action =
    name ~: TestCase $ do
        (conn, written) <- scriptedConnection steps
        _ <- action conn
        actual <- written
        expected @=? actual


baseTest =
    [(OK Nothing "LOGIN Completed", MboxUpdate Nothing Nothing, ())
     ~=? eval' pNone "A001"
             "* OK [ALERT] System shutdown in 10 minutes\r\n\
             \A001 OK LOGIN Completed\r\n"
    ,(NO Nothing "COPY failed: disk is full", MboxUpdate Nothing Nothing, ())
     ~=?  eval' pNone "A223"
              "* NO Disk is 98% full, please delete unnecessary data\r\n\
              \* NO Disk is 99% full, please delete unnecessary data\r\n\
              \A223 NO COPY failed: disk is full\r\n"
    ,(OK Nothing "LOGOUT completed", MboxUpdate Nothing Nothing, ())
     ~=? eval' pNone "a006"
             "* BYE Courier-IMAP server shutting down\r\n\
             \a006 OK LOGOUT completed\r\n"
    ]

capabilityTest =
    (OK Nothing "CAPABILITY completed"
    , MboxUpdate Nothing Nothing
    , ["IMAP4rev1", "STARTTLS", "AUTH=GSSAPI", "LOGINDISABLED"])
    ~=? eval' pCapability "abcd"
            "* CAPABILITY IMAP4rev1 STARTTLS AUTH=GSSAPI LOGINDISABLED\r\n\
            \abcd OK CAPABILITY completed\r\n"

noopTest =
    ( OK Nothing "NOOP completed", MboxUpdate (Just 23) (Just 3), ())
    ~=?  eval' pNone "a047"
             "* 22 EXPUNGE\r\n\
             \* 23 EXISTS\r\n\
             \* 3 RECENT\r\n\
             \* 14 FETCH (FLAGS (\\Seen \\Deleted))\r\n\
             \a047 OK NOOP completed\r\n"

selectTest =
    [ ( OK (Just READ_WRITE) "SELECT completed"
      , MboxUpdate Nothing Nothing
      , MboxInfo "" 172 1 [Answered, Flagged, Deleted, Seen, Draft]
                     [Deleted, Seen] True True 4392 3857529045 )
      ~=? eval' pSelect "A142"
              "* 172 EXISTS\r\n\
              \* 1 RECENT\r\n\
              \* OK [UNSEEN 12] Message 12 is first unseen\r\n\
              \* OK [UIDVALIDITY 3857529045] UIDs valid\r\n\
              \* OK [UIDNEXT 4392] Predicted next UID\r\n\
              \* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n\
              \* OK [PERMANENTFLAGS (\\Deleted \\Seen \\*)] Limited\r\n\
              \A142 OK [READ-WRITE] SELECT completed\r\n"
    , (OK (Just READ_ONLY) "EXAMINE completed"
      , MboxUpdate Nothing Nothing
      , MboxInfo "" 17 2 [Answered, Flagged, Deleted, Seen, Draft]
                     [] False False 4392 3857529045 )
      ~=? eval' pSelect "A932"
              "* 17 EXISTS\r\n\
              \* 2 RECENT\r\n\
              \* OK [UNSEEN 8] Message 8 is first unseen\r\n\
              \* OK [UIDVALIDITY 3857529045] UIDs valid\r\n\
              \* OK [UIDNEXT 4392] Predicted next UID\r\n\
              \* FLAGS (\\Answered \\Flagged \\Deleted \\Seen \\Draft)\r\n\
              \* OK [PERMANENTFLAGS ()] No permanent flags permitted\r\n\
              \A932 OK [READ-ONLY] EXAMINE completed\r\n"
    ]

listTest =
    [ ( OK Nothing "LIST completed"
      , MboxUpdate Nothing Nothing
      , [([], "/", "blurdybloop")
        ,([Noselect], "/", "foo")
        ,([], "/", "foo/bar")
        ,([], "/", "foo")])
      ~=? eval' pList "A682" "* LIST () \"/\" blurdybloop\r\n\
                             \* LIST (\\Noselect) \"/\" foo\r\n\
                             \* LIST () \"/\" foo/bar\r\n\
                             \* LIST () \"/\" \"foo\"\r\n\
                             \A682 OK LIST completed\r\n"
    , ( OK Nothing "LSUB completed"
      , MboxUpdate Nothing Nothing
      , [([], ".", "#news.comp.mail.mime")
        ,([], ".", "#news.comp.mail.misc")])
      ~=? eval' pLsub "A002" "* LSUB () \".\" #news.comp.mail.mime\r\n\
                             \* LSUB () \".\" #news.comp.mail.misc\r\n\
                             \A002 OK LSUB completed\r\n"
    , ( OK Nothing "LIST completed"
      , MboxUpdate Nothing Nothing
      , [([], "/", "Entwürfe")
        ,([], "/", "A&B")])
      ~=? eval' pList "A003" "* LIST () \"/\" Entw&APw-rfe\r\n\
                             \* LIST () \"/\" A&-B\r\n\
                             \A003 OK LIST completed\r\n"
    ]

statusTest =
    ( OK Nothing "STATUS completed"
                  , MboxUpdate Nothing Nothing
                  , [(MESSAGES, 231), (UIDNEXT, 44292)])
    ~=? eval' pStatus "A042"
            "* STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292)\r\n\
            \A042 OK STATUS completed\r\n"

statusQuotedMailboxTest =
    [ ( OK Nothing "STATUS completed"
      , MboxUpdate Nothing Nothing
      , [(MESSAGES, 231), (UIDNEXT, 44292)])
      ~=? eval' pStatus "A042"
              "* STATUS \"[Gmail]/Alle Nachrichten\" (MESSAGES 231 UIDNEXT 44292)\r\n\
              \A042 OK STATUS completed\r\n"
    , ( OK Nothing "STATUS completed"
      , MboxUpdate Nothing Nothing
      , [(MESSAGES, 1)])
      ~=? eval' pStatus "A043"
              "* STATUS \"foo\\\" bar\" (MESSAGES 1)\r\n\
              \A043 OK STATUS completed\r\n"
    , ( OK Nothing "STATUS completed"
      , MboxUpdate Nothing Nothing
      , [(MESSAGES, 1)])
      ~=? eval' pStatus "A044"
              "* STATUS foo (MESSAGES 1)\r\n\
              \A044 OK STATUS completed\r\n"
    ]

expungeTest =
    ( OK Nothing "EXPUNGE completed"
    , MboxUpdate Nothing Nothing
    , [3, 3, 5, 8])
    ~=? eval' pExpunge "A202" "* 3 EXPUNGE\r\n\
                              \* 3 EXPUNGE\r\n\
                              \* 5 EXPUNGE\r\n\
                              \* 8 EXPUNGE\r\n\
                              \A202 OK EXPUNGE completed\r\n"

searchTest =
    [ ( OK Nothing "SEARCH completed"
          , MboxUpdate Nothing Nothing
          , [2, 84, 882])
      ~=? eval' pSearch "A282" "* SEARCH 2 84 882\r\n\
                               \A282 OK SEARCH completed\r\n"
    , ( OK Nothing "SEARCH completed"
          , MboxUpdate Nothing Nothing
          , [] )
      ~=? eval' pSearch "A283" "* SEARCH\r\n\
                               \A283 OK SEARCH completed\r\n"
    ]

fetchTest =
    [ ( OK Nothing "FETCH completed"
      , MboxUpdate Nothing Nothing
      , [ (12, [("FLAGS", "(\\Seen)")
               ,("INTERNALDATE", "\"17-Jul-1996 02:44:25 -0700\"")
               ,("RFC822.SIZE", "4286")
               ,("ENVELOPE", "(\"Wed, 17 Jul 1996 02:23:25 -0700 (PDT)\" \"IMAP4rev1 WG mtg summary and minutes\" ((\"Terry Gray\" NIL \"gray\" \"cac.washington.edu\")) ((\"Terry Gray\" NIL \"gray\" \"cac.washington.edu\")) ((\"Terry Gray\" NIL \"gray\" \"cac.washington.edu\")) ((NIL NIL \"imap\" \"cac.washington.edu\")) ((NIL NIL \"minutes\" \"CNRI.Reston.VA.US\") (\"John Klensin\" NIL \"KLENSIN\" \"MIT.EDU\")) NIL NIL \"<B27397-0100000@cac.washington.edu>\")")
               ,("BODY", "(\"TEXT\" \"PLAIN\" (\"CHARSET\" \"US-ASCII\") NIL NIL \"7BIT\" 3028 92)")
               ])])
      ~=? eval' pFetch "a003" "* 12 FETCH (FLAGS (\\Seen) INTERNALDATE \"17-Jul-1996 02:44:25 -0700\" RFC822.SIZE 4286 ENVELOPE (\"Wed, 17 Jul 1996 02:23:25 -0700 (PDT)\" \"IMAP4rev1 WG mtg summary and minutes\" ((\"Terry Gray\" NIL \"gray\" \"cac.washington.edu\")) ((\"Terry Gray\" NIL \"gray\" \"cac.washington.edu\")) ((\"Terry Gray\" NIL \"gray\" \"cac.washington.edu\")) ((NIL NIL \"imap\" \"cac.washington.edu\")) ((NIL NIL \"minutes\" \"CNRI.Reston.VA.US\") (\"John Klensin\" NIL \"KLENSIN\" \"MIT.EDU\")) NIL NIL \"<B27397-0100000@cac.washington.edu>\") BODY (\"TEXT\" \"PLAIN\" (\"CHARSET\" \"US-ASCII\") NIL NIL \"7BIT\" 3028 92))\r\n\
                              \a003 OK FETCH completed\r\n"
    , ( OK Nothing "FETCH completed"
          , MboxUpdate Nothing Nothing
          , [ (12, [( "BODY[HEADER]"
                    , "Date: Wed, 17 Jul 1996 02:23:25 -0700 (PDT)\r\n\
                      \From: Terry Gray <gray@cac.washington.edu>\r\n\
                      \Subject: IMAP4rev1 WG mtg summary and minutes\r\n\
                      \To: imap@cac.washington.edu\r\n\
                      \cc: minutes@CNRI.Reston.VA.US, John Klensin <KLENSIN@MIT.EDU>\r\n\
                      \Message-Id: <B27397-0100000@cac.washington.edu>\r\n\
                      \MIME-Version: 1.0\r\n\
                      \Content-Type: TEXT/PLAIN; CHARSET=US-ASCII\r\n\
                      \\r\n" )])])
      ~=? eval' pFetch "a004"
              "* 12 FETCH (BODY[HEADER] {342}\r\n\
              \Date: Wed, 17 Jul 1996 02:23:25 -0700 (PDT)\r\n\
              \From: Terry Gray <gray@cac.washington.edu>\r\n\
              \Subject: IMAP4rev1 WG mtg summary and minutes\r\n\
              \To: imap@cac.washington.edu\r\n\
              \cc: minutes@CNRI.Reston.VA.US, John Klensin <KLENSIN@MIT.EDU>\r\n\
              \Message-Id: <B27397-0100000@cac.washington.edu>\r\n\
              \MIME-Version: 1.0\r\n\
              \Content-Type: TEXT/PLAIN; CHARSET=US-ASCII\r\n\r\n\
              \)\r\n\
              \a004 OK FETCH completed\r\n"
    , ( OK Nothing "+FLAGS completed"
          , MboxUpdate Nothing Nothing
          , [(12, [("FLAGS", "(\\Seen \\Deleted)")])])
      ~=? eval' pFetch "a005" "* 12 FETCH (FLAGS (\\Seen \\Deleted))\r\n\
                              \a005 OK +FLAGS completed\r\n"
    ]

imapCommandTest =
    [ assertCommand "create quotes mailbox"
          (commandBytes "000000 CREATE \"foo bar\"")
          [okLine "CREATE completed"]
          (\conn -> IMAP.create conn "foo bar")
    , assertCommand "delete quotes mailbox"
          (commandBytes "000000 DELETE \"foo bar\"")
          [okLine "DELETE completed"]
          (\conn -> IMAP.delete conn "foo bar")
    , assertCommand "rename quotes mailboxes"
          (commandBytes "000000 RENAME \"old name\" \"new name\"")
          [okLine "RENAME completed"]
          (\conn -> IMAP.rename conn "old name" "new name")
    , assertCommand "subscribe quotes mailbox"
          (commandBytes "000000 SUBSCRIBE \"foo bar\"")
          [okLine "SUBSCRIBE completed"]
          (\conn -> IMAP.subscribe conn "foo bar")
    , assertCommand "unsubscribe quotes mailbox"
          (commandBytes "000000 UNSUBSCRIBE \"foo bar\"")
          [okLine "UNSUBSCRIBE completed"]
          (\conn -> IMAP.unsubscribe conn "foo bar")
    , assertCommand "select escapes mailbox"
          (commandBytes "000000 SELECT \"foo\\\"bar\"")
          [okLine "[READ-WRITE] SELECT completed"]
          (\conn -> IMAP.select conn "foo\"bar")
    , assertCommand "select encodes utf7 mailbox"
          (commandBytes "000000 SELECT \"Entw&APw-rfe\"")
          [okLine "[READ-WRITE] SELECT completed"]
          (\conn -> IMAP.select conn "Entwürfe")
    , assertCommand "select encodes ampersand"
          (commandBytes "000000 SELECT \"A&-B\"")
          [okLine "[READ-WRITE] SELECT completed"]
          (\conn -> IMAP.select conn "A&B")
    , "status quotes mailbox" ~: TestCase $ do
          (conn, written) <- scriptedConnection
              [ line "* STATUS \"foo bar\" (MESSAGES 1)"
              , okLine "STATUS completed"
              ]
          statusResult <- IMAP.status conn "foo bar" [MESSAGES]
          [(MESSAGES, 1)] @=? statusResult
          actual <- written
          commandBytes "000000 STATUS \"foo bar\" (MESSAGES)" @=? actual
    , "append quotes mailbox" ~: TestCase $ do
          let mailData = BS.pack "Body"
          (conn, written) <- scriptedConnection
              [ line "+ Ready for literal"
              , okLine "APPEND completed"
              ]
          IMAP.append conn "foo bar" mailData
          actual <- written
          B.concat [ commandBytes "000000 APPEND \"foo bar\" {6}"
                   , BS.pack "Body\r\n\r\n"
                   ] @=? actual
    , assertCommand "copy quotes mailbox"
          (commandBytes "000000 UID COPY 42 \"foo bar\"")
          [okLine "COPY completed"]
          (\conn -> IMAP.copy conn 42 "foo bar")
    , assertCommand "move quotes mailbox"
          (commandBytes "000000 UID MOVE 42 \"foo bar\"")
          [okLine "MOVE completed"]
          (\conn -> IMAP.move conn 42 "foo bar")
    ]

flagTest =
    [ "keyword show omits backslash" ~:
          "Custom" ~=? show (Keyword "Custom")
    , "permanent wildcard show keeps backslash" ~:
          "\\*" ~=? show (Keyword "*")
    , "keyword parser keeps bare keyword" ~:
          [Keyword "Custom"] ~=? eval' dvFlags "" "(Custom)"
    , "keyword parser keeps permanent wildcard" ~:
          [Keyword "*"] ~=? eval' dvFlags "" "(\\*)"
    , "keyword parser preserves unknown system flag" ~:
          [Keyword "\\Custom"] ~=? eval' dvFlags "" "(\\Custom)"
    ]


imapFetchTest =
    [ "fetch works when BODY precedes UID" ~: TestCase $ do
          (conn, _) <- scriptedConnection
              [ line "* 12 FETCH (BODY[] {5}"
              , bytes "hello"
              , line " UID 999)"
              , okLine "FETCH completed"
              ]
          fetched <- IMAP.fetch conn 999
          BS.pack "hello" @=? fetched
    ]


testData = [ "base" ~: baseTest
           , "capability" ~: capabilityTest
           , "noop" ~: noopTest
           , "select" ~: selectTest
           , "list" ~: listTest
           , "status" ~: TestList [ statusTest, TestList statusQuotedMailboxTest ]
           , "expunge" ~: expungeTest
           , "search" ~: searchTest
           , "fetch" ~: fetchTest
           , "imap commands" ~: imapCommandTest
           , "flags" ~: flagTest
           , "imap fetch api" ~: imapFetchTest
           ]


main = do
    counts <- runTestTT (test testData)
    if errors counts == 0 && failures counts == 0
        then exitSuccess
        else exitFailure
