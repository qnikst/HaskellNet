module Main (main) where

import Text.Packrat.IMAPParsers

import Test.HUnit

baseTest =
    [(OK Nothing "LOGIN Completed", MboxUpdate Nothing Nothing)
     ~=? eval' pNone "A001"
             "* OK [ALERT] System shutdown in 10 minutes\r\n\
             \A001 OK LOGIN Completed\r\n"
    ,(NO Nothing "COPY failed: disk is full", MboxUpdate Nothing Nothing)
     ~=?  eval' pNone "A223"
              "* NO Disk is 98% full, please delete unnecessary data\r\n\
              \* NO Disk is 99% full, please delete unnecessary data\r\n\
              \A223 NO COPY failed: disk is full\r\n"
    ]

capabilityTest =
    (OK Nothing "CAPABILITY completed"
    , MboxUpdate Nothing Nothing
    , ["IMAP4rev1", "STARTTLS", "AUTH=GSSAPI", "LOGINDISABLED"])
    ~=? eval' pCapability "abcd"
            "* CAPABILITY IMAP4rev1 STARTTLS AUTH=GSSAPI LOGINDISABLED\r\n\
            \abcd OK CAPABILITY completed\r\n"

noopTest =
    ( OK Nothing "NOOP completed"
    , MboxUpdate (Just 23) (Just 3) )
    ~=?  eval' pNone "a047"
             "* 22 EXPUNGE\r\n\
             \* 23 EXISTS\r\n\
             \* 3 RECENT\r\n\
             \* 14 FETCH (FLAGS (\\Seen \\Deleted))\r\n\
             \a047 OK NOOP completed\r\n"

selectTest =
    [ ( OK (Just READ_WRITE) "SELECT completed"
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
        ,([], "/", "foo/bar")])
      ~=? eval' pList "A682" "* LIST () \"/\" blurdybloop\r\n\
                             \* LIST (\\Noselect) \"/\" foo\r\n\
                             \* LIST () \"/\" foo/bar\r\n\
                             \A682 OK LIST completed\r\n"
    , ( OK Nothing "LSUB completed"
      , MboxUpdate Nothing Nothing
      , [([], ".", "#news.comp.mail.mime")
        ,([], ".", "#news.comp.mail.misc")])
      ~=? eval' pLsub "A002" "* LSUB () \".\" #news.comp.mail.mime\r\n\
                             \* LSUB () \".\" #news.comp.mail.misc\r\n\
                             \A002 OK LSUB completed\r\n"
    ]

statusTest =
    ( OK Nothing "STATUS completed"
                  , MboxUpdate Nothing Nothing
                  , [(MESSAGES, 231), (UIDNEXT, 44292)])
    ~=? eval' pStatus "A042"
            "* STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292)\r\n\
            \A042 OK STATUS completed\r\n"

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


testData = [ "base" ~: baseTest
           , "capability" ~: capabilityTest
           , "noop" ~: noopTest
           , "select" ~: selectTest
           , "list" ~: listTest
           , "status" ~: statusTest
           , "expunge" ~: expungeTest
           , "search" ~: searchTest
           , "fetch" ~: fetchTest
           ]


main = runTestTT (test testData)