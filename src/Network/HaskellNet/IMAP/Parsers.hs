-- | Parsers for IMAP server responses
module Network.HaskellNet.IMAP.Parsers
    ( eval
    , eval'
    , pNone
    , pCapability
    , pSelect
    , pList
    , pLsub
    , pStatus
    , pExpunge
    , pSearch
    , pFetch
    )
where

import Text.Packrat.Parse hiding (space, spaces)
import Text.Packrat.Pos

import Data.Maybe

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Network.HaskellNet.IMAP.Types

eval :: (RespDerivs -> Result RespDerivs r) -> String -> ByteString -> r
eval pMain tag s = case pMain (parse tag (Pos tag 1 1) s) of
                     Parsed v _ _ -> v
                     NoParse e    -> error (show e)

parse :: String -> Pos -> ByteString -> RespDerivs
parse tagstr pos s = d
    where d    = RespDerivs flag tag chr pos
          flag = pParenFlags d
          tag  = Parsed tagstr d (nullError d)
          chr  = if BS.null s
                 then NoParse (eofError d)
                 else let (c, s') = (BS.head s, BS.tail s)
                      in Parsed c (parse tagstr (nextPos pos c) s')
                           (nullError d)

eval' :: (RespDerivs -> Result RespDerivs r) -> String -> String -> r
eval' pMain tag s = case pMain (parse' tag (Pos tag 1 1) s) of
                      Parsed v _ _ -> v
                      NoParse e    -> error (show e)

parse' :: String -> Pos -> String -> RespDerivs
parse' tagstr pos s = d
    where d    = RespDerivs flag tag chr pos
          flag = pParenFlags d
          tag  = Parsed tagstr d (nullError d)
          chr  = case s of
                   (c:s') -> Parsed c (parse' tagstr (nextPos pos c) s')
                               (nullError d)
                   _      -> NoParse (eofError d)

mkMboxUpdate :: [Either (String, Integer) b] -> (MboxUpdate, [b])
mkMboxUpdate untagged = (MboxUpdate exists' recent', others)
    where exists' = lookup "EXISTS" $ catLefts untagged
          recent' = lookup "RECENT" $ catLefts untagged
          others = catRights untagged

pNone :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, ())
Parser pNone =
    do untagged <- many pOtherLine
       resp <- Parser pDone
       let (mboxUp, _) = mkMboxUpdate untagged
       return (resp, mboxUp, ())

pCapability :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, [String])
Parser pCapability =
    do untagged <- many (pCapabilityLine <|> pOtherLine)
       resp <- Parser pDone
       let (mboxUp, caps) = mkMboxUpdate untagged
       return (resp, mboxUp, concat caps)

pList :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, [([Attribute], String, MailboxName)])
Parser pList =
    do untagged <- many (pListLine "LIST" <|> pOtherLine)
       resp <- Parser pDone
       let (mboxUp, listRes) = mkMboxUpdate untagged
       return (resp, mboxUp, listRes)

pLsub :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, [([Attribute], String, MailboxName)])
Parser pLsub =
    do untagged <- many (pListLine "LSUB" <|> pOtherLine)
       resp <- Parser pDone
       let (mboxUp, listRes) = mkMboxUpdate untagged
       return (resp, mboxUp, listRes)

pStatus :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, [(MailboxStatus, Integer)])
Parser pStatus =
    do untagged <- many (pStatusLine <|> pOtherLine)
       resp <- Parser pDone
       let (mboxUp, statRes) = mkMboxUpdate untagged
       return (resp, mboxUp, concat statRes)

pExpunge :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, [Integer])
Parser pExpunge =
    do untagged <- many ((do string "* "
                             n <- pExpungeLine
                             return $ Right ("EXPUNGE", n))
                         <|> pOtherLine)
       resp <- Parser pDone
       let (mboxUp, expunges) = mkMboxUpdate untagged
       return (resp, mboxUp, lookups "EXPUNGE" expunges)

pSearch :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, [UID])
Parser pSearch =
    do untagged <- many (pSearchLine <|> pOtherLine)
       resp <- Parser pDone
       let (mboxUp, searchRes) = mkMboxUpdate untagged
       return (resp, mboxUp, concat searchRes)


pSelect :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, MailboxInfo)
Parser pSelect =
    do untagged <- many (pSelectLine
                         <|> (do string "* "
                                 anyChar `manyTill` crlfP
                                 return id))
       resp <- Parser pDone
       let box = case resp of
                   OK writable _ ->
                       emptyBox { _isWritable = isJust writable && fromJust writable == READ_WRITE }
                   _ -> emptyBox
       return (resp, MboxUpdate Nothing Nothing, foldl (flip ($)) box untagged)
    where emptyBox = MboxInfo "" 0 0 [] [] False False 0 0

pFetch :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, [(Integer, [(String, String)])])
Parser pFetch =
    do untagged <- many (pFetchLine <|> pOtherLine)
       resp <- Parser pDone
       let (mboxUp, fetchRes) = mkMboxUpdate untagged
       return (resp, mboxUp, fetchRes)

pDone :: RespDerivs -> Result RespDerivs ServerResponse
Parser pDone = do tag <- Parser advTag
                  string tag >> space
                  respCode <- parseCode
                  space
                  stat <- optional (do s <- parseStatusCode
                                       space >> return s)
                  body <- anyChar `manyTill` crlfP
                  return $ respCode stat body
    where parseCode = choice $ [ string "OK" >> return OK
                               , string "NO" >> return NO
                               , string "BAD" >> return BAD
                               , string "PREAUTH" >> return PREAUTH
                               ]
          parseStatusCode =
              between (char '[') (char ']') $
              choice [ string "ALERT" >> return ALERT
                     , do { string "BADCHARSET"
                          ; ws <- optional parenWords
                          ; return $ BADCHARSET $ fromMaybe [] ws }
                     , do { string "CAPABILITY"
                          ; space
                          ; ws <- (many1 $ noneOf " ]") `sepBy1` space
                          ; return $ CAPABILITY_sc ws }
                     , string "PARSE" >> return PARSE
                     , do { string "PERMANENTFLAGS" >> space >> char '('
                          ; fs <- pFlag `sepBy1` spaces1
                          ; char ')'
                          ; return $ PERMANENTFLAGS fs }
                     , string "READ-ONLY" >> return READ_ONLY
                     , string "READ-WRITE" >> return READ_WRITE
                     , string "TRYCREATE" >> return TRYCREATE
                     , do { string "UNSEEN" >> space
                          ; num <- many1 digit
                          ; return $ UNSEEN_sc $ read num }
                     , do { string "UIDNEXT" >> space
                          ; num <- many1 digit
                          ; return $ UIDNEXT_sc $ read num }
                     , do { string "UIDVALIDITY" >> space
                          ; num <- many1 digit
                          ; return $ UIDVALIDITY_sc $ read num }
                     ]
          parenWords = between (space >> char '(') (char ')')
                         (many1 (noneOf " )") `sepBy1` space)

pFlag :: Parser RespDerivs Flag
pFlag = do char '\\'
           choice [ string "Seen"     >> return Seen
                  , string "Answered" >> return Answered
                  , string "Flagged"  >> return Flagged
                  , string "Deleted"  >> return Deleted
                  , string "Draft"    >> return Draft
                  , string "Recent"   >> return Recent
                  , char '*'          >> return (Keyword "*")
                  , many1 atomChar    >>= return . Keyword ]
    <|> (many1 atomChar >>= return . Keyword)

pParenFlags :: RespDerivs -> Result RespDerivs [Flag]
Parser pParenFlags = do char '('
                        fs <- pFlag `sepBy` space
                        char ')'
                        return fs

atomChar :: Derivs d => Parser d Char
atomChar = noneOf " (){%*\"\\]"

pNumberedLine :: String -> Parser RespDerivs Integer
pNumberedLine str = do num <- many1 digit
                       space
                       string str
                       crlfP
                       return $ read num

pExistsLine, pRecentLine, pExpungeLine :: Parser RespDerivs Integer
pExistsLine  = pNumberedLine "EXISTS"
pRecentLine  = pNumberedLine "RECENT"
pExpungeLine = pNumberedLine "EXPUNGE"

pOtherLine :: Parser RespDerivs (Either (String, Integer) b)
pOtherLine = do string "* "
                choice [ pExistsLine >>= \n -> return (Left ("EXISTS", n))
                       , pRecentLine >>= \n -> return (Left ("RECENT", n))
                       , blankLine >> return (Left ("", 0))]
    where blankLine = anyChar `manyTill` crlfP


pCapabilityLine :: Parser RespDerivs (Either a [String])
pCapabilityLine = do string "* CAPABILITY "
                     ws <- many1 (noneOf " \r") `sepBy` space
                     crlfP
                     return $ Right ws

pListLine :: String
          -> Parser RespDerivs (Either a ([Attribute], String, MailboxName))
pListLine list =
    do string "* " >> string list >> space
       attrs <- parseAttrs
       sep <- parseSep
       mbox <- parseMailbox
       return $ Right (attrs, sep, mbox)
    where parseAttr =
              do char '\\'
                 choice [ string "Noinferiors" >> return Noinferiors
                        , string "Noselect" >> return Noselect
                        , string "Marked" >> return Marked
                        , string "Unmarked" >> return Unmarked
                        , many atomChar >>= return . OtherAttr
                        ]
          parseAttrs = do char '('
                          attrs <- parseAttr `sepBy` space
                          char ')'
                          return attrs
          parseSep = space >> char '"' >> anyChar `manyTill` char '"'
          parseMailbox = do space
                            q <- optional $ char '"'
                            case q of
                                Just _  -> do mbox <- anyChar `manyTill` char '"'
                                              anyChar `manyTill` crlfP
                                              return mbox
                                Nothing -> anyChar `manyTill` crlfP

pStatusLine :: Parser RespDerivs (Either a [(MailboxStatus, Integer)])
pStatusLine =
    do string "* STATUS "
       mbox <- parseMailbox
       stats <- between (char '(') (char ')') (parseStat `sepBy1` space)
       crlfP
       return $ Right stats
    where parseStat =
              do cons <- choice [ string "MESSAGES"    >>= return . read
                                , string "RECENT"      >>= return . read
                                , string "UIDNEXT"     >>= return . read
                                , string "UIDVALIDITY" >>= return . read
                                , string "UNSEEN"      >>= return . read
                                ]
                 space
                 num <- many1 digit >>= return . read
                 return (cons, num)
          parseMailbox = do
                            q <- optional $ char '"'
                            case q of
                                Just _  -> do mbox <- anyChar `manyTill` char '"'; space; return mbox
                                Nothing -> anyChar `manyTill` space

pSearchLine :: Parser RespDerivs (Either a [UID])
pSearchLine = do string "* SEARCH "
                 nums <- (many1 digit) `sepBy` space
                 crlfP
                 return $ Right $ map read nums

pSelectLine :: Parser RespDerivs (MailboxInfo -> MailboxInfo)
pSelectLine =
    do string "* "
       choice [ pExistsLine >>= \n -> return (\mbox -> mbox { _exists = n })
              , pRecentLine >>= \n -> return (\mbox -> mbox { _recent = n })
              , pFlags  >>= \fs -> return (\mbox -> mbox { _flags = fs })
              , string "OK " >> okResps ]
    where pFlags = do string "FLAGS "
                      char '('
                      fs <- pFlag `sepBy` space
                      char ')' >> crlfP
                      return fs
          okResps =
              do char '['
                 v <- choice [ do { string "UNSEEN "
                                  ; many1 digit
                                  ; return id }
                             , do { string "PERMANENTFLAGS ("
                                  ; fs <- pFlag `sepBy` space
                                  ; char ')'
                                  ; return $ \mbox ->
                                      mbox { _isFlagWritable =
                                               Keyword "*" `elem` fs
                                           , _permanentFlags =
                                               filter (/= Keyword "*") fs } }
                             , do { string "UIDNEXT "
                                  ; n <- many1 digit
                                  ; return $ \mbox ->
                                      mbox { _uidNext = read n } }
                             , do { string "UIDVALIDITY "
                                  ; n <- many1 digit
                                  ; return $ \mbox ->
                                      mbox { _uidValidity = read n } }
                             ]
                 char ']'
                 anyChar `manyTill` crlfP
                 return v

pFetchLine :: Parser RespDerivs (Either a (Integer, [(String, String)]))
pFetchLine =
    do string "* "
       num <- many1 digit
       string " FETCH" >> spaces
       char '('
       pairs <- pPair `manyTill` char ')'
       crlfP
       return $ Right $ (read num, pairs)
    where pPair = do key <- (do k  <- anyChar `manyTill` char '['
                                ps <- anyChar `manyTill` char ']'
                                space
                                return (k++"["++ps++"]"))
                        <|> anyChar `manyTill` space
                     value <- (do char '('
                                  v <- pParen `sepBy` space
                                  char ')'
                                  return ("("++unwords v++")"))
                          <|> (do char '{'
                                  num <- many1 digit >>= return . read
                                  char '}' >> crlfP
                                  sequence $ replicate num anyChar)
                          <|> (do char '"'
                                  v <- noneOf "\"" `manyTill` char '"'
                                  return ("\""++v++"\""))
                          <|> many1 atomChar
                     spaces
                     return (key, value)
          pParen = (do char '"'
                       v <- noneOf "\"" `manyTill` char '"'
                       return ("\""++v++"\""))
               <|> (do char '('
                       v <- pParen `sepBy` space
                       char ')'
                       return ("("++unwords v++")"))
               <|> (do char '\\'
                       v <- many1 atomChar
                       return ('\\':v))
               <|> many1 atomChar

----------------------------------------------------------------------
-- auxiliary parsers
space :: Parser RespDerivs Char
space   = char ' '

spaces, spaces1 :: Parser RespDerivs String
spaces  = many space
spaces1 = many1 space

crlf :: String
crlf = "\r\n"

crlfP :: Derivs d => Parser d String
crlfP = string crlf

lookups :: Eq a => a -> [(a, b)] -> [b]
lookups _ [] = []
lookups k ((k', v):tl) | k == k'   = v : lookups k tl
                       | otherwise = lookups k tl

---- Either handling
catRights :: [Either a b] -> [b]
catRights []           = []
catRights (Right r:tl) = r : catRights tl
catRights (_:tl)       = catRights tl

catLefts :: [Either a b] -> [a]
catLefts []           = []
catLefts (Left r:tl) = r : catLefts tl
catLefts (_:tl)       = catLefts tl
