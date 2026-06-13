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

import Data.Char
    ( toLower
    , toUpper
    )
import Data.Maybe

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Network.HaskellNet.IMAP.Types
import Network.HaskellNet.IMAP.UTF7

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
Parser pNone = pWithTaggedOrFatal pOtherLine (const ())

pCapability :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, [String])
Parser pCapability = pWithTaggedOrFatal (pCapabilityLine <|> pOtherLine) concat

pList :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, [([Attribute], String, MailboxName)])
Parser pList = pWithTaggedOrFatal (pListLine "LIST" <|> pOtherLine) id

pLsub :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, [([Attribute], String, MailboxName)])
Parser pLsub = pWithTaggedOrFatal (pListLine "LSUB" <|> pOtherLine) id

pStatus :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, [(MailboxStatus, Integer)])
Parser pStatus = pWithTaggedOrFatal (pStatusLine <|> pOtherLine) concat

pExpunge :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, [Integer])
Parser pExpunge =
    pWithTaggedOrFatal ((do string "* "
                            n <- pExpungeLine
                            return $ Right ("EXPUNGE", n))
                        <|> pOtherLine)
                       (lookups "EXPUNGE")

pSearch :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, [UID])
Parser pSearch = pWithTaggedOrFatal (pSearchLine <|> pOtherLine) concat


pSelect :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, MailboxInfo)
Parser pSelect =
    tagged <|> fatal
    where emptyBox = MboxInfo "" 0 0 [] [] False False 0 0
          tagged =
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
          fatal =
              do resp <- pFatalLine
                 return (resp, MboxUpdate Nothing Nothing, emptyBox)

pFetch :: RespDerivs -> Result RespDerivs (ServerResponse, MboxUpdate, [(Integer, [(String, String)])])
Parser pFetch = pWithTaggedOrFatal (pFetchLine <|> pOtherLine) id

pWithTaggedOrFatal :: Parser RespDerivs (Either (String, Integer) b)
                   -> ([b] -> v)
                   -> Parser RespDerivs (ServerResponse, MboxUpdate, v)
pWithTaggedOrFatal lineParser build = tagged <|> fatal
  where
    tagged = do untagged <- many lineParser
                resp <- Parser pDone
                let (mboxUp, values) = mkMboxUpdate untagged
                return (resp, mboxUp, build values)
    fatal = do resp <- pFatalLine
               return (resp, MboxUpdate Nothing Nothing, build [])

pDone :: RespDerivs -> Result RespDerivs ServerResponse
Parser pDone = do tag <- Parser advTag
                  string tag >> space
                  respCode <- pRespCode
                  pRespText respCode

pFatalLine :: Parser RespDerivs ServerResponse
pFatalLine = do string "* "
                stringCI "BYE"
                pRespText (\stat body -> BAD stat ("BYE: " ++ body))

pRespCode :: Parser RespDerivs (Maybe StatusCode -> String -> ServerResponse)
pRespCode = choice [ stringCI "OK" >> return OK
                   , stringCI "NO" >> return NO
                   , stringCI "BAD" >> return BAD
                   , stringCI "PREAUTH" >> return PREAUTH
                   ]

pRespText :: (Maybe StatusCode -> String -> ServerResponse)
          -> Parser RespDerivs ServerResponse
pRespText respCode = do space
                        stat <- optional (do s <- pStatusCode
                                             space >> return s)
                        body <- anyChar `manyTill` crlfP
                        return $ respCode stat body

pStatusCode :: Parser RespDerivs StatusCode
pStatusCode =
    between (char '[') (char ']') $
    choice [ stringCI "ALERT" >> return ALERT
           , do { stringCI "BADCHARSET"
                ; ws <- optional parenWords
                ; return $ BADCHARSET $ fromMaybe [] ws }
           , do { stringCI "CAPABILITY"
                ; space
                ; ws <- (many1 $ noneOf " ]") `sepBy1` space
                ; return $ CAPABILITY_sc ws }
           , stringCI "PARSE" >> return PARSE
           , do { stringCI "PERMANENTFLAGS" >> space >> char '('
                ; fs <- pFlag `sepBy` spaces1
                ; char ')'
                ; return $ PERMANENTFLAGS fs }
           , stringCI "READ-ONLY" >> return READ_ONLY
           , stringCI "READ-WRITE" >> return READ_WRITE
           , stringCI "TRYCREATE" >> return TRYCREATE
           , do { stringCI "APPENDUID" >> space
                ; uidValidity <- pUID
                ; space
                ; uid <- pUID
                ; return $ APPENDUID_sc $ AppendUID uidValidity uid }
           , do { stringCI "COPYUID" >> space
                ; uidValidity <- pUID
                ; space
                ; sourceSet <- pUIDSet
                ; space
                ; destinationSet <- pUIDSet
                ; return $ COPYUID_sc $ CopyUID uidValidity sourceSet destinationSet }
           , do { stringCI "UNSEEN" >> space
                ; num <- many1 digit
                ; return $ UNSEEN_sc $ read num }
           , do { stringCI "UIDNEXT" >> space
                ; num <- many1 digit
                ; return $ UIDNEXT_sc $ read num }
           , do { stringCI "UIDVALIDITY" >> space
                ; num <- many1 digit
                ; return $ UIDVALIDITY_sc $ read num }
           , stringCI "UIDNOTSTICKY" >> return UIDNOTSTICKY
           ]
    where parenWords = between (space >> char '(') (char ')')
                         (many1 (noneOf " )") `sepBy1` space)
          pUID = many1 digit >>= return . read
          pUIDSet = many1 (digit <|> char ':' <|> char ',' <|> char '*')

pFlag :: Parser RespDerivs Flag
pFlag = do char '\\'
           choice [ stringCI "Seen"     >> return Seen
                  , stringCI "Answered" >> return Answered
                  , stringCI "Flagged"  >> return Flagged
                  , stringCI "Deleted"  >> return Deleted
                  , stringCI "Draft"    >> return Draft
                  , stringCI "Recent"   >> return Recent
                  , char '*'          >> return (Keyword "*")
                  , many1 atomChar    >>= return . Keyword . ('\\':) ]
    <|> (many1 atomChar >>= return . Keyword)

pParenFlags :: RespDerivs -> Result RespDerivs [Flag]
Parser pParenFlags = do char '('
                        fs <- pFlag `sepBy` space
                        char ')'
                        return fs

atomChar :: Derivs d => Parser d Char
atomChar = noneOf " (){%*\"\\]\r\n"

pQuotedString :: Parser RespDerivs String
pQuotedString = between (char '"') (char '"') (many quotedChar)
    where quotedChar = (char '\\' >> anyChar) <|> noneOf "\"\\\r\n"

pLiteralString :: Parser RespDerivs String
pLiteralString = do optional (char '~')
                    char '{'
                    num <- many1 digit >>= return . read
                    optional (char '+')
                    char '}' >> crlfP
                    sequence $ replicate num anyChar

pAString :: Parser RespDerivs String
pAString = pQuotedString <|> pLiteralString <|> many1 atomChar

pMailboxName :: Parser RespDerivs MailboxName
pMailboxName = decodeMailboxName <$> pAString

pNumberedLine :: String -> Parser RespDerivs Integer
pNumberedLine str = do num <- many1 digit
                       space
                       stringCI str
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
pCapabilityLine = do string "* "
                     stringCI "CAPABILITY"
                     space
                     ws <- many1 (noneOf " \r") `sepBy` space
                     crlfP
                     return $ Right ws

pListLine :: String
          -> Parser RespDerivs (Either a ([Attribute], String, MailboxName))
pListLine list =
    do string "* " >> stringCI list >> space
       attrs <- parseAttrs
       sep <- parseSep
       mbox <- parseMailbox
       return $ Right (attrs, sep, mbox)
    where parseAttr =
              do char '\\'
                 choice [ stringCI "Noinferiors" >> return Noinferiors
                        , stringCI "Noselect" >> return Noselect
                        , stringCI "Marked" >> return Marked
                        , stringCI "Unmarked" >> return Unmarked
                        , many atomChar >>= return . OtherAttr
                        ]
          parseAttrs = do char '('
                          attrs <- parseAttr `sepBy` space
                          char ')'
                          return attrs
          parseSep = space >> ((string "NIL" >> return "") <|> pQuotedString)
          parseMailbox = do space
                            mbox <- pMailboxName
                            crlfP
                            return mbox

pStatusLine :: Parser RespDerivs (Either a [(MailboxStatus, Integer)])
pStatusLine =
    do string "* "
       stringCI "STATUS"
       space
       _ <- pMailboxName
       space
       stats <- between (char '(') (char ')') (parseStat `sepBy1` space)
       crlfP
       return $ Right stats
    where parseStat =
              do cons <- choice [ stringCI "MESSAGES"    >>= return . read
                                , stringCI "RECENT"      >>= return . read
                                , stringCI "UIDNEXT"     >>= return . read
                                , stringCI "UIDVALIDITY" >>= return . read
                                , stringCI "UNSEEN"      >>= return . read
                                ]
                 space
                 num <- many1 digit >>= return . read
                 return (cons, num)

pSearchLine :: Parser RespDerivs (Either a [UID])
pSearchLine = do string "* "
                 stringCI "SEARCH"
                 nums <- option [] (space >> ((many1 digit) `sepBy` space))
                 crlfP
                 return $ Right $ map read nums

pSelectLine :: Parser RespDerivs (MailboxInfo -> MailboxInfo)
pSelectLine =
    do string "* "
       choice [ pExistsLine >>= \n -> return (\mbox -> mbox { _exists = n })
              , pRecentLine >>= \n -> return (\mbox -> mbox { _recent = n })
              , pFlags  >>= \fs -> return (\mbox -> mbox { _flags = fs })
              , stringCI "OK" >> space >> okResps ]
    where pFlags = do stringCI "FLAGS"
                      space
                      char '('
                      fs <- pFlag `sepBy` space
                      char ')' >> crlfP
                      return fs
          okResps =
              do char '['
                 v <- choice [ do { stringCI "UNSEEN" >> space
                                  ; many1 digit
                                  ; return id }
                             , do { stringCI "PERMANENTFLAGS" >> space >> char '('
                                  ; fs <- pFlag `sepBy` space
                                  ; char ')'
                                  ; return $ \mbox ->
                                      mbox { _isFlagWritable =
                                               Keyword "*" `elem` fs
                                           , _permanentFlags =
                                               filter (/= Keyword "*") fs } }
                             , do { stringCI "UIDNEXT" >> space
                                  ; n <- many1 digit
                                  ; return $ \mbox ->
                                      mbox { _uidNext = read n } }
                             , do { stringCI "UIDVALIDITY" >> space
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
       space >> stringCI "FETCH" >> spaces
       char '('
       pairs <- pPair `manyTill` char ')'
       crlfP
       return $ Right $ (read num, pairs)
    where pPair = do key <- pFetchKey
                     value <- pFetchValue
                     spaces
                     return (key, value)
          pFetchKey = do name <- many1 (noneOf " [)\r\n")
                         section <- option "" pSection
                         space
                         return (map toUpper name ++ section)
          pSection = do char '['
                        ps <- anyChar `manyTill` char ']'
                        origin <- option "" pOrigin
                        return ("[" ++ ps ++ "]" ++ origin)
          pOrigin = do char '<'
                       n <- many1 digit
                       char '>'
                       return ("<" ++ n ++ ">")
          pFetchValue = (do char '('
                            v <- pParen `sepBy` space
                            char ')'
                            return ("("++unwords v++")"))
                    <|> pLiteralString
                    <|> (do v <- pQuotedString
                            return ("\""++v++"\""))
                    <|> pAtomValue
          pParen = (do v <- pQuotedString
                       return ("\""++v++"\""))
               <|> (do char '('
                       v <- pParen `sepBy` space
                       char ')'
                       return ("("++unwords v++")"))
               <|> (do char '\\'
                       v <- many1 atomChar
                       return ('\\':v))
               <|> many1 atomChar
          pAtomValue = do v <- many1 atomChar
                          return $ if map toUpper v == "NIL" then "" else v

----------------------------------------------------------------------
-- auxiliary parsers
space :: Parser RespDerivs Char
space   = char ' '

charCI :: Derivs d => Char -> Parser d Char
charCI c = charIf ((toLower c ==) . toLower) <?> show c

stringCI :: Derivs d => String -> Parser d String
stringCI str = go str <?> show str
  where
    go [] = return str
    go (c:cs) = charCI c >> go cs

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
