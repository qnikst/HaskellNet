{-# LANGUAGE CPP #-}
-- | Packrat parsing: Simple, Powerful, Lazy, Linear time by Bryan
-- Ford.  This module achieves monadic parsing library similar to
-- Parsec.
module Text.Packrat.Parse where

import Prelude hiding (exp, rem)

import Data.Char
import Data.List

import Text.Packrat.Pos

import Control.Monad
import qualified Control.Applicative as A

import qualified Control.Monad.Fail as Fail

-- Data types

data Message = Expected String
             | Message String

data ParseError = ParseError { errorPos      :: Pos
                             , errorMessages :: [Message] }

data Result d v = Parsed v d ParseError
                | NoParse ParseError

newtype Parser d v = Parser (d -> Result d v)


class Derivs d where
    dvPos   :: d -> Pos
    dvChar  :: d -> Result d Char


-- Basic Combinators

infixl 2 <|>
infixl 1 <?>
infixl 1 <?!>

instance Derivs d => Functor (Parser d) where
    f `fmap` (Parser p1) = Parser $ parse . p1
        where parse (Parsed val rem err) =
                  let val2 = f val
                  in  Parsed val2 rem err
              parse (NoParse err) = NoParse err

instance Derivs d => Applicative (Parser d) where
    pure x = Parser (\dvs -> Parsed x dvs (nullError dvs))
    (<*>) = ap

instance Derivs d => Monad (Parser d) where
    (Parser p1) >>= f = Parser parse
        where parse dvs = first (p1 dvs)
              first (Parsed val rem err) =
                  let Parser p2 = f val
                  in second err (p2 rem)
              first (NoParse err) = NoParse err
              second err1 (Parsed val rem err) =
                  Parsed val rem (joinErrors err1 err)
              second err1 (NoParse err) =
                  NoParse (joinErrors err1 err)
    return = pure

#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
#endif

instance Derivs d => Fail.MonadFail (Parser d) where
    fail msg = Parser (\dvs -> NoParse (msgError (dvPos dvs) msg))

instance Derivs d => A.Alternative (Parser d) where
    empty = Parser $ NoParse . nullError
    (<|>) = (<|>)

instance Derivs d => MonadPlus (Parser d) where
    mzero = A.empty
    mplus = (A.<|>)

(<|>) :: Derivs d => Parser d v -> Parser d v -> Parser d v
(Parser p1) <|> (Parser p2) = Parser parse
    where parse dvs = first dvs (p1 dvs)
          first _ (result@(Parsed {})) = result
          first dvs (NoParse err) = second err (p2 dvs)
          second err1 (Parsed val rem err) =
              Parsed val rem (joinErrors err1 err)
          second err1 (NoParse err) =
              NoParse (joinErrors err1 err)

satisfy :: Derivs d => Parser d v -> (v -> Bool) -> Parser d v
satisfy (Parser p) test = Parser parse
    where parse dvs = check dvs (p dvs)
          check dvs (result@(Parsed val _ _)) =
              if test val
              then result
              else NoParse (nullError dvs)
          check _ none = none

notFollowedBy :: (Derivs d, Show v) => Parser d v -> Parser d ()
notFollowedBy (Parser p) = Parser parse
    where parse dvs = case p dvs of
                        Parsed val _ _ ->
                            NoParse (msgError (dvPos dvs)
                                     ("unexpected " ++ show val))
                        NoParse _ -> Parsed () dvs (nullError dvs)

optional :: Derivs d => Parser d v -> Parser d (Maybe v)
optional p = (do v <- p; return (Just v)) <|> return Nothing

option :: Derivs d => v -> Parser d v -> Parser d v
option v p = p <|> return v

many :: Derivs d => Parser d v -> Parser d [v]
many p = (do { v <- p; vs <- many p; return (v : vs) } )
     <|> return []

many1 :: Derivs d => Parser d v -> Parser d [v]
many1 p = do { v <- p; vs <- many p; return (v : vs) }

count :: Derivs d => Int -> Parser d v -> Parser d [v]
count = replicateM

sepBy1 :: Derivs d => Parser d v -> Parser d vsep -> Parser d [v]
sepBy1 p psep = do v <- p
                   vs <- many (do { psep; p })
                   return (v : vs)

sepBy :: Derivs d => Parser d v -> Parser d vsep -> Parser d [v]
sepBy p psep = sepBy1 p psep <|> return []

endBy :: Derivs d => Parser d v -> Parser d vend -> Parser d [v]
endBy p pend = many (do { v <- p; pend; return v })

endBy1 :: Derivs d => Parser d v -> Parser d vend -> Parser d [v]
endBy1 p pend = many1 (do { v <- p; pend; return v })

sepEndBy1 :: Derivs d => Parser d v -> Parser d vsep -> Parser d [v]
sepEndBy1 p psep = do v <- sepBy1 p psep; optional psep; return v

sepEndBy :: Derivs d => Parser d v -> Parser d vsep -> Parser d [v]
sepEndBy p psep = do v <- sepBy p psep; optional psep; return v

chainl1 :: Derivs d => Parser d v -> Parser d (v->v->v) -> Parser d v
chainl1 p psep = let psuffix z = (do f <- psep
                                     v <- p
                                     psuffix (f z v))
                             <|> return z
                 in do v <- p
                       psuffix v

chainl :: Derivs d => Parser d v -> Parser d (v->v->v) -> v -> Parser d v
chainl p psep z = chainl1 p psep <|> return z

chainr1 :: Derivs d => Parser d v -> Parser d (v->v->v) -> Parser d v
chainr1 p psep = (do v <- p
                     f <- psep
                     w <- chainr1 p psep
                     return (f v w))
                 <|> p

chainr :: Derivs d => Parser d v -> Parser d (v->v->v) -> v -> Parser d v
chainr p psep z = chainr1 p psep <|> return z

choice :: Derivs d => [Parser d v] -> Parser d v
choice [] = error "choice requires non-empty list"
choice [p] = p
choice (p:ps) = p <|> choice ps


manyTill :: Derivs d => Parser d v -> Parser d vend -> Parser d [v]
manyTill p pend = (pend >> return [])
              <|> do tok <- p
                     rest <- manyTill p pend
                     return (tok:rest)

between :: Derivs d => Parser d vs -> Parser d ve -> Parser d v -> Parser d v
between s e main = do s
                      v <- main
                      e
                      return v

-- Error handling
instance Eq Message where
    Expected e1 == Expected e2  = e1 == e2
    Message m1 == Message m2    = m1 == m2
    _ == _                      = False

failAt :: Derivs d => Pos -> String -> Parser d v
failAt pos msg = Parser (const $ NoParse (msgError pos msg))

-- Annotate a parser with a description of the construct to be parsed.
-- The resulting parser yields an "expected" error message
-- if the construct cannot be parsed
-- and if no error information is already available
-- indicating a position farther right in the source code
-- (which would normally be more localized/detailed information).
(<?>) :: Derivs d => Parser d v -> String -> Parser d v
(Parser p) <?> desc = Parser (\dvs -> munge dvs (p dvs))
    where munge dvs (Parsed v rem err) =
              Parsed v rem (fix dvs err)
          munge dvs (NoParse err) =
              NoParse (fix dvs err)
          fix dvs (err@(ParseError ep _)) =
              if ep > dvPos dvs
              then err
              else expError (dvPos dvs) desc

-- Stronger version of the <?> error annotation operator above,
-- which unconditionally overrides any existing error information.
(<?!>) :: Derivs d => Parser d v -> String -> Parser d v
(Parser p) <?!> desc = Parser (\dvs -> munge dvs (p dvs))
    where munge dvs (Parsed v rem err) =
              Parsed v rem (fix dvs err)
          munge dvs (NoParse err) =
              NoParse (fix dvs err)
          fix dvs (ParseError _ _) =
              expError (dvPos dvs) desc

-- Potentially join two sets of ParseErrors,
-- but only if the position didn't change from the first to the second.
-- If it did, just return the "new" (second) set of errors.
joinErrors :: ParseError -> ParseError -> ParseError
joinErrors (e@(ParseError p m)) (e'@(ParseError p' m'))
    | p' > p || null m  = e'
    | p > p' || null m' = e
    | otherwise         = ParseError p (m `union` m')

nullError :: Derivs d => d -> ParseError
nullError dvs = ParseError (dvPos dvs) []

expError :: Pos -> String -> ParseError
expError pos desc = ParseError pos [Expected desc]

msgError :: Pos -> String -> ParseError
msgError pos msg = ParseError pos [Message msg]

eofError :: Derivs d => d -> ParseError
eofError dvs = msgError (dvPos dvs) "end of input"

expected :: Derivs d => String -> Parser d v
expected desc = Parser (\dvs -> NoParse (expError (dvPos dvs) desc))

unexpected :: Derivs d => String -> Parser d v
unexpected str = fail ("unexpected " ++ str)


-- Comparison operators for ParseError just compare relative positions.
instance Eq ParseError where
    ParseError p1 _ == ParseError p2 _  = p1 == p2
    ParseError p1 _ /= ParseError p2 _  = p1 /= p2

instance Ord ParseError where
    ParseError p1 _ < ParseError p2 _   = p1 < p2
    ParseError p1 _ > ParseError p2 _   = p1 > p2
    ParseError p1 _ <= ParseError p2 _  = p1 <= p2
    ParseError p1 _ >= ParseError p2 _  = p1 >= p2
    -- Special behavior: "max" joins two errors
    max = joinErrors
    min _ _ = undefined

instance Show ParseError where
    show (ParseError pos []) =
        show pos ++ ": unknown error"
    show (ParseError pos msgs) = expectmsg expects ++ messages msgs
        where expects = getExpects msgs
              getExpects [] = []
              getExpects (Expected exp : rest) = exp : getExpects rest
              getExpects (Message _ : rest) = getExpects rest
              expectmsg [] = ""
              expectmsg [exp] = show pos ++ ": expecting " ++ exp ++ "\n"
              expectmsg [e1, e2] = show pos ++ ": expecting either "
                                     ++ e1 ++ " or " ++ e2 ++ "\n"
              expectmsg (first : rest) = show pos ++ ": expecting one of: "
                                           ++ first ++ expectlist rest ++ "\n"
              expectlist [] = ""
              expectlist [lst] = ", or " ++ lst
              expectlist (mid : rest) = ", " ++ mid ++ expectlist rest
              messages [] = []
              messages (Expected _ : rest) = messages rest
              messages (Message msg : rest) =
                  show pos ++ ": " ++ msg ++ "\n" ++ messages rest


-- Character-oriented parsers

anyChar :: Derivs d => Parser d Char
anyChar = Parser dvChar

char :: Derivs d => Char -> Parser d Char
char ch = satisfy anyChar (== ch) <?> show ch

oneOf :: Derivs d => [Char] -> Parser d Char
oneOf chs = satisfy anyChar (`elem` chs)
            <?> ("one of the characters " ++ show chs)

noneOf :: Derivs d => [Char] -> Parser d Char
noneOf chs = satisfy anyChar (`notElem` chs)
             <?> ("any character not in " ++ show chs)


charIf :: Derivs d => (Char -> Bool) -> Parser d Char
charIf p = satisfy anyChar p <?> "predicate is not satisfied"

string :: Derivs d => String -> Parser d String
string str = p str <?> show str
    where p [] = return str
          p (ch:chs) = do { char ch; p chs }

stringFrom :: Derivs d => [String] -> Parser d String
stringFrom [] = error "stringFrom requires non-empty list"
stringFrom [str] = string str
stringFrom (str : strs) = string str <|> stringFrom strs

upper :: Derivs d => Parser d Char
upper = satisfy anyChar isUpper <?> "uppercase letter"

lower :: Derivs d => Parser d Char
lower = satisfy anyChar isLower <?> "lowercase letter"

letter :: Derivs d => Parser d Char
letter = satisfy anyChar isAlpha <?> "letter"

alphaNum :: Derivs d => Parser d Char
alphaNum = satisfy anyChar isAlphaNum <?> "letter or digit"

digit :: Derivs d => Parser d Char
digit = satisfy anyChar isDigit <?> "digit"

hexDigit :: Derivs d => Parser d Char
hexDigit = satisfy anyChar isHexDigit <?> "hexadecimal digit (0-9, a-f)"

octDigit :: Derivs d => Parser d Char
octDigit = satisfy anyChar isOctDigit <?> "octal digit (0-7)"

newline :: Derivs d => Parser d Char
newline = char '\n'

tab :: Derivs d => Parser d Char
tab = char '\t'

space :: Derivs d => Parser d Char
space = satisfy anyChar isSpace <?> "whitespace character"

spaces :: Derivs d => Parser d [Char]
spaces = many space

eof :: Derivs d => Parser d ()
eof = notFollowedBy anyChar <?> "end of input"


-- State manipulation

getDerivs :: Derivs d => Parser d d
getDerivs = Parser (\dvs -> Parsed dvs dvs (nullError dvs))

setDerivs :: Derivs d => d -> Parser d ()
setDerivs newdvs = Parser (Parsed () newdvs . nullError)

getPos :: Derivs d => Parser d Pos
getPos = Parser (\dvs -> Parsed (dvPos dvs) dvs (nullError dvs))


-- Special function that converts a Derivs "back" into an ordinary String
-- by extracting the successive dvChar elements.
dvString :: Derivs d => d -> String
dvString d =
    case dvChar d of
      NoParse _ -> []
      Parsed c rem _ -> c : dvString rem
