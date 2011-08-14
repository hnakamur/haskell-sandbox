{-# LANGUAGE FlexibleContexts #-}

module SandBox.Text.CSS.Parser
    ( 
declaration
, property
, value
, block
, atKeyword
, any
, uri
, ident
, name
, num
, badstring
, string
, unicode
, crlf
, nl
, optional
, simpleEscape
, isSimpleEscapeChar
    ) where

import Control.Monad (liftM)
import Data.Char (chr)
import Numeric (readHex)
import Text.Parsec hiding (string, space, spaces)
import qualified Text.Parsec as P (string)
import SandBox.Text.CSS.Char
import SandBox.Text.CSS.Parsec.Combinator
import SandBox.Text.CSS.Types
import Prelude hiding (any)

declaration :: Stream s m Char => ParsecT s u m Declaration
declaration = do
    p <- property
    spaces
    char ':'
    spaces
    v <- value
    return (Declaration p v)

property :: Stream s m Char => ParsecT s u m Property
property = do
    i <- ident
    return (Property i)

value :: Stream s m Char => ParsecT s u m Value
value = do
    xs <- many1 valueElem
    return (Value xs)

valueElem :: Stream s m Char => ParsecT s u m ValueElem
valueElem =
    choice [ do{ a <- try any; return (VEAny a) }
           , do{ b <- try block; return (VEBlock b) }
           , do{ k <- atKeyword; return (VEAtKeyword k) }
           ]

block :: Stream s m Char => ParsecT s u m Block
block = do
    xs <- between (char '{' >> spaces) (char '}' >> spaces)
                  (many blockElem)
    return (Block xs)

blockElem :: Stream s m Char => ParsecT s u m BlockElem
blockElem = do
    e <- choice [ do{ a <- try any; return (BEAny a) }
                , do{ b <- try block; return (BEBlock b) }
                , do{ k <- atKeyword; return (BEAtKeyword k) }
                ]
    spaces
    return e

any :: Stream s m Char => ParsecT s u m Any
any = do
    a <- choice [ try identifier
                , try stringLit
                , try percentage
                , try dimension
                , try number
                , try uri
                , try hash
                ]
    return a


{-property :: Stream s m Char => ParsecT s u m TokenData
property = identifier-}

identifier :: Stream s m Char => ParsecT s u m Any
identifier = do
    i <- ident
    return (Ident i)

atKeyword :: Stream s m Char => ParsecT s u m AtKeyword
atKeyword = do
    char '@'
    i <- ident
    return (AtKeyword i)

stringLit :: Stream s m Char => ParsecT s u m Any
stringLit = do
    s <- string
    return (CSSString s)

hash :: Stream s m Char => ParsecT s u m Any
hash = do
    char '#'
    n <- name
    return (Hash n)

number :: Stream s m Char => ParsecT s u m Any
number = do
    n <- num
    return (Number n)

percentage :: Stream s m Char => ParsecT s u m Any
percentage = do
    n <- num
    char '%'
    return (Percentage n)

dimension :: Stream s m Char => ParsecT s u m Any
dimension = do
    n <- num
    i <- ident
    return (Dimension n i)

uri :: Stream s m Char => ParsecT s u m Any
uri = do
    between (P.string "url(" >> spaces) (spaces >> P.string ")")
            (uriContent >>= \c -> return (URI c))


uriContent :: Stream s m Char => ParsecT s u m String
uriContent = try string
             <|> (many $ choice
                          [ try (satisfy isUnquotedURIContentChar)
                          , try nonascii
                          , escape
                          ]
                 )

ident :: Stream s m Char => ParsecT s u m String
ident = do
    h <- option "" (P.string "-")
    s <- nmstart
    cs <- many nmchar
    return (h ++ (s:cs))

name :: Stream s m Char => ParsecT s u m String
name = many1 nmchar

nmstart :: Stream s m Char => ParsecT s u m Char
nmstart = satisfy isNmStartChar <|> escape

nonascii :: Stream s m Char => ParsecT s u m Char
nonascii = satisfy isNonAsciiChar

nmchar :: Stream s m Char => ParsecT s u m Char
nmchar = satisfy isNmChar <|> escape

unicode :: Stream s m Char => ParsecT s u m Char
unicode = do
    char '\\'
    ds <- countRange 1 6 hexDigit
    optional (crlf <|> count 1 space)
    return $ chr (hexToI ds)

escape :: Stream s m Char => ParsecT s u m Char
escape = try unicode <|> simpleEscape

simpleEscape :: Stream s m Char => ParsecT s u m Char
simpleEscape = char '\\' >> satisfy isSimpleEscapeChar

crlf :: Stream s m Char => ParsecT s u m String
crlf = P.string "\r\n"

hexToI :: Num a => String -> a
hexToI ds = let ((n,_):_) = readHex ds
            in n

string :: Stream s m Char => ParsecT s u m String
string = string1 <|> string2

badstring :: Stream s m Char => ParsecT s u m String
badstring = badstring1 <|> badstring2

string1 :: Stream s m Char => ParsecT s u m String
string1 = stringSub doubleQuote (stringContent '"') doubleQuote

badstring1 :: Stream s m Char => ParsecT s u m String
badstring1 = stringSub doubleQuote (stringContent '"') (try badStringEnd)

stringContent :: Stream s m Char => Char -> ParsecT s u m String
stringContent quoteChar = choice [ sequence [try escape]
                     , try (char '\\' >> nl)
                     , sequence [satisfy (isPlainStrChar quoteChar)]
                     ]

stringSub :: Stream s m Char => ParsecT s u m String ->
                                ParsecT s u m String ->
                                ParsecT s u m String ->
                                ParsecT s u m String
stringSub start unit end = do
  start
  t <- manyTill unit end
  return (concat t)

doubleQuote :: Stream s m Char => ParsecT s u m String
doubleQuote = P.string "\""

singleQuote :: Stream s m Char => ParsecT s u m String
singleQuote = P.string "'"

badStringEnd :: Stream s m Char => ParsecT s u m String
badStringEnd = P.string "\\?"

string2 :: Stream s m Char => ParsecT s u m String
string2 = stringSub singleQuote (stringContent '\'') singleQuote

badstring2 :: Stream s m Char => ParsecT s u m String
badstring2 = stringSub singleQuote (stringContent '\'') (try badStringEnd)

nl :: Stream s m Char => ParsecT s u m String
nl =   P.string "\n"
   <|> (char '\r' >>= \cr ->
         (try (char '\n') >>= \lf -> return [cr, lf])
         <|> return [cr]
       )
   <|> P.string "\f"

num :: Stream s m Char => ParsecT s u m String
num = try (
        do{ int <- many digit
          ; dot <- char '.'
          ; frac <- many1 digit
          ; return (int ++ dot:frac)
          }
      )
      <|> many1 digit

spaces1 :: Stream s m Char => ParsecT s u m String
spaces1 = many1 space

spaces :: Stream s m Char => ParsecT s u m String
spaces = many space

space :: Stream s m Char => ParsecT s u m Char
space = satisfy isSpaceChar
