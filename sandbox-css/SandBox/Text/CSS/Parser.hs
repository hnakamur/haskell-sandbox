{-# LANGUAGE FlexibleContexts #-}

module SandBox.Text.CSS.Parser
    ( cssTokens
, ident
, name
, num
, badstring
, string
, unicode
, crlf
, nl
, optional
, isSimpleEscapeChar
    ) where

import Control.Monad (liftM)
import Data.Char (chr)
import Numeric (readHex)
import Text.Parsec hiding (string, space, spaces)
import qualified Text.Parsec as P (string)
import SandBox.Text.CSS.Char
import SandBox.Text.CSS.Parsec.Combinator
import qualified SandBox.Text.CSS.Types as T

cssTokens :: Stream s m Char => ParsecT s u m [T.CSSToken]
cssTokens =
    do{ t <- try cssToken
      ; spaces
      ; do{ ts <- try cssTokens
          ; return (t:ts)
          }
        <|> return [t]
      }
     <|> return []

cssToken :: Stream s m Char => ParsecT s u m T.CSSToken
cssToken =
        (try ident >>= \i -> return (T.Ident i))

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

nmchar :: Stream s m Char => ParsecT s u m Char
nmchar = satisfy isNmChar <|> escape

unicode :: Stream s m Char => ParsecT s u m Char
unicode = do
    char '\\'
    ds <- countRange 1 6 hexDigit
    optional (crlf <|> count 1 space)
    return $ chr (hexToI ds)

escape :: Stream s m Char => ParsecT s u m Char
escape = unicode <|> simpleEscape

simpleEscape :: Stream s m Char => ParsecT s u m Char
simpleEscape = char '\\' >> satisfy (not . isSimpleEscapeChar)

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
string1 = stringSub doubleQuote string1Part doubleQuote

badstring1 :: Stream s m Char => ParsecT s u m String
badstring1 = stringSub doubleQuote string1Part (try badStringEnd)

string1Part :: Stream s m Char => ParsecT s u m String
string1Part = choice [ sequence [satisfy isStr1Char]
                     , try (char '\\' >> nl)
                     , sequence [escape]
                     ]

isStr1Char :: Char -> Bool
isStr1Char '\n' = False
isStr1Char '\r' = False
isStr1Char '\f' = False
isStr1Char '"' = False
isStr1Char _ = True

stringSub :: Stream s m Char => ParsecT s u m String ->
                                ParsecT s u m String ->
                                ParsecT s u m String ->
                                ParsecT s u m String
stringSub start unit end = do
  h <- start
  t <- manyUpTo unit end
  return $ concat (h:t)

doubleQuote :: Stream s m Char => ParsecT s u m String
doubleQuote = P.string "\""

singleQuote :: Stream s m Char => ParsecT s u m String
singleQuote = P.string "'"

badStringEnd :: Stream s m Char => ParsecT s u m String
badStringEnd = P.string "\\?"

string2 :: Stream s m Char => ParsecT s u m String
string2 = stringSub singleQuote string2Part singleQuote

badstring2 :: Stream s m Char => ParsecT s u m String
badstring2 = stringSub singleQuote string2Part (try badStringEnd)

string2Part :: Stream s m Char => ParsecT s u m String
string2Part = choice [ sequence [satisfy isStr2Char]
                     , try (char '\\' >> nl)
                     , sequence [escape]
                     ]

isStr2Char :: Char -> Bool
isStr2Char '\n' = False
isStr2Char '\r' = False
isStr2Char '\f' = False
isStr2Char '\'' = False
isStr2Char _ = True

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
