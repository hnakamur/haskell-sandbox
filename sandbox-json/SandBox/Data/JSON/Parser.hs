{-# LANGUAGE FlexibleContexts #-}

module SandBox.Data.JSON.Parser (
    parseJSON
  ) where

import Control.Monad.Identity (Identity)
import Data.Bits ((.|.), shiftL)
import Data.Char (chr, isControl)
import Numeric (readHex)
import Text.Parsec (
        (<|>), (<?>), Stream, ParsecT, ParseError, between, char, count, digit,
        hexDigit, many, many1, oneOf, option, parse, satisfy, sepBy, skipMany,
        space, spaces, string, unexpected
    )
import Prelude hiding (exp)

import SandBox.Data.JSON.Types (JSONValue(..))

parseJSON :: Stream String Identity Char =>
    String -> Either ParseError JSONValue
parseJSON = parse (spaces >> value) ""


valueWs :: Stream s m Char => ParsecT s u m JSONValue
valueWs = lexeme value

value :: Stream s m Char => ParsecT s u m JSONValue
value =
        (stringLiteral >>= (\s -> return $ JString s))
    <|> (number >>= (\n -> return $ JNumber (read n :: Double)))
    <|> (pairs >>= (\xs -> return $ JObject xs))
    <|> (values >>= (\xs -> return $ JArray xs))
    <|> (string "true" >> return (JBool True))
    <|> (string "false" >> return (JBool False))
    <|> (string "null" >> return JNull)

pairs :: Stream s m Char => ParsecT s u m [(String, JSONValue)]
pairs = braces (sepBy pairWs commaWs)

pairWs :: Stream s m Char => ParsecT s u m (String, JSONValue)
pairWs = lexeme pair

pair :: Stream s m Char => ParsecT s u m (String, JSONValue)
pair = do
    k <- lexeme stringLiteral
    colonWs
    v <- valueWs
    return (k, v)

values :: Stream s m Char => ParsecT s u m [JSONValue]
values = brackets (sepBy valueWs commaWs)

braces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braces = between lBraceWs rBraceWs

brackets :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
brackets = between lBracketWs rBracketWs

lBraceWs :: Stream s m Char => ParsecT s u m Char
lBraceWs = charWs '{'

rBraceWs :: Stream s m Char => ParsecT s u m Char
rBraceWs = charWs '}'

lBracketWs :: Stream s m Char => ParsecT s u m Char
lBracketWs = charWs '['

rBracketWs :: Stream s m Char => ParsecT s u m Char
rBracketWs = charWs ']'

commaWs :: Stream s m Char => ParsecT s u m Char
commaWs = charWs ','

colonWs :: Stream s m Char => ParsecT s u m Char
colonWs = charWs ':'

charWs :: Stream s m Char => Char -> ParsecT s u m Char
charWs = lexeme . char

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme p = do
    x <- p
    skipMany (space <?> "")
    return x

stringLiteral :: Stream s m Char => ParsecT s u m String
stringLiteral = between (char '"') (char '"') stringChars

stringChars :: Stream s m Char => ParsecT s u m String
stringChars = many stringChar

stringChar :: Stream s m Char => ParsecT s u m Char
stringChar = (satisfy $ \c -> (c /= '"') && (c /= '\\') && (not $ isControl c))
         <|> escapedChar

escapedChar :: Stream s m Char => ParsecT s u m Char
escapedChar =
    char '\\' >> (
            (char '"' >> return '"')
        <|> (char '\\' >> return '\\')
        <|> (char '/' >> return '/')
        <|> (char 'b' >> return '\b')
        <|> (char 'f' >> return '\f')
        <|> (char 'n' >> return '\n')
        <|> (char 'r' >> return '\r')
        <|> (char 't' >> return '\t')
        <|> (char 'u' >> fourDigitHex >>= highHex)
    )
  where
    highHex :: Stream s m Char => Int -> ParsecT s u m Char
    highHex x = if (isHighSurrogate x)
                then (string "\\u" >> fourDigitHex >>= lowHex)
                     <?> "low surrogate"
                else (return $ chr x)
      where
        lowHex :: Stream s m Char => Int -> ParsecT s u m Char
        lowHex y = if (isLowSurrogate y)
                   then (return $ chr $ fromSurrogate x y)
                   else (unexpected "input, expecting low surrogate")

fromSurrogate :: Int -> Int -> Int
fromSurrogate x y = (((x - 0xD800) `shiftL` 10) .|. (y - 0xDC00)) + 0x10000

isHighSurrogate :: Int -> Bool
isHighSurrogate x = x >= 0xD800 && x <= 0xDBFF

isLowSurrogate :: Int -> Bool
isLowSurrogate x = x >= 0xDC00 && x <= 0xDFFF

fourDigitHex :: Stream s m Char => ParsecT s u m Int
fourDigitHex = count 4 hexDigit >>= return . hexToI

hexToI :: Num a => String -> a
hexToI ds = let ((n,_):_) = readHex ds
              in n

number :: Stream s m Char => ParsecT s u m String
number = do
    s <- option "" minus
    i <- int
    f <- option "" frac
    e <- option "" exp
    return $ s ++ i ++ f ++ e

minus :: Stream s m Char => ParsecT s u m String
minus = char '-' >> return "-"

int :: Stream s m Char => ParsecT s u m String
int = many1 digit

frac :: Stream s m Char => ParsecT s u m String
frac = do
    dot <- char '.'
    frac <- many1 digit
    return $ dot : frac

exp :: Stream s m Char => ParsecT s u m String
exp = do
    e <- oneOf "eE"
    s <- option "" sign
    ds <- int
    return $ e : s ++ ds

sign :: Stream s m Char => ParsecT s u m String
sign = oneOf "-+" >>= \c -> return [c]
