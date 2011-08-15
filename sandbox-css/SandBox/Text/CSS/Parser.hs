{-# LANGUAGE FlexibleContexts #-}

module SandBox.Text.CSS.Parser
    ( 
stylesheet
, selector
, declaration
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
, color
, lexeme
, symbol
, braces
, declarationBlock
, semi
, attrSel
, simpleSel
    ) where

import Control.Monad (liftM)
import Data.Char (chr)
import Data.Functor.Identity (Identity)
import Numeric (readHex)
import Text.Parsec hiding (string, space, spaces)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec as S (string)
import qualified Text.Parsec.Token as P
import SandBox.Text.CSS.Char
import SandBox.Text.CSS.Parsec.Combinator
import SandBox.Text.CSS.Types
import Prelude hiding (any)

stylesheet :: Stream s m Char => ParsecT s u m [Statement]
stylesheet = do
    spaces
    many statement

statement :: Stream s m Char => ParsecT s u m Statement
statement = atRule <|> ruleSet

atRule :: Stream s m Char => ParsecT s u m Statement
atRule = do
    k <- atKeyword
    spaces
    return (AtRule k [] Nothing)

ruleSet :: Stream s m Char => ParsecT s u m Statement
ruleSet = do
    s <- optionMaybe selector
    b <- declarationBlock
    return (RuleSet s b)

selector :: Stream s m Char => ParsecT s u m Selector
selector =
    simpleSel >>= \s1 -> (
      (try (spaces1 >> selector) >>= \s2 ->
       return (DescendSel s1 s2)
      )
      <|>
      (try (spaces >> char '>') >> spaces >> selector >>= \s2 ->
       return (ChildSel s1 s2)
      )
      <|>
      (try (spaces >> char '+') >> spaces >> selector >>= \s2 ->
       return (AdjSel s1 s2)
      )
      <|>
      return s1
    )

simpleSel :: Stream s m Char => ParsecT s u m Selector
simpleSel =
    (typeSel >>= \t ->
     many (try (spaces >> subSel)) >>= \s ->
     return (SimpleSel (TypeSel t s))
    )
    <|>
    (univSel >>
     many (try (spaces >> subSel)) >>= \s ->
     return (SimpleSel (UnivSel s))
    )
    <|>
    (many1 (try (spaces >> subSel)) >>= \s ->
     return (SimpleSel (UnivSel s))
    )

{- NOTE: typeSel is not lexeme parser. -}
typeSel :: Stream s m Char => ParsecT s u m String
typeSel = name

{- NOTE: univSel is not lexeme parser. -}
univSel :: Stream s m Char => ParsecT s u m String
univSel = S.string "*"

subSel :: Stream s m Char => ParsecT s u m SubSel
subSel = 
    (attrSel >>= \s -> return (AttrSel s))
    <|>
    classSel
    <|>
    idSel


attrSel :: Stream s m Char => ParsecT s u m AttrSel
attrSel = between (char '[' >> spaces) (char ']') (
            name >>= \n ->
            spaces >> (
              (try (symbol "=") >> attrVal >>= \v -> return (AttrEq n v))
              <|>
              (try (symbol "~=") >> attrVal >>= \v -> return (AttrContains n v))
              <|>
              (try (symbol "|=") >> attrVal >>= \v -> return (AttrBegins n v))
              <|>
              return (AttrExists n)
            )
          )

attrVal :: Stream s m Char => ParsecT s u m String
attrVal =   identifier
        <|> stringLit

{- Note: No space is allow after '.'. -}
classSel :: Stream s m Char => ParsecT s u m SubSel
classSel = do
    char '.'
    n <- name
    return (ClassSel n)

{- Note: No space is allow after '#'. -}
idSel :: Stream s m Char => ParsecT s u m SubSel
idSel = do
    char '#'
    i <- ident
    return (IdSel i)

declarationBlock :: Stream s m Char => ParsecT s u m [Declaration]
declarationBlock = braces (sepBy (declaration <?> "declaration") semi)

braces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braces = between (symbol "{") (symbol "}")

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme p = do { x <- p; spaces; return x }

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol name = lexeme (S.string name)

{-hash :: Stream s m Char => ParsecT s u m String
hash = symbol "#"-}

dot :: Stream s m Char => ParsecT s u m String
dot = symbol "."

semi :: Stream s m Char => ParsecT s u m String
semi = symbol ";"

comma :: Stream s m Char => ParsecT s u m String
comma = symbol ","

colon :: Stream s m Char => ParsecT s u m String
colon = symbol ":"

{-css21Style :: P.LanguageDef st
css21Style = emptyDef
               { P.commentStart = "/*"
               , P.commentEnd = "*/"
               , P.nestedComments = False
               , P.caseSensitive = False
               }

css21Style :: Stream s m Char => P.GenLanguageDef s u m
css21Style = P.LanguageDef
               { P.commentStart = "/*"
               , P.commentEnd = "*/"
               , P.commentLine = ""
               , P.nestedComments = False
               , P.identStart = asciiAlpha
               , P.identLetter = alphaNum
               , P.opStart = P.opLetter css21Style
               , P.opLetter = oneOf "+>"
               , P.reservedOpNames = []
               , P.reservedNames = []
               , P.caseSensitive = False
               }

lexer :: P.TokenParser ()
lexer :: Stream s m Char => ParsecT s u m String
lexer = P.makeTokenParser css21Style
lexer = P.TokenParser

braces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braces :: ParsecT String () Identity a -> ParsecT String () Identity a
braces = P.braces lexer-}
         
color :: Stream s m Char => ParsecT s u m Color
color = hashColor <|> basicNamedColor

hashColor :: Stream s m Char => ParsecT s u m Color
hashColor = do
    char '#'
    try sixHexDigitColor <|> threeHexDigitColor

sixHexDigitColor :: Stream s m Char => ParsecT s u m Color
sixHexDigitColor = do
    r <- count 2 hexDigit
    g <- count 2 hexDigit
    b <- count 2 hexDigit
    return $ RGBColor (hexToI r) (hexToI g) (hexToI b)

threeHexDigitColor :: Stream s m Char => ParsecT s u m Color
threeHexDigitColor = do
    r <- hexDigit
    g <- hexDigit
    b <- hexDigit
    return $ RGBColor (hexToI (replicate 2 r))
                      (hexToI (replicate 2 g))
                      (hexToI (replicate 2 b))

basicNamedColor :: Stream s m Char => ParsecT s u m Color
basicNamedColor = do
    n <- name
    case (basicNameToColor n) of
      Just c -> return c
      Nothing -> fail "invalid color name"

asciiAlpha :: Stream s m Char => ParsecT s u m Char
asciiAlpha = satisfy isAsciiAlpha

declaration :: Stream s m Char => ParsecT s u m Declaration
declaration = do
    n <- identifier
    colon
    v <- value
    return (Declaration n v)

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
    a <- choice [ try identifier >>= \i -> return (Ident i)
                , try stringLit >>= \s -> return (CSSString s)
                , try percentage
                , try dimension
                , try number
                , try uri
                , try hash
                ]
    spaces
    return a


identifier :: Stream s m Char => ParsecT s u m String
identifier = lexeme ident

stringLit :: Stream s m Char => ParsecT s u m String
stringLit = lexeme string

atKeyword :: Stream s m Char => ParsecT s u m AtKeyword
atKeyword = do
    char '@'
    i <- ident
    return (AtKeyword i)

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
    between (S.string "url(" >> spaces) (spaces >> S.string ")")
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
    h <- option "" (S.string "-")
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
crlf = S.string "\r\n"

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
doubleQuote = S.string "\""

singleQuote :: Stream s m Char => ParsecT s u m String
singleQuote = S.string "'"

badStringEnd :: Stream s m Char => ParsecT s u m String
badStringEnd = S.string "\\?"

string2 :: Stream s m Char => ParsecT s u m String
string2 = stringSub singleQuote (stringContent '\'') singleQuote

badstring2 :: Stream s m Char => ParsecT s u m String
badstring2 = stringSub singleQuote (stringContent '\'') (try badStringEnd)

nl :: Stream s m Char => ParsecT s u m String
nl =   S.string "\n"
   <|> (char '\r' >>= \cr ->
         (try (char '\n') >>= \lf -> return [cr, lf])
         <|> return [cr]
       )
   <|> S.string "\f"

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
