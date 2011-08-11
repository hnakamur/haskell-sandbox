{-# LANGUAGE FlexibleContexts #-}

module SandBox.Text.HTML.Parser
    ( parseHTML
    , parseTag
    , html
    , doctype
    , cdata
    , comment
    , startTag
    , attrsAndClose
    , charRef
    , attrValue
    , attribute
    , decimalCharRef
    , hexadecimalCharRef
    , namedCharRef
    ) where

import Control.Monad.Identity (Identity)
import Data.Char (chr, toLower)
import Numeric (readHex)
import Text.Parsec (
      (<|>), (<?>), Stream, ParsecT, ParseError, alphaNum, anyChar, between,
      char, digit, eof, hexDigit, letter, lookAhead, many, many1, manyTill,
      oneOf, option, optionMaybe, optional, parse, satisfy, sepBy1, skipMany,
      skipMany1, string, try, unexpected
    )

import SandBox.Text.HTML.Char (
      isAllowedRefChar, isAttrNameChar, isSpaceChar, isTextChar
    , isDoubleQuotedAttrValueChar
    , isSingleQuotedAttrValueChar
    , isUnquotedAttrValueChar
    )
import SandBox.Text.HTML.Types (HTML(..), DOCTYPE(..), DTDKind(..), Tag(..), Attribute(..))
import SandBox.Text.HTML.NamedCharRef (charRefNameToMaybeString)

parseHTML :: Stream String Identity Char =>
    String -> Either ParseError HTML
parseHTML = parse html ""

html :: Stream s m Char => ParsecT s u m HTML
html = do
    -- TODO: optional byte order mark
    commentOrSpaces
    d <- doctype
    commentOrSpaces
    root <- startTag
    commentOrSpaces
    return $ HTML d root


doctype :: Stream s m Char => ParsecT s u m DOCTYPE
doctype =
    between 
        (caseInsensitiveString "<!DOCTYPE" >> spaces1 >>
         caseInsensitiveString "html")
        (spaces >> char '>')
        (doctypeSystemSub
         <|> doctypePublicSub
         <|> return (DOCTYPE "html" Nothing Nothing Nothing))

doctypeSystemSub :: Stream s m Char => ParsecT s u m DOCTYPE
doctypeSystemSub = do
    try (spaces1 >> caseInsensitiveString "SYSTEM")
    spaces1
    sysId <- dtdStringLiteral <?> ""
    if sysId == "about:legacy-compat"
        then return (DOCTYPE "html" (Just SYSTEM) Nothing (Just sysId))
        else fail $ "Expected system identifier \"about:legacy-compat\""

doctypePublicSub :: Stream s m Char => ParsecT s u m DOCTYPE
doctypePublicSub = do
    try (spaces1 >> caseInsensitiveString "PUBLIC")
    spaces1
    pubId <- dtdStringLiteral <?> ""
    sysId <- optionMaybe (try (spaces1 >> dtdStringLiteral)) <?> ""
    if (pubId, sysId) `elem` allowedPubIdSysIds
        then return $ DOCTYPE "html" (Just PUBLIC) (Just pubId) sysId
        else fail $ "Expected allowed values for public and system identifiers"

allowedPubIdSysIds :: [(String, Maybe String)]
allowedPubIdSysIds =
    [ ("-//W3C//DTD HTML 4.0//EN",
       (Just "http://www.w3.org/TR/REC-html40/strict.dtd"))
    , ("-//W3C//DTD HTML 4.0//EN",
       Nothing)
    , ("-//W3C//DTD HTML 4.01//EN",
       (Just "http://www.w3.org/TR/html4/strict.dtd"))
    , ("-//W3C//DTD HTML 4.01//EN",
       Nothing)
    , ("-//W3C//DTD XHTML 1.0 Strict//EN",
       (Just "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"))
    , ("-//W3C//DTD XHTML 1.1//EN",
       (Just "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"))
    ]

dtdStringLiteral :: Stream s m Char => ParsecT s u m String
dtdStringLiteral = ((char '\'') >> manyTill anyChar (char '\''))
               <|> ((char '"' ) >> manyTill anyChar (char '"' ))


cdata :: Stream s m Char => ParsecT s u m String
cdata = string "<![CDATA[" >> (manyTill textChar $ try (string "]]>"))

caseInsensitiveString :: Stream s m Char => String -> ParsecT s u m String
caseInsensitiveString = sequence . map caseInsensitiveChar

caseInsensitiveChar :: Stream s m Char => Char -> ParsecT s u m Char
caseInsensitiveChar c = satisfy $ \c' -> toLower c == toLower c'

commentOrSpaces :: Stream s m Char => ParsecT s u m ()
commentOrSpaces = do
    many $ (try comment >> return ()) <|> try spaces1
    return ()

comment :: Stream s m Char => ParsecT s u m String
comment = do
    string "<!--"
    {- TODO: must reject three cases below:
     - 1. The first letter of the text is '-'.
     - 2. The text contains "--" in the middle.
     - 3. The last letter of the text is '-'.
     -}
    manyTill textChar $ try (string "-->")

parseTag :: Stream String Identity Char =>
    String -> Either ParseError Tag
parseTag = parse (spaces >> endTag) ""

{-startTag :: Stream s m Char => ParsecT s u m Tag
startTag = do
    char '<'
    name <- tagName
    attrs <- try $ option [] attributes
    spaces
    ((string "/>" >> return (StartTag name attrs True))
     <|> (char '>' >> return (StartTag name attrs False)))-}

startTag :: Stream s m Char => ParsecT s u m Tag
startTag = do
    char '<'
    name <- tagName
    (attrs, selfClosing) <- attrsAndClose
    return (StartTag name attrs selfClosing)

attrsAndClose :: Stream s m Char => ParsecT s u m ([Attribute], Bool)
attrsAndClose =
    do{ (attr, unquoted) <- try attribute
      ; do{ (attrs, selfClosing) <- try attrsAndClose
          ; return (attr:attrs, selfClosing)
          }
        <|> (try (spaces >> char '>') >> return ([attr], False))
        <|> if unquoted
            then (spaces1 >> string "/>" >> return ([attr], True))
            else (spaces  >> string "/>" >> return ([attr], True))
      }
      <|> (try (spaces >> char '>') >> return ([], False))
      <|> (spaces >> string "/>" >> return ([], True))

attribute :: Stream s m Char => ParsecT s u m (Attribute, Bool)
attribute =
    do{ spaces1
      ; name <- attributeName
      ; do{ try (spaces >> char '=' >> spaces)
          ; do{ value <- unquotedAttrValue
              ; return (Attribute name (Just value), True)
              }
            <|>
            do{ value <- singleQuotedAttrValue <|> doubleQuotedAttrValue
              ; return (Attribute name (Just value), False)
              }
          }
        <|> return (Attribute name Nothing, False)
      }

endTag :: Stream s m Char => ParsecT s u m Tag
endTag = do
    string "</"
    name <- tagName
    spaces
    char '>'
    return (EndTag name)

tagName :: Stream s m Char => ParsecT s u m String
tagName = many1 alphaNum <?> "tag name"

attributeName :: Stream s m Char => ParsecT s u m String
attributeName = many1 attrNameChar

attrNameChar :: Stream s m Char => ParsecT s u m Char
attrNameChar = satisfy isAttrNameChar

attrValue :: Stream s m Char => ParsecT s u m String
attrValue = unquotedAttrValue
        <|> singleQuotedAttrValue
        <|> doubleQuotedAttrValue

unquotedAttrValue :: Stream s m Char => ParsecT s u m String
unquotedAttrValue = do
    strings <- many1 (attrValueChunk isUnquotedAttrValueChar)
    return $ concat strings

singleQuotedAttrValue :: Stream s m Char => ParsecT s u m String
singleQuotedAttrValue = do
    strings <- between (char '\'') (char '\'') $
               many1 (attrValueChunk isSingleQuotedAttrValueChar)
    return $ concat strings

doubleQuotedAttrValue :: Stream s m Char => ParsecT s u m String
doubleQuotedAttrValue = do
    strings <- between (char '"') (char '"') $
               many1 (attrValueChunk isDoubleQuotedAttrValueChar)
    return $ concat strings

attrValueChunk :: Stream s m Char => (Char -> Bool) -> ParsecT s u m String
attrValueChunk pred =
    ((try decimalCharRef <|> try hexadecimalCharRef) >>= checkRefChar)
    {- TODO: Enable to parse non-named-char-ref like "foo&bar" -}
    <|> namedCharRef
    <|> attrValueTextChunk pred

attrValueTextChunk :: Stream s m Char => (Char -> Bool) -> ParsecT s u m String
attrValueTextChunk pred = do
    c <- attrValueLeadChar pred
    cs <- many (attrValueTailChar pred)
    return (c:cs)

attrValueLeadChar :: Stream s m Char => (Char -> Bool) -> ParsecT s u m Char
attrValueLeadChar = satisfy

attrValueTailChar :: Stream s m Char => (Char -> Bool) -> ParsecT s u m Char
attrValueTailChar pred = satisfy $ \c -> (c /= '&' && pred c)

textChar :: Stream s m Char => ParsecT s u m Char
textChar = satisfy isTextChar


charRef :: Stream s m Char => ParsecT s u m String
charRef = ((try decimalCharRef <|> try hexadecimalCharRef) >>= checkRefChar)
        <|> namedCharRef

decimalCharRef :: Stream s m Char => ParsecT s u m Char
decimalCharRef = do
    string "&#"
    ds <- many1 digit
    char ';'
    return $ chr $ read ds

hexadecimalCharRef :: Stream s m Char => ParsecT s u m Char
hexadecimalCharRef = do
    string "&#"
    oneOf "xX"
    ds <- many1 hexDigit
    char ';'
    return $ chr $ hexToI ds

checkRefChar :: Stream s m Char => Char -> ParsecT s u m String
checkRefChar c =
    if (isAllowedRefChar c)
       then return [c]
       else unexpected "reference to a disallowed character"

namedCharRef :: Stream s m Char => ParsecT s u m String
namedCharRef = do
    char '&'
    name <- many1 alphaNum
    char ';'
    case (charRefNameToMaybeString name) of
        (Just value) -> return value
        Nothing -> unexpected "character reference name"

hexToI :: Num a => String -> a
hexToI ds = let ((n,_):_) = readHex ds
            in n

spaces1 :: Stream s m Char => ParsecT s u m ()
spaces1 = skipMany1 space

spaces :: Stream s m Char => ParsecT s u m ()
spaces = skipMany (space <?> "")

space :: Stream s m Char => ParsecT s u m Char
space = satisfy isSpaceChar
