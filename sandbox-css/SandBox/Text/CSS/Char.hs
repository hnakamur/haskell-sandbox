module SandBox.Text.CSS.Char
    ( isNmStartChar
    , isNmChar
    , isNonAsciiChar
    , isPlainStrChar
    , isSimpleEscapeChar
    , isSpaceChar
    , isUnquotedURIContentChar
    ) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isHexDigit)

isNmStartChar :: Char -> Bool
isNmStartChar c = isAsciiAlpha c || c == '_' || isNonAsciiChar c

isNmChar :: Char -> Bool
isNmChar c =  isAsciiAlpha c || isDigit c || c == '_' || c == '-'
           || isNonAsciiChar c

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiLower c || isAsciiUpper c

isNonAsciiChar :: Char -> Bool
isNonAsciiChar c = c >= '\o240' && c <= '\o4177777'

isSimpleEscapeChar :: Char -> Bool
isSimpleEscapeChar '\n' = False
isSimpleEscapeChar '\r' = False
isSimpleEscapeChar '\f' = False
isSimpleEscapeChar c = not (isHexDigit c)

isPlainStrChar :: Char -> Char -> Bool
isPlainStrChar _ '\n' = False
isPlainStrChar _ '\r' = False
isPlainStrChar _ '\f' = False
isPlainStrChar quote c = c /= quote

isSpaceChar :: Char -> Bool
isSpaceChar ' ' = True
isSpaceChar '\t' = True
isSpaceChar '\r' = True
isSpaceChar '\n' = True
isSpaceChar '\f' = True
isSpaceChar _ = False

isUnquotedURIContentChar :: Char -> Bool
isUnquotedURIContentChar '!' = True
isUnquotedURIContentChar '#' = True
isUnquotedURIContentChar '$' = True
isUnquotedURIContentChar '%' = True
isUnquotedURIContentChar '^' = True
isUnquotedURIContentChar c =  (c >= '*' && c <= '[')
                           || (c >= ']' && c <= '~')
