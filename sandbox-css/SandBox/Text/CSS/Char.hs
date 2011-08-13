module SandBox.Text.CSS.Char
    ( isNmStartChar
    , isNmChar
    , isSimpleEscapeChar
    , isSpaceChar
    ) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isHexDigit)

isNmStartChar :: Char -> Bool
isNmStartChar c = isAsciiAlpha c || c == '_' || isNonAsciiChar c

isNmChar :: Char -> Bool
isNmChar c = isAsciiAlpha c || isDigit c || c == '_' || c == '-'
          || isNonAsciiChar c

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiLower c || isAsciiUpper c

isNonAsciiChar :: Char -> Bool
isNonAsciiChar c = c >= '\o240' && c <= '\o4177777'

isSimpleEscapeChar :: Char -> Bool
isSimpleEscapeChar c = c == '\n' || c == '\r' || c == '\f' || isHexDigit c

isSpaceChar :: Char -> Bool
isSpaceChar ' ' = True
isSpaceChar '\t' = True
isSpaceChar '\r' = True
isSpaceChar '\n' = True
isSpaceChar '\f' = True
isSpaceChar _ = False
