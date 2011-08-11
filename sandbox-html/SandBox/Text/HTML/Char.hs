module SandBox.Text.HTML.Char
  ( isAllowedRefChar
  , isAttrNameChar
  , isDoubleQuotedAttrValueChar
  , isSingleQuotedAttrValueChar
  , isSpaceChar
  , isTextChar
  , isUnicodeChar
  , isUnquotedAttrValueChar
  ) where

import Data.Char (
    GeneralCategory (NotAssigned, Surrogate), generalCategory, isControl)


isAttrNameChar :: Char -> Bool
isAttrNameChar '\0' = False
isAttrNameChar '"' = False
isAttrNameChar '\'' = False
isAttrNameChar '>' = False
isAttrNameChar '/' = False
isAttrNameChar '=' = False
isAttrNameChar c = not $
    isSpaceChar c || isControl c || isUndefinedUnicodeChar c

isUnquotedAttrValueChar :: Char -> Bool
isUnquotedAttrValueChar '"' = False
isUnquotedAttrValueChar '\'' = False
isUnquotedAttrValueChar '=' = False
isUnquotedAttrValueChar '<' = False
isUnquotedAttrValueChar '>' = False
isUnquotedAttrValueChar '`' = False
isUnquotedAttrValueChar c = not (isSpaceChar c) && isTextChar c

isSingleQuotedAttrValueChar :: Char -> Bool
isSingleQuotedAttrValueChar '\'' = False
isSingleQuotedAttrValueChar c = isTextChar c

isDoubleQuotedAttrValueChar :: Char -> Bool
isDoubleQuotedAttrValueChar '"' = False
isDoubleQuotedAttrValueChar c = isTextChar c

isTextChar :: Char -> Bool
isTextChar '\0' = False
isTextChar c = not $ isControl c || isUndefinedUnicodeChar c

isSpaceChar :: Char -> Bool
isSpaceChar ' ' = True
isSpaceChar '\t' = True
isSpaceChar '\n' = True
isSpaceChar '\f' = True
isSpaceChar '\r' = True
isSpaceChar _ = False

isAllowedRefChar :: Char -> Bool
isAllowedRefChar '\0' = False
isAllowedRefChar '\x000D' = False
isAllowedRefChar c = not $ isControl c || isUndefinedUnicodeChar c

isUndefinedUnicodeChar :: Char -> Bool
isUndefinedUnicodeChar c = generalCategory c == NotAssigned

isUnicodeChar :: Char -> Bool
isUnicodeChar c = generalCategory c /= Surrogate
