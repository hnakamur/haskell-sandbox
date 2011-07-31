module SandBox.Data.JSON.PrettyPrint (
    renderJSONValue
  ) where

import Data.Bits ((.&.), shiftR)
import Data.Char (ord)
import Numeric (showHex)
import Text.PrettyPrint (
    (<>), (<+>),Doc, Style(..), braces, brackets, char, colon, comma, double,
    doubleQuotes, fsep, hcat, renderStyle, style, text
  )

import SandBox.Data.JSON.Types (JSONValue(..))

renderJSONValue :: Int -> JSONValue -> String
renderJSONValue lineLength v =
  renderStyle
    (style {lineLength=lineLength, ribbonsPerLine=1})
    (toDoc v)

toDoc :: JSONValue -> Doc
toDoc (JString s) = strToDoc s
toDoc (JNumber n) = double n
toDoc (JBool True) = text "true"
toDoc (JBool False) = text "false"
toDoc JNull = text "null"
toDoc (JObject o) = braces $ fsep $ appendSep comma $ map kvToDoc o
toDoc (JArray a) = brackets $ fsep $ appendSep comma $ map toDoc a

kvToDoc :: (String, JSONValue) -> Doc
kvToDoc (k, v) = strToDoc k <> colon <+> toDoc v

strToDoc :: String -> Doc
strToDoc = doubleQuotes . hcat . map oneChar

oneChar :: Char -> Doc
oneChar '\b' = text "\\b"
oneChar '\n' = text "\\n"
oneChar '\f' = text "\\f"
oneChar '\r' = text "\\r"
oneChar '\\' = text "\\\\"
oneChar '"' = text "\\\""
oneChar '/' = text "\\/"
oneChar c | mustEscape c = hexEscape c
          | otherwise = char c
  where mustEscape c = c > '\xff' || c < ' ' || c == '\x7f'

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise = astral d
  where d = ord c

astral :: Int -> Doc
astral n = smallHex a <> smallHex b
  where n' = n - 0x10000
        a = (n' `shiftR` 10) .&. 0x3ff + 0xd800
        b = n' .&. 0x3ff + 0xdc00

smallHex :: Int -> Doc
smallHex = text . showHexEscaped

showHexEscaped :: Int -> String
showHexEscaped d = "\\u" ++ zeroPad 4 (showHex d "")

zeroPad :: Int -> String -> String
zeroPad = lPadChar '0'

lPadChar :: Char -> Int -> String -> String
lPadChar c w s | l < w = replicate (w - l) c ++ s
               | otherwise = s
  where l = length s

appendSep :: Doc -> [Doc] -> [Doc]
appendSep s [] = []
appendSep s [d] = [d]
appendSep s (d:ds) = (d <> s) : appendSep s ds
