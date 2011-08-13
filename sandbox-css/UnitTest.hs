{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Identity (Identity)
import System.Environment (getProgName)
import Test.HUnit ((~:), Test, assertBool, assertEqual, assertFailure,
                   runTestTT, test)
import Text.Parsec (Stream, ParsecT, parse, char)

import SandBox.Text.CSS hiding (ident, num)
import SandBox.Text.CSS.Parser (ident, num)
import SandBox.Text.CSS.Parsec.Combinator

main = do
    getProgName >>= print
    runTestTT tests

tests :: Test
tests = test
    [ testParse cssTokens "" []
    , testParse (countMax 4 (char 'c')) "" ""
    , testParse (countMax 4 (char 'c')) "c" "c"
    , testParse (countMax 4 (char 'c')) "cc" "cc"
    , testParse (countMax 4 (char 'c')) "ccc" "ccc"
    , testParse (countMax 4 (char 'c')) "cccd" "ccc"
    , testParse (countMax 4 (char 'c')) "cccc" "cccc"
    , testParse (countMax 4 (char 'c')) "ccccc" "cccc"
    , testParseError (countMin 4 (char 'c')) ""
    , testParseError (countMin 4 (char 'c')) "c"
    , testParse (countMin 4 (char 'c')) "cccc" "cccc"
    , testParse (countMin 4 (char 'c')) "ccccc" "ccccc"
    , testParseError (countRange 2 4 (char 'c')) "c"
    , testParse (countRange 2 4 (char 'c')) "cc" "cc"
    , testParse (countRange 2 4 (char 'c')) "ccc" "ccc"
    , testParse (countRange 2 4 (char 'c')) "cccc" "cccc"
    , testParse (countRange 2 4 (char 'c')) "ccccc" "cccc"
    , testParse unicode "\\beaf" '\xbeaf'
    , testParse unicode "\\10beaf " '\x10beaf'
    , testParse ident "font-family" "font-family"
    , testParse ident "-webkit-animation" "-webkit-animation"
    , testParse num "123" "123"
    , testParse num "12.34" "12.34"
    , testParse num ".34" ".34"
    ]

testParse :: (Show a, Eq a) =>
             ParsecT String () Identity a -> String -> a -> Test
testParse parser input expected =
    "parse " ++ input ~:
    case (parse parser "" input) of
      (Right actual) -> assertEqual "" expected actual
      (Left err) -> assertFailure $ show err

testParseError :: (Show a, Eq a) =>
                  ParsecT String () Identity a -> String -> Test
testParseError parser input =
    "parse " ++ input ~:
    case (parse parser "" input) of
      (Right actual) -> assertFailure "parse error expected"
      (Left err) -> assertBool "" True
