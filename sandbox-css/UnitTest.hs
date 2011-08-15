{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Identity (Identity)
import System.Environment (getProgName)
import Test.HUnit ((~:), Test, assertBool, assertEqual, assertFailure,
                   runTestTT, test)
import Text.Parsec (Stream, ParsecT, parse, char)

import SandBox.Text.CSS
import SandBox.Text.CSS.Parsec.Combinator

main = do
    getProgName >>= print
    runTestTT tests

tests :: Test
tests = test
    [ testParse
        stylesheet
        "{color:red}" 
        [RuleSet Nothing [Declaration "color" (Value [VEAny (Ident "red")])]]
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
    , testParse uri "url('http://www.example.com/redball.png')"
                    (URI "http://www.example.com/redball.png")
    , testParse uri "url(http://www.example.com/redball.png)"
                    (URI "http://www.example.com/redball.png")
    , testParse declaration
                "color: blue"
                (Declaration "color"
                             (Value [VEAny (Ident "blue")]))
    {-, testParse num "123" "123"
    , testParse num "12.34" "12.34"
    , testParse num ".34" ".34"-}
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
