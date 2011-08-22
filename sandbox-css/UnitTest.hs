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
        "h1 {padding-top:10px}" 
        [RuleSet
          [(SimpleSel (TypeSel "h1" []))]
          [DPadding (PaddingTopDecl (PVWidth (PWLength (Length 10.0 (Just Px)))))]]
    , testParse
        stylesheet
        "h1 {border-top-color:red}" 
        [RuleSet
          [(SimpleSel (TypeSel "h1" []))]
          [DBorderColor (BorderTopColorDecl (BCVColor (BCColor (BasicNamedColor Red))))]]
    , testParse
        stylesheet
        "h1 {border-top-style:dashed}" 
        [RuleSet
          [(SimpleSel (TypeSel "h1" []))]
          [DBorderStyle (BorderTopStyleDecl (BSVStyle BSDashed))]]
    , testParse
        stylesheet
        "p {border: solid red}" 
        [RuleSet
          [SimpleSel (TypeSel "p" [])]
          [DBorder (BorderDecl (BVBorder [BVEStyle BSSolid,BVEColor (BCColor (BasicNamedColor Red))]))]]
    , testParse (allInAnyOrder [char 'a', char 'b', char 'c']) "abc" "abc"
    , testParse (allInAnyOrder [char 'a', char 'b', char 'c']) "acb" "acb"
    , testParse (allInAnyOrder [char 'a', char 'b', char 'c']) "cab" "cab"
    , testParse (allInAnyOrder [char 'a', char 'b', char 'c']) "cba" "cba"
    , testParse (allInAnyOrder [char 'a', char 'b', char 'c']) "bca" "bca"
    , testParse (allInAnyOrder [char 'a', char 'b', char 'c']) "bac" "bac"
    , testParseError (allInAnyOrder [char 'a', char 'b', char 'c']) ""
    , testParseError (allInAnyOrder [char 'a', char 'b', char 'c']) "a"
    , testParseError (allInAnyOrder [char 'a', char 'b', char 'c']) "ab"
    , testParse (oneOrMoreInAnyOrder [char 'a', char 'b', char 'c']) "a" "a"
    , testParse (oneOrMoreInAnyOrder [char 'a', char 'b', char 'c']) "ab" "ab"
    , testParse (oneOrMoreInAnyOrder [char 'a', char 'b', char 'c']) "ca" "ca"
    , testParse (oneOrMoreInAnyOrder [char 'a', char 'b', char 'c']) "abc" "abc"
    , testParse (oneOrMoreInAnyOrder [char 'a', char 'b', char 'c']) "acb" "acb"
    , testParse (oneOrMoreInAnyOrder [char 'a', char 'b', char 'c']) "cab" "cab"
    , testParse (oneOrMoreInAnyOrder [char 'a', char 'b', char 'c']) "cba" "cba"
    , testParse (oneOrMoreInAnyOrder [char 'a', char 'b', char 'c']) "bca" "bca"
    , testParse (oneOrMoreInAnyOrder [char 'a', char 'b', char 'c']) "bac" "bac"
    , testParseError (oneOrMoreInAnyOrder [char 'a', char 'b', char 'c']) ""
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
                    "http://www.example.com/redball.png"
    , testParse uri "url(http://www.example.com/redball.png)"
                    "http://www.example.com/redball.png"
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
