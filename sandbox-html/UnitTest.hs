{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Identity (Identity)
import System.Environment (getProgName)
import Test.HUnit ((~:), Test, assertBool, assertEqual, assertFailure,
                   runTestTT, test)
import Text.Parsec (Stream, ParsecT, parse)

import SandBox.Text.HTML

main = do
    getProgName >>= print
    runTestTT tests

tests = test
    [ testParse html
          "<!DOCTYPE html><html>"
          (HTML (DOCTYPE "html" Nothing Nothing Nothing)
                (StartTag "html" [] False))
    , testParse html
          "  <!-- comment1 --> <!-- comment2 --> <!DOCTYPE html><!-- This is coment --> <html>"
          (HTML (DOCTYPE "html" Nothing Nothing Nothing)
                (StartTag "html" [] False))
    , testParse htmlTokens
          "<!DOCTYPE html><html>Hello, HTML.</html>"
          [ DOCTYPEToken (DOCTYPE "html" Nothing Nothing Nothing)
          , TagToken (StartTag "html" [] False)
          , TextToken "Hello, HTML."
          , TagToken (EndTag "html")
          ]
    , testParse startTag
          "<html>"
          (StartTag "html" [] False)
    , testParseError startTag
          "< html>"
    , testParse startTag
          "<html >"
          (StartTag "html" [] False)
    , testParse startTag
          "<html lang=\"en\">"
          (StartTag "html" [Attribute "lang" (Just "en")] False)
    , testParse startTag
          "<html lang=\"en\" >"
          (StartTag "html" [Attribute "lang" (Just "en")] False)
    , testParse startTag
          "<br>"
          (StartTag "br" [] False)
    , testParse startTag
          "<br/>"
          (StartTag "br" [] True)
    , testParse startTag
          "<br />"
          (StartTag "br" [] True)
    , testParse startTag
          "<img src=\"a.jpg\">"
          (StartTag "img" [Attribute "src" (Just "a.jpg")] False)
    , testParse startTag
          "<img src=\"a.jpg\"/>"
          (StartTag "img" [Attribute "src" (Just "a.jpg")] True)
    , testParse startTag
          "<img src=\"a.jpg\" />"
          (StartTag "img" [Attribute "src" (Just "a.jpg")] True)
    , testParse startTag
          "<a href=\"/example/\">"
          (StartTag "a" [Attribute "href" (Just "/example/")] False)
    , testParse startTag
          "<a href=/example/>"
          (StartTag "a" [Attribute "href" (Just "/example/")] False)
    , testParse startTag
          "<a href=/example />"
          (StartTag "a" [Attribute "href" (Just "/example")] True)
    , testParse startTag
          "<a href=\"/example\"/>"
          (StartTag "a" [Attribute "href" (Just "/example")] True)
    , testParse comment "<!-- -->" (Comment " ")
    , testParse comment "<!---->" (Comment "")
    , testParse comment
          "<!-- This is comment. -->"
          (Comment " This is comment. ")
    , testParseError comment "<!--- -->"
    , testParseError comment "<!--> -->"
    , testParseError comment "<!-- --->"
    , testParseError comment "<!-- -- -->"
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
