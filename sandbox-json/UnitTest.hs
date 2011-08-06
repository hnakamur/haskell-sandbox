import System.Environment (getProgName)
import Test.HUnit ((~:), Test, assertBool, assertEqual, assertFailure,
                   runTestTT, test)
import SandBox.Data.JSON (JSONValue(..), parseJSON)

main = do
    getProgName >>= print
    runTestTT tests

tests = test
    [ testParse "null" JNull
    , testParse "false" (JBool False)
    , testParse "true" (JBool True)
    , testParse "\"\"" (JString "")
    , testParse "{}" (JObject [])
    , testParse "[]" (JArray [])
    , numbers
    , errors
    ]

numbers = test
    [ testParse "0" (JNumber 0.0)
    , testParse "1" (JNumber 1.0)
    , testParse "2" (JNumber 2.0)
    , testParse "3" (JNumber 3.0)
    , testParse "4" (JNumber 4.0)
    , testParse "5" (JNumber 5.0)
    , testParse "6" (JNumber 6.0)
    , testParse "7" (JNumber 7.0)
    , testParse "8" (JNumber 8.0)
    , testParse "9" (JNumber 9.0)
    , testParse "10" (JNumber 10.0)
    , testParse "-1" (JNumber (-1.0))
    , testParse "-10" (JNumber (-10.0))
    , testParse "1.2" (JNumber 1.2)
    , testParse "1.23" (JNumber 1.23)
    , testParse "-1.2" (JNumber (-1.2))
    , testParse "-1.23" (JNumber (-1.23))
    , testParse "13e9" (JNumber 13e9)
    , testParse "13E9" (JNumber 13e9)
    , testParse "13e+9" (JNumber 13e9)
    , testParse "1e12" (JNumber 1e12)
    , testParse "13e-9" (JNumber 13e-9)
    , testParse "1e-12" (JNumber 1e-12)
    , testParse "1.23e45" (JNumber 1.23e45)
    , testParse "1.23e-45" (JNumber 1.23e-45)
    , testParse "-1.23e-45" (JNumber (-1.23e-45))
    ]    

errors = test
    [ testParseError "failure"
    , testParseError "nil"
    , testParseError "+2"
    , testParseError ".2"
    , testParseError "2."
    , testParseError "- 1.2"
    ]

testParse :: String -> JSONValue -> Test
testParse input expected =
    "parse " ++ input ~:
    case (parseJSON input) of
      (Right actual) -> assertEqual "" expected actual
      (Left err) -> assertFailure $ show err

testParseError :: String -> Test
testParseError input =
    "parse " ++ input ~:
    case (parseJSON input) of
      (Right actual) -> assertFailure "parse error expected"
      (Left err) -> assertBool "" True
