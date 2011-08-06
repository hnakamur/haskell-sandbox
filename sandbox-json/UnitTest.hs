import System.Environment (getProgName)
import Test.HUnit ((~:), Test, assertBool, assertEqual, assertFailure,
                   runTestTT, test)
import SandBox.Data.JSON (JSONValue(..), parseJSON)

main = do
    getProgName >>= print
    runTestTT tests

tests = test
    [ strings
    , numbers
    , objects
    , arrays
    , testParse "true" (JBool True)
    , testParse "null" JNull
    , testParse "false" (JBool False)
    , errors
    ]

strings = test
    [ testParse "\"\"" (JString "")
    , testParse "\"f\"" (JString "f")
    , testParse "\"foo\"" (JString "foo")
    , testParse "\"\\\"foo\\\"\"" (JString "\"foo\"")
    , testParse "\"\\\\\"" (JString "\\")
    , testParse "\"\\/\"" (JString "/")
    , testParse "\"\\b\"" (JString "\b")
    , testParse "\"\\f\"" (JString "\f")
    , testParse "\"\\n\"" (JString "\n")
    , testParse "\"\\r\"" (JString "\r")
    , testParse "\"\\t\"" (JString "\t")
    , testParse "\"\\u1234\"" (JString "\x1234")
    , testParse "\"\\uD950\\uDF21\"" (JString "\x64321")
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

objects = test
    [ testParse "{}" (JObject [])
    , testParse "{\"k1\":true}" (JObject [("k1",JBool True)])
    , testParse "{\"k1\":true,\"k2\":null}"
                (JObject [("k1",JBool True),("k2",JNull)])
    , testParse "{\"k1\": true, \"k2\": null}"
                (JObject [("k1",JBool True),("k2",JNull)])
    , testParse "{ \"k1\" : true , \"k2\" : null }"
                (JObject [("k1",JBool True),("k2",JNull)])
    , testParse "{\"k1\": true, \"k2\": null, \"k3\": [\"a\", false, null]}"
                (JObject [("k1",JBool True),("k2",JNull),
                          ("k3",(JArray [JString "a",JBool False,JNull]))])
    ]
    
arrays = test
    [ testParse "[]" (JArray [])
    , testParse "[true]" (JArray [JBool True])
    , testParse "[\"a\",false,null]" (JArray [JString "a",JBool False,JNull])
    , testParse "[\"a\", false, null]" (JArray [JString "a",JBool False,JNull])
    , testParse "[ \"a\" , false , null ]"
                (JArray [JString "a",JBool False,JNull])
    , testParse "[\"a\", false, null, { \"k1\": true, \"k2\": null } ]"
                (JArray [JString "a",JBool False,JNull,
                         (JObject [("k1",JBool True),("k2",JNull)])])
    ]

errors = test
    [ testParseError "failure"
    , testParseError "nil"
    , testParseError "+2"
    , testParseError ".2"
    , testParseError "2."
    , testParseError "- 1.2"
    , testParseError "\"\\uD950\\u0021\""
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
