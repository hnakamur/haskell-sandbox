module SandBox.Data.JSON (
    module SandBox.Data.JSON.PrettyPrint,
    module SandBox.Data.JSON.Types,
    obj1, obj2
  ) where

import SandBox.Data.JSON.PrettyPrint
import SandBox.Data.JSON.Types

value1 = JArray [JString "foo", JString "bar", JString "baz", JString "\x3042\x3044\x0346"]
value2 = JArray $ map JString $ words "The default interface to the pretty-printing library. Provides a collection of pretty printer combinators."
value3 = JArray [JString "str1", JNumber 1, JNumber 2.3, JBool True,
                 JBool False, JNull]

obj1 = JObject [("key1", value1), ("key2", value2), ("key3", value3)]

obj2 = JObject [("key1", JString "value1"), ("key2", JString "value2"),
                ("key3", JString "value3")]
