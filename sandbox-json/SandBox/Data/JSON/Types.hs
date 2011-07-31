module SandBox.Data.JSON.Types (
    JSONValue(..), getString, getNumber, getBool, isNull, getObject, getArray
  ) where

data JSONValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JSONValue)]
            | JArray [JSONValue]
            deriving (Eq, Ord, Show)

getString :: JSONValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getNumber :: JSONValue -> Maybe Double
getNumber (JNumber n) = Just n
getNumber _ = Nothing

getBool :: JSONValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _ = Nothing

isNull :: JSONValue -> Bool
isNull v = v == JNull

getObject :: JSONValue -> Maybe [(String, JSONValue)]
getObject (JObject o) = Just o
getObject _ = Nothing

getArray :: JSONValue -> Maybe [JSONValue]
getArray (JArray a) = Just a
getArray _ = Nothing
