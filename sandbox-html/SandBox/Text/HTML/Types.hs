module SandBox.Text.HTML.Types (
    HTML(..)
  , getDOCTYPE
  , getRootElement
  , DOCTYPE(..)
  , DTDKind(..)
  , getDTDRootElement
  , getMaybeDTDKind
  , getMaybePublicId
  , getMaybeSystemId
  , Tag(..)
  , Attribute(..)
  , getTagName
  , getAttributes
  , isSelfClosing
  , getAttributeName
  , getAttributeMaybeValue
  , getAttributeValue
  ) where

data HTML = HTML DOCTYPE Tag
            deriving (Eq, Ord, Show)

getDOCTYPE :: HTML -> DOCTYPE
getDOCTYPE (HTML doctype _) = doctype

getRootElement :: HTML -> Tag
getRootElement (HTML _ root) = root

data DOCTYPE = DOCTYPE String (Maybe DTDKind) (Maybe String) (Maybe String)
             deriving (Eq, Ord, Show)

data DTDKind = PUBLIC
             | SYSTEM
             deriving (Eq, Ord, Show)

getDTDRootElement :: DOCTYPE -> String
getDTDRootElement (DOCTYPE root _ _ _) = root

getMaybeDTDKind :: DOCTYPE -> Maybe DTDKind
getMaybeDTDKind (DOCTYPE _ kind _ _) = kind

getMaybePublicId :: DOCTYPE -> Maybe String
getMaybePublicId (DOCTYPE _ _ pubId _) = pubId

getMaybeSystemId :: DOCTYPE -> Maybe String
getMaybeSystemId (DOCTYPE _ _ _ sysId) = sysId



data Tag = StartTag String [Attribute] Bool
         | EndTag String
         deriving (Eq, Ord, Show)

getTagName :: Tag -> String
getTagName (StartTag n _ _) = n
getTagName (EndTag n) = n

getAttributes :: Tag -> [Attribute]
getAttributes (StartTag _ attributes _) = attributes

isSelfClosing :: Tag -> Bool
isSelfClosing (StartTag _ _ b) = b


data Attribute = Attribute String (Maybe String)
               deriving (Eq, Ord, Show)

getAttributeName :: Attribute -> String
getAttributeName (Attribute n _) = n

getAttributeMaybeValue :: Attribute -> Maybe String
getAttributeMaybeValue (Attribute _ v) = v

getAttributeValue :: Attribute -> String
getAttributeValue (Attribute _ (Just v)) = v
