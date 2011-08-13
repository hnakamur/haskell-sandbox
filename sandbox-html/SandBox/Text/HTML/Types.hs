module SandBox.Text.HTML.Types
  ( HTML(..)
  , Comment(..)
  , DOCTYPE(..)
  , DTDKind(..)
  , Tag(..)
  , Attribute(..)
  , getAttributeName
  , getAttributeMaybeValue
  , getAttributeValue
  ) where

data HTML = HTML
  { docType :: DOCTYPE
  , rootElement :: Tag
  } deriving (Eq, Ord)

instance Show HTML where
  show h = "HTML (" ++ show (docType h) ++ ") ("
           ++ show (rootElement h) ++ ")"

data Comment = Comment String
             deriving (Eq, Ord, Show)

data DOCTYPE = DOCTYPE
  { dtdRootElement :: String
  , dtdKind :: Maybe DTDKind
  , publicId :: Maybe String
  , systemId :: Maybe String
  } deriving (Eq, Ord)

instance Show DOCTYPE where
  show dt = "DOCTYPE " ++ show (dtdRootElement dt) ++ " "
            ++ showMaybeParen (dtdKind dt) ++ " "
            ++ showMaybeParen (publicId dt) ++ " "
            ++ showMaybeParen (systemId dt)

showMaybeParen :: Show a => Maybe a -> String
showMaybeParen v@(Just _) = "(" ++ show v ++ ")"
showMaybeParen v@Nothing = show v


data DTDKind = PUBLIC
             | SYSTEM
             deriving (Eq, Ord, Show)

data Tag = StartTag
           { tagName :: String
           , attributes :: [Attribute]
           , selfClosing :: Bool
           }
         | EndTag
           { tagName :: String }
         deriving (Eq, Ord)

instance Show Tag where
  show (StartTag n attrs c) =
    "StartTag " ++ show n ++ " " ++ show attrs ++ " " ++ show c
  show (EndTag n) =
    "EndTag " ++ show n

data Attribute = Attribute String (Maybe String)
               deriving (Eq, Ord, Show)

getAttributeName :: Attribute -> String
getAttributeName (Attribute n _) = n

getAttributeMaybeValue :: Attribute -> Maybe String
getAttributeMaybeValue (Attribute _ v) = v

getAttributeValue :: Attribute -> String
getAttributeValue (Attribute _ (Just v)) = v
