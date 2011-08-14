module SandBox.Text.CSS.Types
  ( TokenData(..)
  , StyleSheet(..)
  , Statement(..)
  , Selector(..)
  , AtKeyword(..)
  , Any(..)
  , Block(..)
  , BlockElem(..)
  , Declaration(..)
  , Property(..)
  , Value(..)
  , ValueElem(..)
  {-, tdIdent
  , tdString
  , tdNum
  , tdName
  , tdURI
  , tdStart
  , tdEnd-}
  ) where

data StyleSheet = StyleSheet [Statement]
                deriving (Eq, Ord, Show)

data Statement = RuleSet (Maybe Selector) [Declaration]
               | AtRule AtKeyword [Any] (Maybe Block)
               deriving (Eq, Ord, Show)

data Selector = Selector [Any]
              deriving (Eq, Ord, Show)

data AtKeyword = AtKeyword String
               deriving (Eq, Ord, Show)

data Any = Ident String
         | Number String
         | Percentage String
         | Dimension String String
         | CSSString String
         | URI String
         | Hash String
         | UnicodeRange Char Char
         deriving (Eq, Ord, Show)

data Block = Block [BlockElem]
           deriving (Eq, Ord, Show)

data BlockElem = BEAny Any
               | BEBlock Block
               | BEAtKeyword AtKeyword
               deriving (Eq, Ord, Show)

data Declaration = Declaration Property Value
                 deriving (Eq, Ord, Show)

data Property = Property String
              deriving (Eq, Ord, Show)

data Value = Value [ValueElem]
           deriving (Eq, Ord, Show)

data ValueElem = VEAny Any
               | VEBlock Block
               | VEAtKeyword AtKeyword
               deriving (Eq, Ord, Show)

data TokenData =
      Function String
    deriving (Eq, Ord, Show)

{-tdIdent :: TokenData -> String
tdIdent (Ident i) = i
tdIdent (AtKeyword i) = i
tdIdent (Dimension _ i) = i
tdIdent (Function i) = i

tdString :: TokenData -> String
tdString (CSSString s) = s

tdNum :: TokenData -> String
tdNum (Percentage n) = n
tdNum (Number n) = n
tdNum (Dimension n _) = n

tdName :: TokenData -> String
tdName (Hash n) = n

tdURI :: TokenData -> String
tdURI (URI s) = s

tdStart :: TokenData -> Char
tdStart (UnicodeRange s _) = s

tdEnd :: TokenData -> Char
tdEnd (UnicodeRange _ e) = e-}
