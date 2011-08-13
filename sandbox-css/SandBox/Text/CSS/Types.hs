module SandBox.Text.CSS.Types
  ( CSSToken(..)
  ) where

data CSSToken =
      Ident { ident :: String }
    | AtKeyword { ident :: String }
    | Number { num :: String }
    | Percentage { num :: String }
    deriving (Eq, Ord, Show)

data CSSNumber = Int Int
               | Double Double
               deriving (Eq, Ord, Show)
