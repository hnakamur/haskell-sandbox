module SandBox.Text.CSS.Types
  ( EncodingName
  , AtCharset(..)
  , AtImport(..)
  , AtMedia(..)
  , TokenData(..)
  , StyleSheet(..)
  , Statement(..)
  , Selector(..)
  , SimpleSel(..)
  , getSubSelList
  , Element
  , SubSel(..)
  , PseudoClassSel(..)
  , PseudoElementSel(..)
  , AttrSel(..)
  , AttrName
  , AttrVal
  , AtKeyword(..)
  , Any(..)
  , Block(..)
  , BlockElem(..)
  , Declaration(..)
  , Value(..)
  , ValueElem(..)
  , Color(..)
  , Length(..)
  , LengthUnit(..)
  , Percentage(..)
  , URI(..)
  , PVWhiteSpace(..)
  , BasicColorKeyword(..)
  , toRGBColor
  , basicNameToColor
  , MediaType(..)
  , mediaTypeFromStr
  , mediaTypeToStr
  , mediaTypes
  , Margin(..)
  , AtPage(..)
  , PageSelector(..)
  , MarginDecl(..)
  , MarginVal(..)
  , MarginWidth(..)
  , Important
  {-, tdIdent
  , tdString
  , tdNum
  , tdName
  , tdURI
  , tdStart
  , tdEnd-}
  ) where

import Control.Monad (liftM)
import Data.Char (toLower)
import qualified Data.Map as M (Map, fromList, lookup)

type EncodingName = String

data AtCharset = AtCharset EncodingName
               deriving (Eq, Ord, Show)

data AtImport = AtImport URI [MediaType]
              deriving (Eq, Ord, Show)

data Color = BasicNamedColor BasicColorKeyword
           | RGBColor Int Int Int
           deriving (Eq, Ord, Show)

data BasicColorKeyword = Black
                       | Silver
                       | Gray
                       | White
                       | Maroon
                       | Red
                       | Purple
                       | Fuchsia
                       | Green
                       | Lime
                       | Olive
                       | Yellow
                       | Navy
                       | Blue
                       | Teal
                       | Aqua
                       deriving (Eq, Ord, Show)

basicColorMap :: M.Map String BasicColorKeyword
basicColorMap = M.fromList
  [ ("black", Black)
  , ("silver", Silver)
  , ("gray", Gray)
  , ("white", White)
  , ("maroon", Maroon)
  , ("red", Red)
  , ("purple", Purple)
  , ("fuchsia", Fuchsia)
  , ("green", Green)
  , ("lime", Lime)
  , ("olive", Olive)
  , ("yellow", Yellow)
  , ("navy", Navy)
  , ("blue", Blue)
  , ("teal", Teal)
  , ("aqua", Aqua)
  ]

basicNameToColor :: String -> Maybe Color
basicNameToColor name = liftM BasicNamedColor $
                          M.lookup (map toLower name) basicColorMap

toRGBColor :: Color -> Color
toRGBColor (BasicNamedColor Black)   = RGBColor 0 0 0
toRGBColor (BasicNamedColor Silver)  = RGBColor 192 192 192
toRGBColor (BasicNamedColor Gray)    = RGBColor 128 128 128
toRGBColor (BasicNamedColor White)   = RGBColor 255 255 255
toRGBColor (BasicNamedColor Maroon)  = RGBColor 128 0 0
toRGBColor (BasicNamedColor Red)     = RGBColor 255 0 0
toRGBColor (BasicNamedColor Purple)  = RGBColor 128 0 128
toRGBColor (BasicNamedColor Fuchsia) = RGBColor 255 0 255
toRGBColor (BasicNamedColor Green)   = RGBColor 0 128 0
toRGBColor (BasicNamedColor Lime)    = RGBColor 0 255 0
toRGBColor (BasicNamedColor Olive)   = RGBColor 128 128 0
toRGBColor (BasicNamedColor Yellow)  = RGBColor 255 255 0
toRGBColor (BasicNamedColor Navy)    = RGBColor 0 0 128
toRGBColor (BasicNamedColor Blue)    = RGBColor 0 0 255
toRGBColor (BasicNamedColor Teal)    = RGBColor 0 128 128
toRGBColor (BasicNamedColor Aqua)    = RGBColor 0 255 255

data Length = Length Double (Maybe LengthUnit)
            deriving (Eq, Ord, Show)

data LengthUnit = Em
                | Ex
                | In
                | Cm
                | Mm
                | Pt
                | Pc
                | Px
                deriving (Eq, Ord, Show)

data Percentage = Percentage Double
                deriving (Eq, Ord, Show)

data PVWhiteSpace = PVWhiteSpaceNormal
                  | PVWhiteSpacePre
                  | PVWhiteSpaceNoWrap
                  | PVWhiteSpacePreWrap
                  | PVWhiteSpacePreLine
                  | PVWhiteSpaceInherit
                  deriving (Eq, Ord, Show)

type URI = String

type Important = Bool

data StyleSheet = StyleSheet [Statement]
                deriving (Eq, Ord, Show)

data Statement = RuleSet [Selector] [Declaration]
               | AtRule AtKeyword [Any] (Maybe Block)
               deriving (Eq, Ord, Show)

data Selector = SimpleSel SimpleSel
              | DescendSel Selector Selector
              | ChildSel Selector Selector
              | AdjSel Selector Selector
              deriving (Eq, Ord, Show)

data SimpleSel = UnivSel [SubSel]
               | TypeSel Element [SubSel]
               deriving (Eq, Ord, Show)

getSubSelList :: SimpleSel -> [SubSel]
getSubSelList (UnivSel ss) = ss
getSubSelList (TypeSel _ ss) = ss

type Element = String

data SubSel = AttrSel AttrSel
            | ClassSel Class
            | IdSel Id
            | SSPseudoClassSel PseudoClassSel
            | SSPseudoElementSel PseudoElementSel
            deriving (Eq, Ord, Show)

type Class = String
type Id = String

data PseudoClassSel = PCSFirstChild
                    | PCSLink
                    | PCSVisited
                    | PCSHover
                    | PCSActive
                    | PCSFocus
                    | PCSLang String
                    deriving (Eq, Ord, Show)

data PseudoElementSel = PESFirstLine
                      | PESFirstLetter
                      | PESBefore
                      | PESAfter
                      deriving (Eq, Ord, Show)

data AttrSel = AttrExists AttrName
             | AttrEq AttrName AttrVal
             | AttrContains AttrName AttrVal
             | AttrBegins AttrName AttrVal
             deriving (Eq, Ord, Show)

type AttrName = String
type AttrVal = String

data MediaType = MTBraille
               | MTEmbossed
               | MTHandheld
               | MTPrint
               | MTProjection
               | MTScreen
               | MTSpeech
               | MTTty
               | MTTv
               deriving (Eq, Ord, Show)

mediaTypeFromStr :: String -> Maybe MediaType
mediaTypeFromStr s = M.lookup (toLowerStr s) mediaTypeMap

mediaTypeToStr :: MediaType -> String
mediaTypeToStr t = case (M.lookup t mediaTypeRevMap) of
                     Just s -> s
                     Nothing -> error "invalid mediaType"

mediaTypeRevMap :: M.Map MediaType String 
mediaTypeRevMap = M.fromList (map (\(s, t) -> (t, s)) mediaTypeAssocList)

mediaTypes :: [String]
mediaTypes = map fst mediaTypeAssocList

mediaTypeMap :: M.Map String MediaType
mediaTypeMap = M.fromList mediaTypeAssocList

mediaTypeAssocList :: [(String, MediaType)]
mediaTypeAssocList =
  [ ("braille", MTBraille)
  , ("embossed", MTHandheld)
  , ("print", MTPrint)
  , ("projection", MTProjection)
  , ("screen", MTScreen)
  , ("speech", MTSpeech)
  , ("tty", MTTty)
  , ("tv", MTTv)
  ]

toLowerStr :: String -> String
toLowerStr = map toLower

{-data AttrVal = AttrValStr String
             | AttrValId String
             deriving (Eq, Ord, Show)-}

data AtPage = AtPage (Maybe PageSelector) [MarginDecl]
            deriving (Eq, Ord, Show)

data PageSelector = PSFirst
                  | PSLeft
                  | PSRight
                  deriving (Eq, Ord, Show)

data Margin = Margin
                { topMargin :: MarginVal
                , rightMargin :: MarginVal
                , bottomMargin :: MarginVal
                , leftMargin :: MarginVal
                }
            deriving (Eq, Ord, Show)

data MarginDecl = MarginDecl [MarginVal]
                | MarginTopDecl MarginVal
                | MarginBottomDecl MarginVal
                | MarginRightDecl MarginVal
                | MarginLeftDecl MarginVal
                deriving (Eq, Ord, Show)

data MarginVal = MVWidth MarginWidth
               | MVInherit
               deriving (Eq, Ord, Show)

data MarginWidth = MWLength Length
                 | MWPercentage Percentage
                 | MWAuto
                 deriving (Eq, Ord, Show)

data AtMedia = AtMedia [MediaType] [Statement]
             deriving (Eq, Ord, Show)

data AtKeyword = AtKeyword String
               deriving (Eq, Ord, Show)

data Any = Ident String
         | Number String
         {-| Percentage String-}
         | Dimension String String
         | CSSString String
         {-| URI String-}
         | Hash String
         | UnicodeRange Char Char
         | Colon
         deriving (Eq, Ord, Show)

data Block = Block [BlockElem]
           deriving (Eq, Ord, Show)

data BlockElem = BEAny Any
               | BEBlock Block
               | BEAtKeyword AtKeyword
               deriving (Eq, Ord, Show)

data Declaration = Declaration String Value
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
