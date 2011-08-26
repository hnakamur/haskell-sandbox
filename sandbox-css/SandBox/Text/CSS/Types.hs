module SandBox.Text.CSS.Types
  ( EncodingName
  , AtCharset(..)
  , AtImport(..)
  , AtMedia(..)
  , Id
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
  , AtPage(..)
  , PageSelector(..)
  , MarginVal(..)
  , MarginWidth(..)
  , PaddingVal(..)
  , PaddingWidth(..)
  , BorderWidthVal(..)
  , BorderWidth(..)
  , BorderColorVal(..)
  , BorderColor(..)
  , BorderStyleVal(..)
  , BorderStyle(..)
  , BorderVal(..)
  , BorderValElem(..)
  , DisplayVal(..)
  , PositionVal(..)
  , BoxOffsetVal(..)
  , FloatVal(..)
  , ClearVal(..)
  , ZIndexVal(..)
  , DirectionVal(..)
  , UnicodeBidiVal(..)
  , WidthVal(..)
  , MinWidthVal(..)
  , MaxWidthVal(..)
  , HeightVal(..)
  , MinHeightVal(..)
  , MaxHeightVal(..)
  , LineHeightVal(..)
  , VerticalAlignVal(..)
  , OverflowVal(..)
  , ClipVal(..)
  , ClipOffset(..)
  , VisibilityVal(..)
  , ContentVal(..)
  , ContentValElem(..)
  , Counter(..)
  , ListStyleTypeVal(..)
  , ListStyleType(..)
  , ListStyleImageVal(..)
  , ListStyleImage(..)
  , ListStylePositionVal(..)
  , ListStylePosition(..)
  , ListStyleVal(..)
  , ListStyleValElem(..)
  , QuotesVal(..)
  , QuotePair
  , CounterResetVal(..)
  , CounterIncrementVal(..)
  , PageBreakBeforeVal(..)
  , PageBreakAfterVal(..)
  , PageBreakInsideVal(..)
  , OrphansVal(..)
  , WidowsVal(..)
  , ColorVal(..)
  , BackgroundColorVal(..)
  , BackgroundImageVal(..)
  , BackgroundRepeatVal(..)
  , BackgroundAttachmentVal(..)
  , BackgroundPositionVal(..)
  , HorizPos(..)
  , VertPos(..)
  , BackgroundVal(..)
  , BackgroundValElem(..)
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

data AtPage = AtPage (Maybe PageSelector) [Declaration]
            deriving (Eq, Ord, Show)

data PageSelector = PSFirst
                  | PSLeft
                  | PSRight
                  deriving (Eq, Ord, Show)

data MarginVal = MVWidth MarginWidth
               | MVInherit
               deriving (Eq, Ord, Show)

data MarginWidth = MWLength Length
                 | MWPercentage Percentage
                 | MWAuto
                 deriving (Eq, Ord, Show)


data PaddingVal = PadWidth PaddingWidth
                | PadInherit
                deriving (Eq, Ord, Show)

data PaddingWidth = PWLength Length
                  | PWPercentage Percentage
                  deriving (Eq, Ord, Show)


data BorderWidthVal = BWVWidth BorderWidth
                    | BWVInherit
                    deriving (Eq, Ord, Show)

data BorderWidth = BWLength Length
                 | BWThin
                 | BWMedium
                 | BWThick
                 deriving (Eq, Ord, Show)



data BorderColorVal = BCVColor BorderColor
                    | BCVInherit
                    deriving (Eq, Ord, Show)

data BorderColor = BCColor Color
                 | BCTransparent
                 deriving (Eq, Ord, Show)



data BorderStyleVal = BSVStyle BorderStyle
                    | BSVInherit
                    deriving (Eq, Ord, Show)

data BorderStyle = BSNone
                 | BSHidden
                 | BSDotted
                 | BSDashed
                 | BSSolid
                 | BSDouble
                 | BSGroove
                 | BSRidge
                 | BSInset
                 | BSOutset
                 deriving (Eq, Ord, Show)


data BorderVal = BVBorder [BorderValElem]
               | BVInherit
               deriving (Eq, Ord, Show)

data BorderValElem = BVEWidth BorderWidth
                   | BVEStyle BorderStyle
                   | BVEColor BorderColor
                   deriving (Eq, Ord, Show)

data DisplayVal = DVInline
                | DVBlock
                | DVListItem
                | DVInlineBlock
                | DVTable
                | DVInlineTable
                | DVTableRowGroup
                | DVTableHeaderGroup
                | DVTableFooterGroup
                | DVTableRow
                | DVTableColumnGroup
                | DVTableColumn
                | DVTableCell
                | DVTableCaption
                | DVNone
                | DVInherit
                deriving (Eq, Ord, Show)

data PositionVal = PosStatic
                 | PosRelative
                 | PosAbsolute
                 | PosFixed
                 | PosInherit
                 deriving (Eq, Ord, Show)

data BoxOffsetVal = BOVLength Length
                  | BOVPercentage Percentage
                  | BOVAuto
                  | BOVInherit
                  deriving (Eq, Ord, Show)

data FloatVal = FVLeft
              | FVRight
              | FVNone
              | FVInherit
              deriving (Eq, Ord, Show)

data ClearVal = CleNone
              | CleLeft
              | CleRight
              | CleBoth
              | CleInherit
              deriving (Eq, Ord, Show)

data ZIndexVal = ZIndAuto
               | ZIndInt Int
               | ZIndInherit
               deriving (Eq, Ord, Show)

data DirectionVal = DirLtr
                  | DirRtl
                  | DirInherit
                  deriving (Eq, Ord, Show)

data UnicodeBidiVal
    = UBdNormal
    | UBdEmbed
    | UBdBidiOverride
    | UBdInherit
    deriving (Eq, Ord, Show)


data WidthVal
    = WidLength Length
    | WidPercentage Percentage
    | WidAuto
    | WidInherit
    deriving (Eq, Ord, Show)

data MinWidthVal
    = MinWidLength Length
    | MinWidPercentage Percentage
    | MinWidInherit
    deriving (Eq, Ord, Show)

data MaxWidthVal
    = MaxWidLength Length
    | MaxWidPercentage Percentage
    | MaxWidNone
    | MaxWidInherit
    deriving (Eq, Ord, Show)

data HeightVal
    = HeiLength Length
    | HeiPercentage Percentage
    | HeiAuto
    | HeiInherit
    deriving (Eq, Ord, Show)

data MinHeightVal
    = MinHeiLength Length
    | MinHeiPercentage Percentage
    | MinHeiInherit
    deriving (Eq, Ord, Show)

data MaxHeightVal
    = MaxHeiLength Length
    | MaxHeiPercentage Percentage
    | MaxHeiNone
    | MaxHeiInherit
    deriving (Eq, Ord, Show)

data LineHeightVal
    = LinHeiNormal
    | LinHeiNumber Double
    | LinHeiLength Length
    | LinHeiPercentage Percentage
    | LinHeiInherit
    deriving (Eq, Ord, Show)

data VerticalAlignVal
    = VAliBaseline
    | VAliSub
    | VAliSuper
    | VAliTop
    | VAliTextTop
    | VAliMiddle
    | VAliBottom
    | VAliTextBottom
    | VAliLength Length
    | VAliPercentage Percentage
    | VAliInherit
    deriving (Eq, Ord, Show)

data OverflowVal
    = OveVisible
    | OveHidden
    | OveScroll
    | OveAuto
    | OveInherit
    deriving (Eq, Ord, Show)

data ClipVal
    = CliShape ClipOffset ClipOffset ClipOffset ClipOffset
    | CliAuto
    | CliInherit
    deriving (Eq, Ord, Show)

data ClipOffset
    = CliOffLength Length
    | CliOffAuto
    deriving (Eq, Ord, Show)

data VisibilityVal
    = VisVisible
    | VisHidden
    | VisCollapse
    | VisInherit
    deriving (Eq, Ord, Show)

data ContentVal
    = ConNormal
    | ConNone
    | ConValues [ContentValElem]
    | ConInherit
    deriving (Eq, Ord, Show)

data ContentValElem
    = CVEString String
    | CVEURI URI
    | CVECounter Counter
    | CVEAttr Id
    | CVEOpenQuote
    | CVECloseQuote
    | CVENoOpenQuote
    | CVENoCloseQuote
    deriving (Eq, Ord, Show)

data Counter
    = Counter Id (Maybe ListStyleType)
    | Counters Id String (Maybe ListStyleType)
    deriving (Eq, Ord, Show)

data ListStyleTypeVal
    = LSTVType ListStyleType
    | LSTVInherit
    deriving (Eq, Ord, Show)

data ListStyleType
    = LSTDisc
    | LSTCircle
    | LSTSquare
    | LSTDecimal
    | LSTDecimalLeadingZero
    | LSTLowerRoman
    | LSTUpperRoman
    | LSTLowerGreek
    | LSTLowerLatin
    | LSTUpperLatin
    | LSTArmenian
    | LSTGeorgian
    | LSTLowerAlpha
    | LSTUpperAlpha
    | LSTNone
    deriving (Eq, Ord, Show)

data ListStyleImageVal
    = LSIVImage ListStyleImage
    | LSIVInherit
    deriving (Eq, Ord, Show)

data ListStyleImage
    = LSIURI URI
    | LSINone
    deriving (Eq, Ord, Show)

data ListStylePositionVal
    = LSPVPosition ListStylePosition
    | LSPVInherit
    deriving (Eq, Ord, Show)

data ListStylePosition
    = LSPInside
    | LSPOutside
    deriving (Eq, Ord, Show)

data ListStyleVal
    = LSVValues [ListStyleValElem]
    | LSVInherit
    deriving (Eq, Ord, Show)

data ListStyleValElem
    = LSVEType ListStyleType
    | LSVEPosition ListStylePosition
    | LSVEImage ListStyleImage
    deriving (Eq, Ord, Show)

data QuotesVal
    = QVQuotePairs [QuotePair]
    | QVNone
    | QVInherit
    deriving (Eq, Ord, Show)

type QuotePair = (String, String)

data CounterResetVal
    = CRVCounters [(Id, (Maybe Int))]
    | CRVNone
    | CRVInherit
    deriving (Eq, Ord, Show)

data CounterIncrementVal
    = CIVCounters [(Id, (Maybe Int))]
    | CIVNone
    | CIVInherit
    deriving (Eq, Ord, Show)


data PageBreakBeforeVal
    = PBBVAuto
    | PBBVAlways
    | PBBVAvoid
    | PBBVLeft
    | PBBVRight
    | PBBVInherit
    deriving (Eq, Ord, Show)

data PageBreakAfterVal
    = PBAVAuto
    | PBAVAlways
    | PBAVAvoid
    | PBAVLeft
    | PBAVRight
    | PBAVInherit
    deriving (Eq, Ord, Show)

data PageBreakInsideVal
    = PBIVAvoid
    | PBIVAuto
    | PBIVInherit
    deriving (Eq, Ord, Show)

data OrphansVal
    = OVInt Int
    | OVInherit
    deriving (Eq, Ord, Show)

data WidowsVal
    = WVInt Int
    | WVInherit
    deriving (Eq, Ord, Show)

data ColorVal
    = CVColor Color
    | CVInherit
    deriving (Eq, Ord, Show)

data BackgroundColorVal
    = BgCVColor Color
    | BgCVTransparent
    | BgCVInherit
    deriving (Eq, Ord, Show)

data BackgroundImageVal
    = BgIVURI URI
    | BgIVNone
    | BgIVInherit
    deriving (Eq, Ord, Show)

data BackgroundRepeatVal
    = BgRVRepeat
    | BgRVRepeatX
    | BgRVRepeatY
    | BgRVNoRepeat
    | BgRVInherit
    deriving (Eq, Ord, Show)

data BackgroundAttachmentVal
    = BgAVScroll
    | BgAVFixed
    | BgAVInherit
    deriving (Eq, Ord, Show)

data BackgroundPositionVal
    = BgPVPos HorizPos VertPos
    | BgPVInherit
    deriving (Eq, Ord, Show)

data HorizPos
    = HPosPercentage Percentage
    | HPosLength Length
    | HPosLeft
    | HPosCenter
    | HPosRight
    deriving (Eq, Ord, Show)

data VertPos
    = VPosPercentage Percentage
    | VPosLength Length
    | VPosTop
    | VPosCenter
    | VPosBottom
    deriving (Eq, Ord, Show)

data BackgroundVal
    = BgVValues [BackgroundValElem]
    | BgVInherit
    deriving (Eq, Ord, Show)

data BackgroundValElem
    = BgVEColor BackgroundColorVal
    | BgVEImage BackgroundImageVal
    | BgVERepeat BackgroundRepeatVal
    | BgVEAttachment BackgroundAttachmentVal
    | BgVEPosition BackgroundPositionVal
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


data Declaration
    = DeclMargin [MarginVal] Important
    | DeclMarginTop MarginVal Important
    | DeclMarginBottom MarginVal Important
    | DeclMarginRight MarginVal Important
    | DeclMarginLeft MarginVal Important
    | DeclPadding [PaddingVal] Important
    | DeclPaddingTop PaddingVal Important
    | DeclPaddingBottom PaddingVal Important
    | DeclPaddingRight PaddingVal Important
    | DeclPaddingLeft PaddingVal Important
    | DeclBorderWidth [BorderWidthVal] Important
    | DeclBorderTopWidth BorderWidthVal Important
    | DeclBorderBottomWidth BorderWidthVal Important
    | DeclBorderRightWidth BorderWidthVal Important
    | DeclBorderLeftWidth BorderWidthVal Important
    | DeclBorderColor [BorderColorVal] Important
    | DeclBorderTopColor BorderColorVal Important
    | DeclBorderBottomColor BorderColorVal Important
    | DeclBorderRightColor BorderColorVal Important
    | DeclBorderLeftColor BorderColorVal Important
    | DeclBorderStyle [BorderStyleVal] Important
    | DeclBorderTopStyle BorderStyleVal Important
    | DeclBorderBottomStyle BorderStyleVal Important
    | DeclBorderRightStyle BorderStyleVal Important
    | DeclBorderLeftStyle BorderStyleVal Important
    | DeclBorder BorderVal Important
    | DeclBorderTop BorderVal Important
    | DeclBorderBottom BorderVal Important
    | DeclBorderRight BorderVal Important
    | DeclBorderLeft BorderVal Important
    | DeclDisplay DisplayVal Important
    | DeclPosition PositionVal Important
    | DeclTop BoxOffsetVal Important
    | DeclRight BoxOffsetVal Important
    | DeclBottom BoxOffsetVal Important
    | DeclLeft BoxOffsetVal Important
    | DeclFloat FloatVal Important
    | DeclClear ClearVal Important
    | DeclZIndex ZIndexVal Important
    | DeclDirection DirectionVal Important
    | DeclUnicodeBidi UnicodeBidiVal Important
    | DeclWidth WidthVal Important
    | DeclMinWidth MinWidthVal Important
    | DeclMaxWidth MaxWidthVal Important
    | DeclHeight HeightVal Important
    | DeclMinHeight MinHeightVal Important
    | DeclMaxHeight MaxHeightVal Important
    | DeclLineHeight LineHeightVal Important
    | DeclVerticalAlign VerticalAlignVal Important
    | DeclOverflow OverflowVal Important
    | DeclClip ClipVal Important
    | DeclVisibility VisibilityVal Important
    | DeclContent ContentVal Important
    | DeclQuotes QuotesVal Important
    | DeclCounterReset CounterResetVal Important
    | DeclCounterIncrement CounterIncrementVal Important
    | DeclListStyleType ListStyleTypeVal Important
    | DeclListStyleImage ListStyleImageVal Important
    | DeclListStylePosition ListStylePositionVal Important
    | DeclListStyle ListStyleVal Important
    | DeclPageBreakBefore PageBreakBeforeVal Important
    | DeclPageBreakAfter PageBreakAfterVal Important
    | DeclPageBreakInside PageBreakInsideVal Important
    | DeclOrphans OrphansVal Important
    | DeclWidows WidowsVal Important
    | DeclColor ColorVal Important
    | DeclBackgroundColor BackgroundColorVal Important
    | DeclBackgroundImage BackgroundImageVal Important
    | DeclBackgroundRepeat BackgroundRepeatVal Important
    | DeclBackgroundAttachment BackgroundAttachmentVal Important
    | DeclBackgroundPosition BackgroundPositionVal Important
    | DeclBackground BackgroundVal Important
    deriving (Eq, Ord, Show)

{-data Declaration = Declaration String Value
                 deriving (Eq, Ord, Show)-}

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
