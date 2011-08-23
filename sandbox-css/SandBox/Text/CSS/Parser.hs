{-# LANGUAGE FlexibleContexts #-} 
module SandBox.Text.CSS.Parser
    ( 
stylesheet
, selectorList
, selector
, atRule
, declaration
, value
, block
, atKeyword
, any
, uri
, ident
, name
, num
, int
, badstring
, string
, unicode
, crlf
, nl
, optional
, simpleEscape
, isSimpleEscapeChar
, color
, lexeme
, symbol
, braces
, declarationBlock
, semi
, attrSel
, simpleSel
, pseudoClassSel
, strCase
, keywordCase
, lengthVal
, pvWhiteSpace
, atCharset
, atImport
, atPage
, atMedia
, important
, hasPseudoElement
, composeSel
    ) where

import Control.Monad (liftM)
import Data.Char (chr, toLower)
import Data.Functor.Identity (Identity)
import qualified Data.Map as M (Map, fromList, lookup)
import Numeric (readHex)
import Text.Parsec hiding (string, space, spaces)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec as S (string)
import qualified Text.Parsec.Token as P
import SandBox.Text.CSS.Char
import SandBox.Text.CSS.Parsec.Combinator
import SandBox.Text.CSS.Types
import Prelude hiding (any)
import qualified Prelude as R (any)

stylesheet :: Stream s m Char => ParsecT s u m [Statement]
stylesheet = do
    spaces
    many statement

statement :: Stream s m Char => ParsecT s u m Statement
statement = atRule <|> ruleSet

atRule :: Stream s m Char => ParsecT s u m Statement
atRule = do
    k <- atKeyword
    xs <- many (spaces >> any)
    b <- (optionMaybe (try (spaces >> block)) <|> return Nothing)
    return (AtRule k xs b)

atCharset :: Stream s m Char => ParsecT s u m AtCharset
atCharset = do
    S.string "@charset \""
    n <- encodingName
    S.string "\";"
    return (AtCharset n)

encodingName :: Stream s m Char => ParsecT s u m EncodingName
encodingName = name

atImport :: Stream s m Char => ParsecT s u m AtImport
atImport = do
    keywordCase "@import"
    spaces
    u <- (uri <|> string)
    spaces
    ts <- (try mediaTypeList <|> return [])
    spaces
    semi
    return (AtImport u ts)

mediaTypeList :: Stream s m Char => ParsecT s u m [MediaType]
mediaTypeList = (try (keywordCase "all") >> return [])
                <|>
                (try (sepBy1 mediaType comma))
  where
    mediaType :: Stream s m Char => ParsecT s u m MediaType
    mediaType = choice
                  [ try (keywordCase "braille") >> return MTBraille
                  , try (keywordCase "embossed") >> return MTHandheld
                  , try (keywordCase "print") >> return MTPrint
                  , try (keywordCase "projection") >> return MTProjection
                  , try (keywordCase "screen") >> return MTScreen
                  , try (keywordCase "speech") >> return MTSpeech
                  , try (keywordCase "tty") >> return MTTty
                  , try (keywordCase "tv") >> return MTTv
                  ]

atPage :: Stream s m Char => ParsecT s u m AtPage
atPage = do
    keywordCase "@page"
    spaces
    s <- optionMaybe pageSelector
    spaces
    b <- atPageBlock
    return (AtPage s b)

pageSelector :: Stream s m Char => ParsecT s u m PageSelector
pageSelector = choice
    [ try (keyword ":first") >> return PSFirst
    , try (keyword ":left") >> return PSLeft
    , try (keyword ":right") >> return PSRight
    ]

atPageBlock :: Stream s m Char => ParsecT s u m [Declaration]
atPageBlock = braces (sepEndBy (marginDecl <?> "margin declaration") semi)

marginDecl :: Stream s m Char => ParsecT s u m Declaration
marginDecl = do
    n <- try ident <?> "property name"
    if n `elem` marginPropNames
        then case (M.lookup (map toLower n) declSubMap) of
                 Just d -> d
                 Nothing -> unexpected ("property name: " ++ show n)
        else fail "only margin properties are allowed in @page block."

marginPropNames :: [String]
marginPropNames =
    ["margin-top", "margin-bottom", "margin-right", "margin-left", "margin"]

declaration :: Stream s m Char => ParsecT s u m Declaration
declaration = do
    n <- try ident <?> "property name"
    case (M.lookup (map toLower n) declSubMap) of
        Just d -> d
        Nothing -> unexpected ("property name: " ++ show n)

declSub :: Stream s m Char =>
               ParsecT s u m v -> (v -> Important -> Declaration) ->
               String -> ParsecT s u m Declaration
declSub valueParser ctor propName = do
    spaces
    colon
    v <- (valueParser <?> "property value")
         <|> fail ("invalid value for property " ++ show propName)
    spaces
    i <- important
    return (ctor v i)

declSubMap :: Stream s m Char => M.Map String (ParsecT s u m Declaration)
declSubMap = M.fromList $ map (\(k, v) -> (k, v k))
    [ ("border-top-width", declSub individualBorderWidthVal DeclBorderTopWidth)
    , ("border-bottom-width", declSub individualBorderWidthVal DeclBorderBottomWidth)
    , ("border-right-width", declSub individualBorderWidthVal DeclBorderRightWidth)
    , ("border-left-width", declSub individualBorderWidthVal DeclBorderLeftWidth)
    , ("border-width", declSub shortHandBorderWidthVal DeclBorderWidth)
    , ("border-top-color", declSub individualBorderColorVal DeclBorderTopColor)
    , ("border-bottom-color", declSub individualBorderColorVal DeclBorderBottomColor)
    , ("border-right-color", declSub individualBorderColorVal DeclBorderRightColor)
    , ("border-left-color", declSub individualBorderColorVal DeclBorderLeftColor)
    , ("border-color", declSub shortHandBorderColorVal DeclBorderColor)
    , ("border-top-style", declSub individualBorderStyleVal DeclBorderTopStyle)
    , ("border-bottom-style", declSub individualBorderStyleVal DeclBorderBottomStyle)
    , ("border-right-style", declSub individualBorderStyleVal DeclBorderRightStyle)
    , ("border-left-style", declSub individualBorderStyleVal DeclBorderLeftStyle)
    , ("border-style", declSub shortHandBorderStyleVal DeclBorderStyle)
    , ("border-top", declSub borderVal DeclBorderTop)
    , ("border-bottom", declSub borderVal DeclBorderBottom)
    , ("border-right", declSub borderVal DeclBorderRight)
    , ("border-left", declSub borderVal DeclBorderLeft)
    , ("border", declSub borderVal DeclBorder)
    , ("margin-top", declSub individualMarginVal DeclMarginTop)
    , ("margin-bottom", declSub individualMarginVal DeclMarginBottom)
    , ("margin-right", declSub individualMarginVal DeclMarginRight)
    , ("margin-left", declSub individualMarginVal DeclMarginLeft)
    , ("margin", declSub shortHandMarginVal DeclMargin)
    , ("padding-top", declSub individualPaddingVal DeclPaddingTop)
    , ("padding-bottom", declSub individualPaddingVal DeclPaddingBottom)
    , ("padding-right", declSub individualPaddingVal DeclPaddingRight)
    , ("padding-left", declSub individualPaddingVal DeclPaddingLeft)
    , ("padding", declSub shortHandPaddingVal DeclPadding)
    , ("display", declSub displayVal DeclDisplay)
    , ("position", declSub positionVal DeclPosition)
    , ("top", declSub boxOffsetVal DeclTop)
    , ("right", declSub boxOffsetVal DeclRight)
    , ("bottom", declSub boxOffsetVal DeclBottom)
    , ("left", declSub boxOffsetVal DeclLeft)
    , ("float", declSub floatVal DeclFloat)
    , ("clear", declSub clearVal DeclClear)
    , ("z-index", declSub zIndexVal DeclZIndex)
    , ("direction", declSub directionVal DeclDirection)
    , ("unicode-bidi", declSub unicodeBidiVal DeclUnicodeBidi)
    , ("width", declSub widthVal DeclWidth)
    , ("min-width", declSub minWidthVal DeclMinWidth)
    , ("max-width", declSub maxWidthVal DeclMaxWidth)
    , ("height", declSub heightVal DeclHeight)
    , ("min-height", declSub minHeightVal DeclMinHeight)
    , ("max-height", declSub maxHeightVal DeclMaxHeight)
    , ("line-height", declSub lineHeightVal DeclLineHeight)
    , ("vertical-align", declSub verticalAlignVal DeclVerticalAlign)
    , ("overflow", declSub overflowVal DeclOverflow)
    , ("clip", declSub clipVal DeclClip)
    , ("visibility", declSub visibilityVal DeclVisibility)
    ]


individualMarginVal :: Stream s m Char => ParsecT s u m MarginVal
individualMarginVal = choice
    [ try marginWidth >>= \w -> return (MVWidth w)
    , try (keywordCase "inherit") >> return MVInherit
    ]

shortHandMarginVal :: Stream s m Char => ParsecT s u m [MarginVal]
shortHandMarginVal = choice
    [ try (countRange 1 4 (marginWidth >>= \w -> spaces >> return (MVWidth w)))
    , try (keywordCase "inherit") >> return [MVInherit]
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
marginWidth :: Stream s m Char => ParsecT s u m MarginWidth
marginWidth = choice
    [ try percentage >>= \p -> return (MWPercentage p)
    , try lengthVal >>= \l -> return (MWLength l)
    , try (keywordCase "auto") >> return MWAuto
    ]


individualPaddingVal :: Stream s m Char => ParsecT s u m PaddingVal
individualPaddingVal = choice
    [ try paddingWidth >>= \w -> return (PadWidth w)
    , try (keywordCase "inherit") >> return PadInherit
    ]

shortHandPaddingVal :: Stream s m Char => ParsecT s u m [PaddingVal]
shortHandPaddingVal = choice
    [ try (countRange 1 4 (paddingWidth >>= \w -> spaces >>
                           return (PadWidth w)))
    , try (keywordCase "inherit") >> return [PadInherit]
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
paddingWidth :: Stream s m Char => ParsecT s u m PaddingWidth
paddingWidth = choice
    [ try percentage >>= \p -> return (PWPercentage p)
    , try lengthVal >>= \l -> return (PWLength l)
    ]



individualBorderWidthVal :: Stream s m Char => ParsecT s u m BorderWidthVal
individualBorderWidthVal = choice
    [ try borderWidth >>= \w -> return (BWVWidth w)
    , try (keywordCase "inherit") >> return BWVInherit
    ]

shortHandBorderWidthVal :: Stream s m Char => ParsecT s u m [BorderWidthVal]
shortHandBorderWidthVal = choice
    [ try (countRange 1 4 (borderWidth >>= \w -> spaces >>
                           return (BWVWidth w)))
    , try (keywordCase "inherit") >> return [BWVInherit]
    ]

borderWidth :: Stream s m Char => ParsecT s u m BorderWidth
borderWidth = choice
    [ try (keywordCase "thin") >> return BWThin
    , try (keywordCase "medium") >> return BWMedium
    , try (keywordCase "thick") >> return BWThick
    , try lengthVal >>= \l -> return (BWLength l)
    ]


individualBorderColorVal :: Stream s m Char => ParsecT s u m BorderColorVal
individualBorderColorVal = choice
    [ try borderColor >>= \w -> return (BCVColor w)
    , try (keywordCase "inherit") >> return BCVInherit
    ]

shortHandBorderColorVal :: Stream s m Char => ParsecT s u m [BorderColorVal]
shortHandBorderColorVal = choice
    [ try (countRange 1 4 (borderColor >>= \w -> spaces >>
                           return (BCVColor w)))
    , try (keywordCase "inherit") >> return [BCVInherit]
    ]

borderColor :: Stream s m Char => ParsecT s u m BorderColor
borderColor = choice
    [ try color >>= \c -> return (BCColor c)
    , try (keywordCase "transparent") >> return BCTransparent
    ]


individualBorderStyleVal :: Stream s m Char => ParsecT s u m BorderStyleVal
individualBorderStyleVal = choice
    [ try borderStyle >>= \w -> return (BSVStyle w)
    , try (keywordCase "inherit") >> return BSVInherit
    ]

shortHandBorderStyleVal :: Stream s m Char => ParsecT s u m [BorderStyleVal]
shortHandBorderStyleVal = choice
    [ try (countRange 1 4 (borderStyle >>= \w -> spaces >>
                           return (BSVStyle w)))
    , try (keywordCase "inherit") >> return [BSVInherit]
    ]

borderStyle :: Stream s m Char => ParsecT s u m BorderStyle
borderStyle = choice
    [ try (keywordCase "none") >> return BSNone
    , try (keywordCase "hidden") >> return BSHidden
    , try (keywordCase "dotted") >> return BSDotted
    , try (keywordCase "dashed") >> return BSDashed
    , try (keywordCase "solid") >> return BSSolid
    , try (keywordCase "double") >> return BSDouble
    , try (keywordCase "groove") >> return BSGroove
    , try (keywordCase "ridge") >> return BSRidge
    , try (keywordCase "inset") >> return BSInset
    , try (keywordCase "outset") >> return BSOutset
    ]


borderVal :: Stream s m Char => ParsecT s u m BorderVal
borderVal = choice
    [ try borderValElems >>= \vs -> return (BVBorder vs)
    , try (keywordCase "inherit") >> return BVInherit
    ]

borderValElems :: Stream s m Char => ParsecT s u m [BorderValElem]
borderValElems = oneOrMoreInAnyOrder
    [ try borderWidth >>= \w -> spaces >> return (BVEWidth w)
    , try borderStyle >>= \s -> spaces >> return (BVEStyle s)
    , try borderColor >>= \c -> spaces >> return (BVEColor c)
    ]

displayVal :: Stream s m Char => ParsecT s u m DisplayVal
displayVal = choice
    [ try (keywordCase "inline") >> return DVInline
    , try (keywordCase "block") >> return DVBlock
    , try (keywordCase "list-item") >> return DVListItem
    , try (keywordCase "inline-block") >> return DVInlineBlock
    , try (keywordCase "table") >> return DVTable
    , try (keywordCase "inline-table") >> return DVInlineTable
    , try (keywordCase "table-row-group") >> return DVTableRowGroup
    , try (keywordCase "table-header-group") >> return DVTableHeaderGroup
    , try (keywordCase "table-footer-group") >> return DVTableFooterGroup
    , try (keywordCase "table-row") >> return DVTableRow
    , try (keywordCase "table-column-group") >> return DVTableColumnGroup
    , try (keywordCase "table-column") >> return DVTableColumn
    , try (keywordCase "table-cell") >> return DVTableCell
    , try (keywordCase "table-caption") >> return DVTableCaption
    , try (keywordCase "none") >> return DVNone
    , try (keywordCase "inherit") >> return DVInherit
    ]


positionVal :: Stream s m Char => ParsecT s u m PositionVal
positionVal = choice
    [ try (keywordCase "static") >> return PosStatic
    , try (keywordCase "relative") >> return PosRelative
    , try (keywordCase "absolute") >> return PosAbsolute
    , try (keywordCase "fixed") >> return PosFixed
    , try (keywordCase "inherit") >> return PosInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
boxOffsetVal :: Stream s m Char => ParsecT s u m BoxOffsetVal
boxOffsetVal = choice
    [ try percentage >>= \p -> return (BOVPercentage p)
    , try lengthVal >>= \l -> return (BOVLength l)
    , try (keywordCase "auto") >> return BOVAuto
    , try (keywordCase "inherit") >> return BOVInherit
    ]

floatVal :: Stream s m Char => ParsecT s u m FloatVal
floatVal = choice
    [ try (keywordCase "left") >> return FVLeft
    , try (keywordCase "right") >> return FVRight
    , try (keywordCase "none") >> return FVNone
    , try (keywordCase "inherit") >> return FVInherit
    ]

clearVal :: Stream s m Char => ParsecT s u m ClearVal
clearVal = choice
    [ try (keywordCase "none") >> return CleNone
    , try (keywordCase "left") >> return CleLeft
    , try (keywordCase "right") >> return CleRight
    , try (keywordCase "both") >> return CleBoth
    , try (keywordCase "inherit") >> return CleInherit
    ]

zIndexVal :: Stream s m Char => ParsecT s u m ZIndexVal
zIndexVal = choice
    [ try (keywordCase "auto") >> return ZIndAuto
    , try int >>= \n -> return (ZIndInt n)
    , try (keywordCase "inherit") >> return ZIndInherit
    ]

directionVal :: Stream s m Char => ParsecT s u m DirectionVal
directionVal = choice
    [ try (keywordCase "ltr") >> return DirLtr
    , try (keywordCase "rtl") >> return DirRtl
    , try (keywordCase "inherit") >> return DirInherit
    ]

unicodeBidiVal :: Stream s m Char => ParsecT s u m UnicodeBidiVal
unicodeBidiVal = choice
    [ try (keywordCase "normal") >> return UBdNormal
    , try (keywordCase "embed") >> return UBdEmbed
    , try (keywordCase "bidi-override") >> return UBdBidiOverride
    , try (keywordCase "inherit") >> return UBdInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
widthVal :: Stream s m Char => ParsecT s u m WidthVal
widthVal = choice
    [ try percentage >>= \p -> return (WidPercentage p)
    , try lengthVal >>= \l -> return (WidLength l)
    , try (keywordCase "auto") >> return WidAuto
    , try (keywordCase "inherit") >> return WidInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
minWidthVal :: Stream s m Char => ParsecT s u m MinWidthVal
minWidthVal = choice
    [ try percentage >>= \p -> return (MinWidPercentage p)
    , try lengthVal >>= \l -> return (MinWidLength l)
    , try (keywordCase "inherit") >> return MinWidInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
maxWidthVal :: Stream s m Char => ParsecT s u m MaxWidthVal
maxWidthVal = choice
    [ try percentage >>= \p -> return (MaxWidPercentage p)
    , try lengthVal >>= \l -> return (MaxWidLength l)
    , try (keywordCase "none") >> return MaxWidNone
    , try (keywordCase "inherit") >> return MaxWidInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
heightVal :: Stream s m Char => ParsecT s u m HeightVal
heightVal = choice
    [ try percentage >>= \p -> return (HeiPercentage p)
    , try lengthVal >>= \l -> return (HeiLength l)
    , try (keywordCase "auto") >> return HeiAuto
    , try (keywordCase "inherit") >> return HeiInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
minHeightVal :: Stream s m Char => ParsecT s u m MinHeightVal
minHeightVal = choice
    [ try percentage >>= \p -> return (MinHeiPercentage p)
    , try lengthVal >>= \l -> return (MinHeiLength l)
    , try (keywordCase "inherit") >> return MinHeiInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
maxHeightVal :: Stream s m Char => ParsecT s u m MaxHeightVal
maxHeightVal = choice
    [ try percentage >>= \p -> return (MaxHeiPercentage p)
    , try lengthVal >>= \l -> return (MaxHeiLength l)
    , try (keywordCase "none") >> return MaxHeiNone
    , try (keywordCase "inherit") >> return MaxHeiInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
lineHeightVal :: Stream s m Char => ParsecT s u m LineHeightVal
lineHeightVal = choice
    [ try (keywordCase "normal") >> return LinHeiNormal
    , try nonNegativeNumber >>= \n -> return (LinHeiNumber n)
    , try percentage >>= \p -> return (LinHeiPercentage p)
    , try lengthVal >>= \l -> return (LinHeiLength l)
    , try (keywordCase "inherit") >> return LinHeiInherit
    ]

nonNegativeNumber :: Stream s m Char => ParsecT s u m Double
nonNegativeNumber = num >>= \s -> return (read s)

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
verticalAlignVal :: Stream s m Char => ParsecT s u m VerticalAlignVal
verticalAlignVal = choice
    [ try (keywordCase "baseline") >> return VAliBaseline
    , try (keywordCase "sub") >> return VAliSub
    , try (keywordCase "super") >> return VAliSuper
    , try (keywordCase "top") >> return VAliTop
    , try (keywordCase "text-top") >> return VAliTextTop
    , try (keywordCase "middle") >> return VAliMiddle
    , try (keywordCase "bottom") >> return VAliBottom
    , try (keywordCase "text-bottom") >> return VAliTextBottom
    , try percentage >>= \p -> return (VAliPercentage p)
    , try lengthVal >>= \l -> return (VAliLength l)
    , try (keywordCase "inherit") >> return VAliInherit
    ]

overflowVal :: Stream s m Char => ParsecT s u m OverflowVal
overflowVal = choice
    [ try (keywordCase "visible") >> return OveVisible
    , try (keywordCase "hidden") >> return OveHidden
    , try (keywordCase "scroll") >> return OveScroll
    , try (keywordCase "auto") >> return OveAuto
    , try (keywordCase "inherit") >> return OveInherit
    ]

clipVal :: Stream s m Char => ParsecT s u m ClipVal
clipVal = choice
    [ try clipShape
    , try (keywordCase "auto") >> return CliAuto
    , try (keywordCase "inherit") >> return CliInherit
    ]

clipShape :: Stream s m Char => ParsecT s u m ClipVal
clipShape = do
    symbol "rect("
    top <- clipOffset
    comma
    right <- clipOffset
    comma
    bottom <- clipOffset
    comma
    left <- clipOffset
    symbol ")"
    return (CliShape top right bottom left)

clipOffset :: Stream s m Char => ParsecT s u m ClipOffset
clipOffset = choice
    [ try lengthVal >>= \l -> return (CliOffLength l)
    , try (keywordCase "auto") >> return CliOffAuto
    ]

visibilityVal :: Stream s m Char => ParsecT s u m VisibilityVal
visibilityVal = choice
    [ try (keywordCase "visible") >> return VisVisible
    , try (keywordCase "hidden") >> return VisHidden
    , try (keywordCase "collapse") >> return VisCollapse
    , try (keywordCase "inherit") >> return VisInherit
    ]

atMedia :: Stream s m Char => ParsecT s u m AtMedia
atMedia = do
    keywordCase "@media"
    spaces
    ts <- mediaTypeList
    spaces
    rs <- braces (many ruleSet)
    return (AtMedia ts rs)

ruleSet :: Stream s m Char => ParsecT s u m Statement
ruleSet = do
    ss <- selectorList
    b <- declarationBlock
    return (RuleSet ss b)

selectorList :: Stream s m Char => ParsecT s u m [Selector]
selectorList = sepBy selector comma

selector :: Stream s m Char => ParsecT s u m Selector
selector =
    simpleSel >>= \s1 -> (
      (try (spaces1 >> selector) >>= \s2 ->
       composeSel DescendSel s1 s2)
      <|>
      (try (spaces >> char '>') >> spaces >> selector >>= \s2 ->
       composeSel ChildSel s1 s2)
      <|>
      (try (spaces >> char '+') >> spaces >> selector >>= \s2 ->
       composeSel AdjSel s1 s2)
      <|>
      return (SimpleSel s1)
    )

simpleSel :: Stream s m Char => ParsecT s u m SimpleSel
simpleSel =
    (typeSel >>= \t ->
     many (try (spaces >> subSel)) >>= \s ->
     return (TypeSel t s)
    )
    <|>
    (univSel >>
     many (try (spaces >> subSel)) >>= \s ->
     return (UnivSel s)
    )
    <|>
    (many1 (try (spaces >> subSel)) >>= \s ->
     return (UnivSel s)
    )

composeSel :: Stream s m Char =>
              (Selector -> Selector -> Selector) -> SimpleSel -> Selector ->
              ParsecT s u m Selector
composeSel op s1 s2 =
    if (hasPseudoElement s1)
        then fail "pseudo-elements may only be appended after the last simple selector of the selector."
        else return (op (SimpleSel s1) s2)

hasPseudoElement :: SimpleSel -> Bool
hasPseudoElement s = R.any isPseudoElement (getSubSelList s)

isPseudoElement :: SubSel -> Bool
isPseudoElement (SSPseudoElementSel _) = True
isPseudoElement _ = False

{- NOTE: typeSel is not lexeme parser. -}
typeSel :: Stream s m Char => ParsecT s u m String
typeSel = name

{- NOTE: univSel is not lexeme parser. -}
univSel :: Stream s m Char => ParsecT s u m String
univSel = S.string "*"

subSel :: Stream s m Char => ParsecT s u m SubSel
subSel = 
    (attrSel >>= \s -> return (AttrSel s))
    <|>
    classSel
    <|>
    idSel
    <|>
    (pseudoClassSel >>= \s -> return (SSPseudoClassSel s))
    <|>
    (pseudoElementSel >>= \s -> return (SSPseudoElementSel s))

attrSel :: Stream s m Char => ParsecT s u m AttrSel
attrSel = between (char '[' >> spaces) (char ']') (
            name >>= \n ->
            spaces >> (
              (try (symbol "=") >> attrVal >>= \v -> return (AttrEq n v))
              <|>
              (try (symbol "~=") >> attrVal >>= \v -> return (AttrContains n v))
              <|>
              (try (symbol "|=") >> attrVal >>= \v -> return (AttrBegins n v))
              <|>
              return (AttrExists n)
            )
          )

attrVal :: Stream s m Char => ParsecT s u m String
attrVal =   identifier
        <|> stringLit

{- Note: No space is allow after '.'. -}
classSel :: Stream s m Char => ParsecT s u m SubSel
classSel = do
    char '.'
    n <- name
    return (ClassSel n)

{- Note: No space is allowed after '#'. -}
idSel :: Stream s m Char => ParsecT s u m SubSel
idSel = do
    char '#'
    i <- ident
    return (IdSel i)

pseudoClassSel :: Stream s m Char => ParsecT s u m PseudoClassSel
pseudoClassSel = choice
    [ try (keywordCase ":first-child") >> return PCSFirstChild
    , try (keywordCase ":link") >> return PCSLink
    , try (keywordCase ":visited") >> return PCSVisited
    , try (keywordCase ":hover") >> return PCSHover
    , try (keywordCase ":active") >> return PCSActive
    , try (keywordCase ":focus") >> return PCSFocus
    , try (keywordCase ":lang") >> parens identifier >>= \i ->
        return (PCSLang i)
    ]

pseudoElementSel :: Stream s m Char => ParsecT s u m PseudoElementSel
pseudoElementSel = choice
    [ try (keywordCase ":first-line") >> return PESFirstLine
    , try (keywordCase ":first-letter") >> return PESFirstLetter
    , try (keywordCase ":before") >> return PESBefore
    , try (keywordCase ":after") >> return PESAfter
    ]

declarationBlock :: Stream s m Char => ParsecT s u m [Declaration]
declarationBlock =
    spaces >> braces (sepEndBy (declaration <?> "declaration") semi)

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = between (symbol "(") (symbol ")")

braces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braces = between (symbol "{") (symbol "}")

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme p = do { x <- p; spaces; return x }

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol name = lexeme (S.string name)

{-hash :: Stream s m Char => ParsecT s u m String
hash = symbol "#"-}

dot :: Stream s m Char => ParsecT s u m String
dot = symbol "."

semi :: Stream s m Char => ParsecT s u m String
semi = symbol ";"

comma :: Stream s m Char => ParsecT s u m String
comma = symbol ","

colon :: Stream s m Char => ParsecT s u m String
colon = symbol ":"

strCase :: Stream s m Char => String -> ParsecT s u m String
strCase [] = return []
strCase s@(c:cs) = satisfy (charCaseEq c) >> strCase cs >> return s

{-css21Style :: P.LanguageDef st
css21Style = emptyDef
               { P.commentStart = "/*"
               , P.commentEnd = "*/"
               , P.nestedComments = False
               , P.caseSensitive = False
               }

css21Style :: Stream s m Char => P.GenLanguageDef s u m
css21Style = P.LanguageDef
               { P.commentStart = "/*"
               , P.commentEnd = "*/"
               , P.commentLine = ""
               , P.nestedComments = False
               , P.identStart = asciiAlpha
               , P.identLetter = alphaNum
               , P.opStart = P.opLetter css21Style
               , P.opLetter = oneOf "+>"
               , P.reservedOpNames = []
               , P.reservedNames = []
               , P.caseSensitive = False
               }

lexer :: P.TokenParser ()
lexer :: Stream s m Char => ParsecT s u m String
lexer = P.makeTokenParser css21Style
lexer = P.TokenParser

braces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braces :: ParsecT String () Identity a -> ParsecT String () Identity a
braces = P.braces lexer-}
         
color :: Stream s m Char => ParsecT s u m Color
color = hashColor <|> basicNamedColor

hashColor :: Stream s m Char => ParsecT s u m Color
hashColor = do
    char '#'
    try sixHexDigitColor <|> threeHexDigitColor

sixHexDigitColor :: Stream s m Char => ParsecT s u m Color
sixHexDigitColor = do
    r <- count 2 hexDigit
    g <- count 2 hexDigit
    b <- count 2 hexDigit
    return $ RGBColor (hexToI r) (hexToI g) (hexToI b)

threeHexDigitColor :: Stream s m Char => ParsecT s u m Color
threeHexDigitColor = do
    r <- hexDigit
    g <- hexDigit
    b <- hexDigit
    return $ RGBColor (hexToI (replicate 2 r))
                      (hexToI (replicate 2 g))
                      (hexToI (replicate 2 b))

basicNamedColor :: Stream s m Char => ParsecT s u m Color
basicNamedColor = do
    n <- name
    case (basicNameToColor n) of
      Just c -> return c
      Nothing -> fail "invalid color name"

asciiAlpha :: Stream s m Char => ParsecT s u m Char
asciiAlpha = satisfy isAsciiAlpha

value :: Stream s m Char => ParsecT s u m Value
value = do
    xs <- many1 valueElem
    return (Value xs)

valueElem :: Stream s m Char => ParsecT s u m ValueElem
valueElem =
    choice [ do{ a <- try any; return (VEAny a) }
           , do{ b <- try block; return (VEBlock b) }
           , do{ k <- atKeyword; return (VEAtKeyword k) }
           ]

block :: Stream s m Char => ParsecT s u m Block
block = do
    xs <- between (char '{' >> spaces) (char '}' >> spaces)
                  (many blockElem)
    return (Block xs)

blockElem :: Stream s m Char => ParsecT s u m BlockElem
blockElem = do
    e <- choice [ do{ a <- try any; return (BEAny a) }
                , do{ b <- try block; return (BEBlock b) }
                , do{ k <- atKeyword; return (BEAtKeyword k) }
                ]
    spaces
    return e

any :: Stream s m Char => ParsecT s u m Any
any = do
    a <- choice [ try identifier >>= \i -> return (Ident i)
                , try stringLit >>= \s -> return (CSSString s)
                {-, try percentage-}
                , try dimension
                , try number
                {-, try uri-}
                , try hash
                , try (symbol ":") >>= \s -> return Colon
                ]
    spaces
    return a


identifier :: Stream s m Char => ParsecT s u m String
identifier = lexeme ident

stringLit :: Stream s m Char => ParsecT s u m String
stringLit = lexeme string

atKeyword :: Stream s m Char => ParsecT s u m AtKeyword
atKeyword = do
    char '@'
    i <- ident
    return (AtKeyword i)

hash :: Stream s m Char => ParsecT s u m Any
hash = do
    char '#'
    n <- name
    return (Hash n)

number :: Stream s m Char => ParsecT s u m Any
number = do
    n <- num
    return (Number n)

dimension :: Stream s m Char => ParsecT s u m Any
dimension = do
    n <- num
    i <- ident
    return (Dimension n i)

uri :: Stream s m Char => ParsecT s u m URI
uri = do
    between (S.string "url(" >> spaces) (spaces >> S.string ")")
            uriContent


uriContent :: Stream s m Char => ParsecT s u m String
uriContent = try string
             <|> (many $ choice
                          [ try (satisfy isUnquotedURIContentChar)
                          , try nonascii
                          , escape
                          ]
                 )

lengthVal :: Stream s m Char => ParsecT s u m Length
lengthVal = do
    s <- num
    let n = read s :: Double
    if n == 0
    then optional lengthUnit >> return (Length 0 Nothing)
    else (lengthUnit >>= \u -> return (Length n (Just u)))
         <|> fail "length unit needed after non-zero value"

lengthUnit :: Stream s m Char => ParsecT s u m LengthUnit
lengthUnit = choice 
               [ try (keywordCase "em") >> return Em
               , try (keywordCase "ex") >> return Ex
               , try (keywordCase "in") >> return In
               , try (keywordCase "cm") >> return Cm
               , try (keywordCase "mm") >> return Mm
               , try (keywordCase "pt") >> return Pt
               , try (keywordCase "pc") >> return Pc
               , try (keywordCase "px") >> return Px
               ]

pvWhiteSpace :: Stream s m Char => ParsecT s u m PVWhiteSpace
pvWhiteSpace = choice
    [ try (keywordCase "normal") >> return PVWhiteSpaceNormal
    , try (keywordCase "pre") >> return PVWhiteSpacePre
    , try (keywordCase "nowrap") >> return PVWhiteSpaceNoWrap
    , try (keywordCase "pre-wrap") >> return PVWhiteSpacePreWrap
    , try (keywordCase "pre-line") >> return PVWhiteSpacePreLine
    ,     (keywordCase "inherit") >> return PVWhiteSpaceInherit
    ]

percentage :: Stream s m Char => ParsecT s u m Percentage
percentage = do
    n <- num
    char '%'
    return (Percentage (read n :: Double))

int :: Stream s m Char => ParsecT s u m Int
int = do
    s <- option "" (S.string "-")
    ds <- many1 digit
    return (read (s ++ ds))

important :: Stream s m Char => ParsecT s u m Important
important = option False (symbol "!" >> keyword "important" >> return True)

keyword :: Stream s m Char => String -> ParsecT s u m String
keyword name = do
    n <- S.string name <?> name
    notFollowedBy nmchar <?> ("end of " ++ show name)
    return n

keywordCase :: Stream s m Char => String -> ParsecT s u m String
keywordCase name = do
    n <- strCase name <?> name
    notFollowedBy nmchar <?> ("end of " ++ show name)
    return n

ident :: Stream s m Char => ParsecT s u m String
ident = do
    h <- option "" (S.string "-")
    s <- nmstart
    cs <- many nmchar
    return (h ++ (s:cs))

name :: Stream s m Char => ParsecT s u m String
name = many1 nmchar

nmstart :: Stream s m Char => ParsecT s u m Char
nmstart = satisfy isNmStartChar <|> escape

nonascii :: Stream s m Char => ParsecT s u m Char
nonascii = satisfy isNonAsciiChar

nmchar :: Stream s m Char => ParsecT s u m Char
nmchar = satisfy isNmChar <|> escape

unicode :: Stream s m Char => ParsecT s u m Char
unicode = do
    char '\\'
    ds <- countRange 1 6 hexDigit
    optional (crlf <|> count 1 space)
    return $ chr (hexToI ds)

escape :: Stream s m Char => ParsecT s u m Char
escape = try unicode <|> simpleEscape

simpleEscape :: Stream s m Char => ParsecT s u m Char
simpleEscape = char '\\' >> satisfy isSimpleEscapeChar

crlf :: Stream s m Char => ParsecT s u m String
crlf = S.string "\r\n"

hexToI :: Num a => String -> a
hexToI ds = let ((n,_):_) = readHex ds
            in n

string :: Stream s m Char => ParsecT s u m String
string = string1 <|> string2

badstring :: Stream s m Char => ParsecT s u m String
badstring = badstring1 <|> badstring2

string1 :: Stream s m Char => ParsecT s u m String
string1 = stringSub doubleQuote (stringContent '"') doubleQuote

badstring1 :: Stream s m Char => ParsecT s u m String
badstring1 = stringSub doubleQuote (stringContent '"') (try badStringEnd)

stringContent :: Stream s m Char => Char -> ParsecT s u m String
stringContent quoteChar = choice [ sequence [try escape]
                     , try (char '\\' >> nl)
                     , sequence [satisfy (isPlainStrChar quoteChar)]
                     ]

stringSub :: Stream s m Char => ParsecT s u m String ->
                                ParsecT s u m String ->
                                ParsecT s u m String ->
                                ParsecT s u m String
stringSub start unit end = do
  start
  t <- manyTill unit end
  return (concat t)

doubleQuote :: Stream s m Char => ParsecT s u m String
doubleQuote = S.string "\""

singleQuote :: Stream s m Char => ParsecT s u m String
singleQuote = S.string "'"

badStringEnd :: Stream s m Char => ParsecT s u m String
badStringEnd = S.string "\\?"

string2 :: Stream s m Char => ParsecT s u m String
string2 = stringSub singleQuote (stringContent '\'') singleQuote

badstring2 :: Stream s m Char => ParsecT s u m String
badstring2 = stringSub singleQuote (stringContent '\'') (try badStringEnd)

nl :: Stream s m Char => ParsecT s u m String
nl =   S.string "\n"
   <|> (char '\r' >>= \cr ->
         (try (char '\n') >>= \lf -> return [cr, lf])
         <|> return [cr]
       )
   <|> S.string "\f"

num :: Stream s m Char => ParsecT s u m String
num = try (
        do{ int <- many digit
          ; dot <- char '.'
          ; frac <- many1 digit
          ; return (int ++ dot:frac)
          }
      )
      <|> many1 digit

spaces1 :: Stream s m Char => ParsecT s u m String
spaces1 = many1 space

spaces :: Stream s m Char => ParsecT s u m String
spaces = many space

space :: Stream s m Char => ParsecT s u m Char
space = satisfy isSpaceChar
