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
, positiveIntNoSign
, signedPercentage
, sign
, signedLengthVal
, fontFamilyList
, fontFamily
, fontSVW
, fontVal
, perm0
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
import Text.Parsec.Perm ((<$$>), (<||>), (<$?>), (<|?>), permute)
import SandBox.Text.CSS.Char
import SandBox.Text.CSS.Parsec.Combinator
import SandBox.Text.CSS.Types
import Prelude hiding (any)
import qualified Prelude as R (any)

stylesheet :: Stream s Identity Char => ParsecT s u Identity [Statement]
stylesheet = do
    spaces
    many statement

statement :: Stream s Identity Char => ParsecT s u Identity Statement
statement = atRule <|> ruleSet

atRule :: Stream s Identity Char => ParsecT s u Identity Statement
atRule = do
    k <- atKeyword
    xs <- many (spaces >> any)
    b <- (optionMaybe (try (spaces >> block)) <|> return Nothing)
    return (AtRule k xs b)

atCharset :: Stream s Identity Char => ParsecT s u Identity AtCharset
atCharset = do
    S.string "@charset \""
    n <- encodingName
    S.string "\";"
    return (AtCharset n)

encodingName :: Stream s Identity Char => ParsecT s u Identity EncodingName
encodingName = name

atImport :: Stream s Identity Char => ParsecT s u Identity AtImport
atImport = do
    keywordCase "@import"
    spaces
    u <- (uri <|> string)
    spaces
    ts <- (try mediaTypeList <|> return [])
    spaces
    semi
    return (AtImport u ts)

mediaTypeList :: Stream s Identity Char => ParsecT s u Identity [MediaType]
mediaTypeList = (try (keywordCase "all") >> return [])
                <|>
                (try (sepBy1 mediaType comma))
  where
    mediaType :: Stream s Identity Char => ParsecT s u Identity MediaType
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

atPage :: Stream s Identity Char => ParsecT s u Identity AtPage
atPage = do
    keywordCase "@page"
    spaces
    s <- optionMaybe pageSelector
    spaces
    b <- atPageBlock
    return (AtPage s b)

pageSelector :: Stream s Identity Char => ParsecT s u Identity PageSelector
pageSelector = choice
    [ try (keyword ":first") >> return PSFirst
    , try (keyword ":left") >> return PSLeft
    , try (keyword ":right") >> return PSRight
    ]

atPageBlock :: Stream s Identity Char => ParsecT s u Identity [Declaration]
atPageBlock = braces (sepEndBy (marginDecl <?> "margin declaration") semi)

marginDecl :: Stream s Identity Char => ParsecT s u Identity Declaration
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

declaration :: Stream s Identity Char => ParsecT s u Identity Declaration
declaration = do
    n <- try ident <?> "property name"
    case (M.lookup (map toLower n) declSubMap) of
        Just d -> d
        Nothing -> unexpected ("property name: " ++ show n)

declSub :: Stream s Identity Char =>
               ParsecT s u Identity v -> (v -> Important -> Declaration) ->
               String -> ParsecT s u Identity Declaration
declSub valueParser ctor propName = do
    spaces
    colon
    v <- (valueParser <?> "property value")
         <|> fail ("invalid value for property " ++ show propName)
    spaces
    i <- important
    return (ctor v i)

declSubMap :: Stream s Identity Char => M.Map String (ParsecT s u Identity Declaration)
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
    , ("content", declSub contentVal DeclContent)
    , ("quotes", declSub quotesVal DeclQuotes)
    , ("counter-reset", declSub counterResetVal DeclCounterReset)
    , ("counter-increment", declSub counterIncrementVal DeclCounterIncrement)
    , ("list-style-type", declSub listStyleTypeVal DeclListStyleType)
    , ("list-style-image", declSub listStyleImageVal DeclListStyleImage)
    , ("list-style-position", declSub listStylePositionVal DeclListStylePosition)
    , ("list-style", declSub listStyleVal DeclListStyle)
    , ("page-break-before", declSub pageBreakBeforeVal DeclPageBreakBefore)
    , ("page-break-after", declSub pageBreakAfterVal DeclPageBreakAfter)
    , ("page-break-inside", declSub pageBreakInsideVal DeclPageBreakInside)
    , ("orphans", declSub orphansVal DeclOrphans)
    , ("widows", declSub widowsVal DeclWidows)
    , ("color", declSub colorVal DeclColor)
    , ("background-color", declSub backgroundColorVal DeclBackgroundColor)
    , ("background-image", declSub backgroundImageVal DeclBackgroundImage)
    , ("background-repeat", declSub backgroundRepeatVal DeclBackgroundRepeat)
    , ("background-attachment", declSub backgroundAttachmentVal DeclBackgroundAttachment)
    , ("background-position", declSub backgroundPositionVal DeclBackgroundPosition)
    , ("background", declSub backgroundVal DeclBackground)
    , ("font-family", declSub fontFamilyVal DeclFontFamily)
    , ("font-style", declSub fontStyleVal DeclFontStyle)
    , ("font-variant", declSub fontVariantVal DeclFontVariant)
    , ("font-weight", declSub fontWeightVal DeclFontWeight)
    , ("font-size", declSub fontSizeVal DeclFontSize)
    , ("font", declSub fontVal DeclFont)
    , ("text-align", declSub textAlignVal DeclTextAlign)
    , ("text-decoration", declSub textDecorationVal DeclTextDecoration)
    , ("letter-spacing", declSub letterSpacingVal DeclLetterSpacing)
    , ("word-spacing", declSub wordSpacingVal DeclWordSpacing)
    , ("text-transform", declSub textTransformVal DeclTextTransform)
    , ("white-space", declSub whiteSpaceVal DeclWhiteSpace)
    ]


individualMarginVal :: Stream s Identity Char => ParsecT s u Identity MarginVal
individualMarginVal = choice
    [ try marginWidth >>= \w -> return (MVWidth w)
    , try (keywordCase "inherit") >> return MVInherit
    ]

shortHandMarginVal :: Stream s Identity Char => ParsecT s u Identity [MarginVal]
shortHandMarginVal = choice
    [ try (countRange 1 4 (marginWidth >>= \w -> spaces >> return (MVWidth w)))
    , try (keywordCase "inherit") >> return [MVInherit]
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
marginWidth :: Stream s Identity Char => ParsecT s u Identity MarginWidth
marginWidth = choice
    [ try percentage >>= \p -> return (MWPercentage p)
    , try lengthVal >>= \l -> return (MWLength l)
    , try (keywordCase "auto") >> return MWAuto
    ]


individualPaddingVal :: Stream s Identity Char => ParsecT s u Identity PaddingVal
individualPaddingVal = choice
    [ try paddingWidth >>= \w -> return (PadWidth w)
    , try (keywordCase "inherit") >> return PadInherit
    ]

shortHandPaddingVal :: Stream s Identity Char => ParsecT s u Identity [PaddingVal]
shortHandPaddingVal = choice
    [ try (countRange 1 4 (paddingWidth >>= \w -> spaces >>
                           return (PadWidth w)))
    , try (keywordCase "inherit") >> return [PadInherit]
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
paddingWidth :: Stream s Identity Char => ParsecT s u Identity PaddingWidth
paddingWidth = choice
    [ try percentage >>= \p -> return (PWPercentage p)
    , try lengthVal >>= \l -> return (PWLength l)
    ]



individualBorderWidthVal :: Stream s Identity Char => ParsecT s u Identity BorderWidthVal
individualBorderWidthVal = choice
    [ try borderWidth >>= \w -> return (BWVWidth w)
    , try (keywordCase "inherit") >> return BWVInherit
    ]

shortHandBorderWidthVal :: Stream s Identity Char => ParsecT s u Identity [BorderWidthVal]
shortHandBorderWidthVal = choice
    [ try (countRange 1 4 (borderWidth >>= \w -> spaces >>
                           return (BWVWidth w)))
    , try (keywordCase "inherit") >> return [BWVInherit]
    ]

borderWidth :: Stream s Identity Char => ParsecT s u Identity BorderWidth
borderWidth = choice
    [ try (keywordCase "thin") >> return BWThin
    , try (keywordCase "medium") >> return BWMedium
    , try (keywordCase "thick") >> return BWThick
    , try lengthVal >>= \l -> return (BWLength l)
    ]


individualBorderColorVal :: Stream s Identity Char => ParsecT s u Identity BorderColorVal
individualBorderColorVal = choice
    [ try borderColor >>= \w -> return (BCVColor w)
    , try (keywordCase "inherit") >> return BCVInherit
    ]

shortHandBorderColorVal :: Stream s Identity Char => ParsecT s u Identity [BorderColorVal]
shortHandBorderColorVal = choice
    [ try (countRange 1 4 (borderColor >>= \w -> spaces >>
                           return (BCVColor w)))
    , try (keywordCase "inherit") >> return [BCVInherit]
    ]

borderColor :: Stream s Identity Char => ParsecT s u Identity BorderColor
borderColor = choice
    [ try color >>= \c -> return (BCColor c)
    , try (keywordCase "transparent") >> return BCTransparent
    ]


individualBorderStyleVal :: Stream s Identity Char => ParsecT s u Identity BorderStyleVal
individualBorderStyleVal = choice
    [ try borderStyle >>= \w -> return (BSVStyle w)
    , try (keywordCase "inherit") >> return BSVInherit
    ]

shortHandBorderStyleVal :: Stream s Identity Char => ParsecT s u Identity [BorderStyleVal]
shortHandBorderStyleVal = choice
    [ try (countRange 1 4 (borderStyle >>= \w -> spaces >>
                           return (BSVStyle w)))
    , try (keywordCase "inherit") >> return [BSVInherit]
    ]

borderStyle :: Stream s Identity Char => ParsecT s u Identity BorderStyle
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


borderVal :: Stream s Identity Char => ParsecT s u Identity BorderVal
borderVal = choice
    [ try borderValElems >>= \vs -> return (BVBorder vs)
    , try (keywordCase "inherit") >> return BVInherit
    ]

borderValElems :: Stream s Identity Char => ParsecT s u Identity [BorderValElem]
borderValElems = oneOrMoreInAnyOrder
    [ try borderWidth >>= \w -> spaces >> return (BVEWidth w)
    , try borderStyle >>= \s -> spaces >> return (BVEStyle s)
    , try borderColor >>= \c -> spaces >> return (BVEColor c)
    ]

displayVal :: Stream s Identity Char => ParsecT s u Identity DisplayVal
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


positionVal :: Stream s Identity Char => ParsecT s u Identity PositionVal
positionVal = choice
    [ try (keywordCase "static") >> return PosStatic
    , try (keywordCase "relative") >> return PosRelative
    , try (keywordCase "absolute") >> return PosAbsolute
    , try (keywordCase "fixed") >> return PosFixed
    , try (keywordCase "inherit") >> return PosInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
boxOffsetVal :: Stream s Identity Char => ParsecT s u Identity BoxOffsetVal
boxOffsetVal = choice
    [ try percentage >>= \p -> return (BOVPercentage p)
    , try lengthVal >>= \l -> return (BOVLength l)
    , try (keywordCase "auto") >> return BOVAuto
    , try (keywordCase "inherit") >> return BOVInherit
    ]

floatVal :: Stream s Identity Char => ParsecT s u Identity FloatVal
floatVal = choice
    [ try (keywordCase "left") >> return FlVLeft
    , try (keywordCase "right") >> return FlVRight
    , try (keywordCase "none") >> return FlVNone
    , try (keywordCase "inherit") >> return FlVInherit
    ]

clearVal :: Stream s Identity Char => ParsecT s u Identity ClearVal
clearVal = choice
    [ try (keywordCase "none") >> return CleNone
    , try (keywordCase "left") >> return CleLeft
    , try (keywordCase "right") >> return CleRight
    , try (keywordCase "both") >> return CleBoth
    , try (keywordCase "inherit") >> return CleInherit
    ]

zIndexVal :: Stream s Identity Char => ParsecT s u Identity ZIndexVal
zIndexVal = choice
    [ try (keywordCase "auto") >> return ZIndAuto
    , try integer >>= \n -> return (ZIndInt n)
    , try (keywordCase "inherit") >> return ZIndInherit
    ]

directionVal :: Stream s Identity Char => ParsecT s u Identity DirectionVal
directionVal = choice
    [ try (keywordCase "ltr") >> return DirLtr
    , try (keywordCase "rtl") >> return DirRtl
    , try (keywordCase "inherit") >> return DirInherit
    ]

unicodeBidiVal :: Stream s Identity Char => ParsecT s u Identity UnicodeBidiVal
unicodeBidiVal = choice
    [ try (keywordCase "normal") >> return UBdNormal
    , try (keywordCase "embed") >> return UBdEmbed
    , try (keywordCase "bidi-override") >> return UBdBidiOverride
    , try (keywordCase "inherit") >> return UBdInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
widthVal :: Stream s Identity Char => ParsecT s u Identity WidthVal
widthVal = choice
    [ try percentage >>= \p -> return (WidPercentage p)
    , try lengthVal >>= \l -> return (WidLength l)
    , try (keywordCase "auto") >> return WidAuto
    , try (keywordCase "inherit") >> return WidInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
minWidthVal :: Stream s Identity Char => ParsecT s u Identity MinWidthVal
minWidthVal = choice
    [ try percentage >>= \p -> return (MinWidPercentage p)
    , try lengthVal >>= \l -> return (MinWidLength l)
    , try (keywordCase "inherit") >> return MinWidInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
maxWidthVal :: Stream s Identity Char => ParsecT s u Identity MaxWidthVal
maxWidthVal = choice
    [ try percentage >>= \p -> return (MaxWidPercentage p)
    , try lengthVal >>= \l -> return (MaxWidLength l)
    , try (keywordCase "none") >> return MaxWidNone
    , try (keywordCase "inherit") >> return MaxWidInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
heightVal :: Stream s Identity Char => ParsecT s u Identity HeightVal
heightVal = choice
    [ try percentage >>= \p -> return (HeiPercentage p)
    , try lengthVal >>= \l -> return (HeiLength l)
    , try (keywordCase "auto") >> return HeiAuto
    , try (keywordCase "inherit") >> return HeiInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
minHeightVal :: Stream s Identity Char => ParsecT s u Identity MinHeightVal
minHeightVal = choice
    [ try percentage >>= \p -> return (MinHeiPercentage p)
    , try lengthVal >>= \l -> return (MinHeiLength l)
    , try (keywordCase "inherit") >> return MinHeiInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
maxHeightVal :: Stream s Identity Char => ParsecT s u Identity MaxHeightVal
maxHeightVal = choice
    [ try percentage >>= \p -> return (MaxHeiPercentage p)
    , try lengthVal >>= \l -> return (MaxHeiLength l)
    , try (keywordCase "none") >> return MaxHeiNone
    , try (keywordCase "inherit") >> return MaxHeiInherit
    ]

lineHeightVal :: Stream s Identity Char => ParsecT s u Identity LineHeightVal
lineHeightVal = choice
    [ try lineHeight >>= \h -> return (LHVVal h)
    , try (keywordCase "inherit") >> return LHVInherit
    ]

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
lineHeight :: Stream s Identity Char => ParsecT s u Identity LineHeight
lineHeight = choice
    [ try (keywordCase "normal") >> return LHNormal
    , try percentage >>= \p -> return (LHPercentage p)
    , try lengthVal >>= \l -> return (LHLength l)
    , try nonNegativeNumber >>= \n -> return (LHNumber n)
    ]

nonNegativeNumber :: Stream s Identity Char => ParsecT s u Identity Double
nonNegativeNumber = num >>= \s -> return (read s)

{- NOTE: try percentage before lengthVal for "0%" to be parsed as percentage. -}
verticalAlignVal :: Stream s Identity Char => ParsecT s u Identity VerticalAlignVal
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

overflowVal :: Stream s Identity Char => ParsecT s u Identity OverflowVal
overflowVal = choice
    [ try (keywordCase "visible") >> return OveVisible
    , try (keywordCase "hidden") >> return OveHidden
    , try (keywordCase "scroll") >> return OveScroll
    , try (keywordCase "auto") >> return OveAuto
    , try (keywordCase "inherit") >> return OveInherit
    ]

clipVal :: Stream s Identity Char => ParsecT s u Identity ClipVal
clipVal = choice
    [ try clipShape
    , try (keywordCase "auto") >> return CliAuto
    , try (keywordCase "inherit") >> return CliInherit
    ]

clipShape :: Stream s Identity Char => ParsecT s u Identity ClipVal
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

clipOffset :: Stream s Identity Char => ParsecT s u Identity ClipOffset
clipOffset = choice
    [ try lengthVal >>= \l -> return (CliOffLength l)
    , try (keywordCase "auto") >> return CliOffAuto
    ]

visibilityVal :: Stream s Identity Char => ParsecT s u Identity VisibilityVal
visibilityVal = choice
    [ try (keywordCase "visible") >> return VisVisible
    , try (keywordCase "hidden") >> return VisHidden
    , try (keywordCase "collapse") >> return VisCollapse
    , try (keywordCase "inherit") >> return VisInherit
    ]

contentVal :: Stream s Identity Char => ParsecT s u Identity ContentVal
contentVal = choice
    [ try (keywordCase "normal") >> return ConNormal
    , try (keywordCase "none") >> return ConNone
    , try contentValElems >>= \vs -> return (ConValues vs)
    , try (keywordCase "inherit") >> return ConInherit
    ]

contentValElems :: Stream s Identity Char => ParsecT s u Identity [ContentValElem]
contentValElems = many1 (spaces >> contentValElem)

contentValElem :: Stream s Identity Char => ParsecT s u Identity ContentValElem
contentValElem = choice
    [ try stringLit >>= \s -> return (CVEString s)
    , try uri >>= \u -> return (CVEURI u)
    , try counter >>= \c -> return (CVECounter c)
    , try contentValAttr
    , try (keywordCase "open-quote") >> return CVEOpenQuote
    , try (keywordCase "close-quote") >> return CVECloseQuote
    , try (keywordCase "no-open-quote") >> return CVENoOpenQuote
    , try (keywordCase "no-close-quote") >> return CVENoCloseQuote
    ]

contentValAttr :: Stream s Identity Char => ParsecT s u Identity ContentValElem
contentValAttr = do
    id <- between (symbol "attr(") (symbol ")") identifier
    return (CVEAttr id)

counter :: Stream s Identity Char => ParsecT s u Identity Counter
counter = choice
    [ try (between (symbol "counter(") (symbol ")")
              (identifier >>= \id ->
               optionMaybe (comma >> listStyleType) >>= \t ->
               return (Counter id t)))
    , try (between (symbol "counters(") (symbol ")")
              (identifier >>= \id ->
               stringLit >>= \s ->
               optionMaybe (comma >> listStyleType) >>= \t ->
               return (Counters id s t)))
    ]

listStyleTypeVal :: Stream s Identity Char => ParsecT s u Identity ListStyleTypeVal
listStyleTypeVal = choice
    [ try listStyleType >>= \t -> return (LSTVType t)
    , try (keywordCase "inherit") >> return LSTVInherit
    ]

listStyleType :: Stream s Identity Char => ParsecT s u Identity ListStyleType
listStyleType = choice
    [ try (keywordCase "disc") >> return LSTDisc
    , try (keywordCase "circle") >> return LSTCircle
    , try (keywordCase "square") >> return LSTSquare
    , try (keywordCase "decimal") >> return LSTDecimal
    , try (keywordCase "decimal-leading-zero") >> return LSTDecimalLeadingZero
    , try (keywordCase "lower-roman") >> return LSTLowerRoman
    , try (keywordCase "upper-roman") >> return LSTUpperRoman
    , try (keywordCase "lower-greek") >> return LSTLowerGreek
    , try (keywordCase "lower-latin") >> return LSTLowerLatin
    , try (keywordCase "armenian") >> return LSTArmenian
    , try (keywordCase "georgian") >> return LSTGeorgian
    , try (keywordCase "lower-alpha") >> return LSTLowerAlpha
    , try (keywordCase "upper-alpha") >> return LSTUpperAlpha
    , try (keywordCase "none") >> return LSTNone
    ]

listStyleImageVal :: Stream s Identity Char => ParsecT s u Identity ListStyleImageVal
listStyleImageVal = choice
    [ try listStyleImage >>= \i -> return (LSIVImage i)
    , try (keywordCase "inherit") >> return LSIVInherit
    ]

listStyleImage :: Stream s Identity Char => ParsecT s u Identity ListStyleImage
listStyleImage = choice
    [ try uri >>= \u -> return (LSIURI u)
    , try (keywordCase "none") >> return LSINone
    ]

listStylePositionVal :: Stream s Identity Char => ParsecT s u Identity ListStylePositionVal
listStylePositionVal = choice
    [ try listStylePosition >>= \i -> return (LSPVPosition i)
    , try (keywordCase "inherit") >> return LSPVInherit
    ]

listStylePosition :: Stream s Identity Char => ParsecT s u Identity ListStylePosition
listStylePosition = choice
    [ try (keywordCase "inside") >> return LSPInside
    , try (keywordCase "outside") >> return LSPOutside
    ]

listStyleVal :: Stream s Identity Char => ParsecT s u Identity ListStyleVal
listStyleVal = choice
    [ try listStyleValElems >>= \vs -> return (LSVValues vs)
    , try (keywordCase "inherit") >> return LSVInherit
    ]

listStyleValElems :: Stream s Identity Char => ParsecT s u Identity [ListStyleValElem]
listStyleValElems = oneOrMoreInAnyOrder
    [ try listStyleType >>= \t -> spaces >> return (LSVEType t)
    , try listStylePosition >>= \p -> spaces >> return (LSVEPosition p)
    , try listStyleImage >>= \i -> spaces >> return (LSVEImage i)
    ]


quotesVal :: Stream s Identity Char => ParsecT s u Identity QuotesVal
quotesVal = choice
    [ try quotePairs >>= \xs -> return (QVQuotePairs xs)
    , try (keywordCase "none") >> return QVNone
    , try (keywordCase "inherit") >> return QVInherit
    ]

quotePairs :: Stream s Identity Char => ParsecT s u Identity [QuotePair]
quotePairs = many1 quotePair

quotePair :: Stream s Identity Char => ParsecT s u Identity QuotePair
quotePair = do
    open <- stringLit
    close <- stringLit
    return (open, close)

counterResetVal :: Stream s Identity Char => ParsecT s u Identity CounterResetVal
counterResetVal = choice
    [ try counterIdAndInts >>= \xs -> return (CRVCounters xs)
    , try (keywordCase "none") >> return CRVNone
    , try (keywordCase "inherit") >> return CRVInherit
    ]

counterIncrementVal :: Stream s Identity Char => ParsecT s u Identity CounterIncrementVal
counterIncrementVal = choice
    [ try counterIdAndInts >>= \xs -> return (CIVCounters xs)
    , try (keywordCase "none") >> return CIVNone
    , try (keywordCase "inherit") >> return CIVInherit
    ]

counterIdAndInts :: Stream s Identity Char => ParsecT s u Identity [(Id, (Maybe Int))]
counterIdAndInts = many1 counterIdAndInt

counterIdAndInt :: Stream s Identity Char => ParsecT s u Identity (Id, (Maybe Int))
counterIdAndInt = do
    id <- identifier
    i <- optionMaybe integer
    spaces
    return (id, i)

pageBreakBeforeVal :: Stream s Identity Char => ParsecT s u Identity PageBreakBeforeVal
pageBreakBeforeVal = choice
    [ try (keywordCase "auto") >> return PBBVAuto
    , try (keywordCase "always") >> return PBBVAlways
    , try (keywordCase "avoid") >> return PBBVAvoid
    , try (keywordCase "left") >> return PBBVLeft
    , try (keywordCase "right") >> return PBBVRight
    , try (keywordCase "inherit") >> return PBBVInherit
    ]

pageBreakAfterVal :: Stream s Identity Char => ParsecT s u Identity PageBreakAfterVal
pageBreakAfterVal = choice
    [ try (keywordCase "auto") >> return PBAVAuto
    , try (keywordCase "always") >> return PBAVAlways
    , try (keywordCase "avoid") >> return PBAVAvoid
    , try (keywordCase "left") >> return PBAVLeft
    , try (keywordCase "right") >> return PBAVRight
    , try (keywordCase "inherit") >> return PBAVInherit
    ]

pageBreakInsideVal :: Stream s Identity Char => ParsecT s u Identity PageBreakInsideVal
pageBreakInsideVal = choice
    [ try (keywordCase "avoid") >> return PBIVAvoid
    , try (keywordCase "auto") >> return PBIVAuto
    , try (keywordCase "inherit") >> return PBIVInherit
    ]

orphansVal :: Stream s Identity Char => ParsecT s u Identity OrphansVal
orphansVal = choice
    [ try positiveIntNoSign >>= \n -> return (OVInt n)
    , try (keywordCase "inherit") >> return OVInherit
    ]

widowsVal :: Stream s Identity Char => ParsecT s u Identity WidowsVal
widowsVal = choice
    [ try positiveIntNoSign >>= \n -> return (WVInt n)
    , try (keywordCase "inherit") >> return WVInherit
    ]

colorVal :: Stream s Identity Char => ParsecT s u Identity ColorVal
colorVal = choice
    [ try color >>= \c -> return (CVColor c)
    , try (keywordCase "inherit") >> return CVInherit
    ]

backgroundColorVal :: Stream s Identity Char => ParsecT s u Identity BackgroundColorVal
backgroundColorVal = choice
    [ try backgroundColor
    , try (keywordCase "inherit") >> return BgCVInherit
    ]

backgroundColor :: Stream s Identity Char => ParsecT s u Identity BackgroundColorVal
backgroundColor = choice
    [ try color >>= \c -> return (BgCVColor c)
    , try (keywordCase "transparent") >> return BgCVTransparent
    ]

backgroundImageVal :: Stream s Identity Char => ParsecT s u Identity BackgroundImageVal
backgroundImageVal = choice
    [ try backgroundImage
    , try (keywordCase "inherit") >> return BgIVInherit
    ]

backgroundImage :: Stream s Identity Char => ParsecT s u Identity BackgroundImageVal
backgroundImage = choice
    [ try uri >>= \u -> return (BgIVURI u)
    , try (keywordCase "none") >> return BgIVNone
    ]

backgroundRepeatVal :: Stream s Identity Char => ParsecT s u Identity BackgroundRepeatVal
backgroundRepeatVal = choice
    [ try backgroundRepeat
    , try (keywordCase "inherit") >> return BgRVInherit
    ]

backgroundRepeat :: Stream s Identity Char => ParsecT s u Identity BackgroundRepeatVal
backgroundRepeat = choice
    [ try (keywordCase "repeat") >> return BgRVRepeat
    , try (keywordCase "repeat-x") >> return BgRVRepeatX
    , try (keywordCase "repeat-y") >> return BgRVRepeatY
    , try (keywordCase "no-repeat") >> return BgRVNoRepeat
    ]

backgroundAttachmentVal :: Stream s Identity Char => ParsecT s u Identity BackgroundAttachmentVal
backgroundAttachmentVal = choice
    [ try backgroundAttachment
    , try (keywordCase "inherit") >> return BgAVInherit
    ]

backgroundAttachment :: Stream s Identity Char => ParsecT s u Identity BackgroundAttachmentVal
backgroundAttachment = choice
    [ try (keywordCase "scroll") >> return BgAVScroll
    , try (keywordCase "fixed") >> return BgAVFixed
    ]

backgroundPositionVal :: Stream s Identity Char => ParsecT s u Identity BackgroundPositionVal
backgroundPositionVal = choice
    [ try backgroundPosition
    , try (keywordCase "inherit") >> return BgPVInherit
    ]

backgroundPosition :: Stream s Identity Char => ParsecT s u Identity BackgroundPositionVal
backgroundPosition = choice
    [ try signedPercentage >>= \p -> spaces >> withHPos (HPosPercentage p)
    , try signedLengthVal >>= \l -> spaces >> withHPos (HPosLength l)
    , try (symbol "left") >> withHPos HPosLeft
    , try (symbol "center") >> withCenter
    , try (symbol "right") >> withHPos HPosRight
    , try (symbol "top") >> withVPos VPosTop
    , try (symbol "bottom") >> withVPos VPosBottom
    , try (keywordCase "inherit") >> return BgPVInherit
    ]
  where
    withHPos :: Stream s Identity Char => HorizPos -> ParsecT s u Identity BackgroundPositionVal
    withHPos hPos = choice
        [ try signedPercentage >>= \p ->
              return (BgPVPos hPos (VPosPercentage p))
        , try signedLengthVal >>= \l ->
              return (BgPVPos hPos (VPosLength l))
        , try (symbol "top") >>
              return (BgPVPos hPos VPosTop)
        , try (symbol "center") >>
              return (BgPVPos hPos VPosCenter)
        , try (symbol "bottom") >>
              return (BgPVPos hPos VPosBottom)
        , return (BgPVPos hPos VPosCenter)
        ]
    withVPos :: Stream s Identity Char => VertPos -> ParsecT s u Identity BackgroundPositionVal
    withVPos vPos = choice
        [ try signedPercentage >>= \p ->
              return (BgPVPos (HPosPercentage p) vPos)
        , try signedLengthVal >>= \l ->
              return (BgPVPos (HPosLength l) vPos)
        , try (symbol "left") >>
              return (BgPVPos HPosLeft vPos)
        , try (symbol "center") >>
              return (BgPVPos HPosCenter vPos)
        , try (symbol "right") >>
              return (BgPVPos HPosRight vPos)
        , return (BgPVPos HPosCenter vPos)
        ]
    withCenter :: Stream s Identity Char => ParsecT s u Identity BackgroundPositionVal
    withCenter = choice
        [ try signedPercentage >>= \p ->
              return (BgPVPos HPosCenter (VPosPercentage p))
        , try signedLengthVal >>= \l ->
              return (BgPVPos HPosCenter (VPosLength l))
        , try (symbol "top") >>
              return (BgPVPos HPosCenter VPosTop)
        , try (symbol "center") >>
              return (BgPVPos HPosCenter VPosCenter)
        , try (symbol "bottom") >>
              return (BgPVPos HPosCenter VPosBottom)
        , try (symbol "left") >>
              return (BgPVPos HPosLeft VPosCenter)
        , try (symbol "right") >>
              return (BgPVPos HPosRight VPosCenter)
        , return (BgPVPos HPosCenter VPosCenter)
        ]

backgroundVal :: Stream s Identity Char => ParsecT s u Identity BackgroundVal
backgroundVal = choice
    [ try backgroundValElems >>= \vs -> return (BgVValues vs)
    , try (keywordCase "inherit") >> return BgVInherit
    ]

backgroundValElems :: Stream s Identity Char => ParsecT s u Identity [BackgroundValElem]
backgroundValElems = oneOrMoreInAnyOrder
    [ try backgroundColor >>= \c -> spaces >> return (BgVEColor c)
    , try backgroundImage >>= \i -> spaces >> return (BgVEImage i)
    , try backgroundRepeat >>= \r -> spaces >> return (BgVERepeat r)
    , try backgroundAttachment >>= \a -> spaces >> return (BgVEAttachment a)
    , try backgroundPosition >>= \p -> spaces >> return (BgVEPosition p)
    ]

{- NOTE: Order is significant. We must try 'inherit' first. -}
fontFamilyVal :: Stream s Identity Char => ParsecT s u Identity FontFamilyVal
fontFamilyVal = choice
    [ try (keywordCase "inherit") >> return FFVInherit
    , try fontFamilyList >>= \xs -> return (FFVValues xs)
    ]

fontFamilyList :: Stream s Identity Char => ParsecT s u Identity [FontFamily]
fontFamilyList = sepBy1 (try fontFamily) comma

fontFamily :: Stream s Identity Char => ParsecT s u Identity FontFamily
fontFamily = choice
    [ try genericFamily >>= \f -> return (FFGeneric f)
    , try familyName >>= \n -> return (FFName n)
    ]

genericFamily :: Stream s Identity Char => ParsecT s u Identity GenericFamily
genericFamily = choice
    [ try (keywordCase "serif") >> return Serif
    , try (keywordCase "sans-serif") >> return SansSerif
    , try (keywordCase "cursive") >> return Cursive
    , try (keywordCase "fantasy") >> return Fantasy
    , try (keywordCase "monospace") >> return Monospace
    ]

familyName :: Stream s Identity Char => ParsecT s u Identity String
familyName = choice
    [ try identifier
    , try stringLit
    ]

fontStyleVal :: Stream s Identity Char => ParsecT s u Identity FontStyleVal
fontStyleVal = choice
    [ try fontStyle >>= \s -> return (FStVVal s)
    , try (keywordCase "inherit") >> return FStVInherit
    ]

fontStyle :: Stream s Identity Char => ParsecT s u Identity FontStyle
fontStyle = choice
    [ try (keywordCase "normal") >> return FStNormal
    , try (keywordCase "italic") >> return FStItalic
    , try (keywordCase "oblique") >> return FStOblique
    ]

fontVariantVal :: Stream s Identity Char => ParsecT s u Identity FontVariantVal
fontVariantVal = choice
    [ try fontVariant >>= \s -> return (FVVVal s)
    , try (keywordCase "inherit") >> return FVVInherit
    ]

fontVariant :: Stream s Identity Char => ParsecT s u Identity FontVariant
fontVariant = choice
    [ try (keywordCase "normal") >> return FVNormal
    , try (keywordCase "small-caps") >> return FVSmallCaps
    ]

fontWeightVal :: Stream s Identity Char => ParsecT s u Identity FontWeightVal
fontWeightVal = choice
    [ try fontWeight >>= \s -> return (FWVVal s)
    , try (keywordCase "inherit") >> return FWVInherit
    ]

fontWeight :: Stream s Identity Char => ParsecT s u Identity FontWeight
fontWeight = choice
    [ try (keywordCase "normal") >> return FWNormal
    , try (keywordCase "bold") >> return FWBold
    , try (keywordCase "bolder") >> return FWBolder
    , try (keywordCase "lighter") >> return FWLighter
    , try (keyword "100") >> return FW100
    , try (keyword "200") >> return FW200
    , try (keyword "300") >> return FW300
    , try (keyword "400") >> return FW400
    , try (keyword "500") >> return FW500
    , try (keyword "600") >> return FW600
    , try (keyword "700") >> return FW700
    , try (keyword "800") >> return FW800
    , try (keyword "900") >> return FW900
    ]

fontSizeVal :: Stream s Identity Char => ParsecT s u Identity FontSizeVal
fontSizeVal = choice
    [ try fontSize >>= \s -> return (FSVVal s)
    , try (keywordCase "inherit") >> return FSVInherit
    ]

fontSize :: Stream s Identity Char => ParsecT s u Identity FontSize
fontSize = choice
    [ try absoluteSize  >>= \s -> return (FSAbs s)
    , try relativeSize  >>= \s -> return (FSRel s)
    , try percentage  >>= \s -> return (FSPercentage s)
    , try lengthVal  >>= \s -> return (FSLength s)
    ]

absoluteSize :: Stream s Identity Char => ParsecT s u Identity AbsoluteSize
absoluteSize = choice
    [ try (keywordCase "xx-small") >> return ASXXSmall
    , try (keywordCase "x-small") >> return ASXSmall
    , try (keywordCase "small") >> return ASSmall
    , try (keywordCase "medium") >> return ASMedium
    , try (keywordCase "large") >> return ASLarge
    , try (keywordCase "x-large") >> return ASXLarge
    , try (keywordCase "xx-large") >> return ASXXLarge
    ]

relativeSize :: Stream s Identity Char => ParsecT s u Identity RelativeSize
relativeSize = choice
    [ try (keywordCase "larger") >> return RSLarger
    , try (keywordCase "smaller") >> return RSSmaller
    ]

fontVal :: Stream s Identity Char => ParsecT s u Identity FontVal
fontVal = choice
    [ try (keywordCase "caption") >> return FVCaption
    , try (keywordCase "icon") >> return FVIcon
    , try (keywordCase "menu") >> return FVMenu
    , try (keywordCase "message-box") >> return FVMessageBox
    , try (keywordCase "small-caption") >> return FVSmallCaption
    , try (keywordCase "status-bar") >> return FVStatusBar
    , try (keywordCase "inherit") >> return FVInherit
    , try shorthand
    ]
  where
    shorthand :: Stream s Identity Char => ParsecT s u Identity FontVal
    shorthand = do
        (st, var, wei) <- fontSVW
        sz <- fontSize
        spaces
        lh <- optionMaybe (symbol "/" >> lineHeight)
        spaces
        ffs <- fontFamilyList
        return (FVVal st var wei sz lh ffs)

fontSVW :: Stream s Identity Char => ParsecT s u Identity (Maybe FontStyle, Maybe FontVariant, Maybe FontWeight)
fontSVW = permute (
            (,,)
            <$?> (Nothing, fontStyle >>= \s -> spaces >> return (Just s))
            <|?> (Nothing, fontVariant >>= \v -> spaces >> return (Just v))
            <|?> (Nothing, fontWeight >>= \w -> spaces >> return (Just w))
          )

textIndentVal :: Stream s Identity Char => ParsecT s u Identity TextIndentVal
textIndentVal = choice
    [ try textIndent >>= \s -> return (TIVVal s)
    , try (keywordCase "inherit") >> return TIVInherit
    ]

textIndent :: Stream s Identity Char => ParsecT s u Identity TextIndent
textIndent = choice
    [ try percentage >>= \p -> return (TIPercentage p)
    , try lengthVal >>= \l -> return (TILength l)
    ]

textAlignVal :: Stream s Identity Char => ParsecT s u Identity TextAlignVal
textAlignVal = choice
    [ try textAlign >>= \s -> return (TAVVal s)
    , try (keywordCase "inherit") >> return TAVInherit
    ]

textAlign :: Stream s Identity Char => ParsecT s u Identity TextAlign
textAlign = choice
    [ try (keywordCase "left") >> return TALeft
    , try (keywordCase "right") >> return TARight
    , try (keywordCase "center") >> return TACenter
    , try (keywordCase "justify") >> return TAJustify
    ]

textDecorationVal :: Stream s Identity Char => ParsecT s u Identity TextDecorationVal
textDecorationVal = choice
    [ try textDecoration >>= \s -> return (TDVVal s)
    , try (keywordCase "inherit") >> return TDVInherit
    ]

textDecoration :: Stream s Identity Char => ParsecT s u Identity TextDecoration
textDecoration = choice
    [ try (keywordCase "none") >> return TDNone
    , try textDecTypes >>= \ts -> return (TDValues ts)
    ]

textDecTypes :: Stream s Identity Char => ParsecT s u Identity [TextDecType]
textDecTypes = oneOrMoreInAnyOrder
    [ try (keywordCase "underline") >> spaces >> return TDTUnderline
    , try (keywordCase "overline") >> spaces >> return TDTOverline
    , try (keywordCase "line-through") >> spaces >> return TDTLineThrough
    , try (keywordCase "blink") >> spaces >> return TDTBlink
    ]

letterSpacingVal :: Stream s Identity Char => ParsecT s u Identity LetterSpacingVal
letterSpacingVal = choice
    [ try letterSpacing >>= \s -> return (LSpVVal s)
    , try (keywordCase "inherit") >> return LSpVInherit
    ]

letterSpacing :: Stream s Identity Char => ParsecT s u Identity LetterSpacing
letterSpacing = choice
    [ try (keywordCase "normal") >> return LSpNormal
    , try lengthVal >>= \l -> return (LSpLength l)
    ]

wordSpacingVal :: Stream s Identity Char => ParsecT s u Identity WordSpacingVal
wordSpacingVal = choice
    [ try wordSpacing >>= \s -> return (WSpVVal s)
    , try (keywordCase "inherit") >> return WSpVInherit
    ]

wordSpacing :: Stream s Identity Char => ParsecT s u Identity WordSpacing
wordSpacing = choice
    [ try (keywordCase "normal") >> return WSpNormal
    , try lengthVal >>= \l -> return (WSpLength l)
    ]

textTransformVal :: Stream s Identity Char => ParsecT s u Identity TextTransformVal
textTransformVal = choice
    [ try textTransform >>= \s -> return (TTVVal s)
    , try (keywordCase "inherit") >> return TTVInherit
    ]

textTransform :: Stream s Identity Char => ParsecT s u Identity TextTransform
textTransform = choice
    [ try (keywordCase "capitalize") >> return TTCapitalize
    , try (keywordCase "uppercase") >> return TTUppercase
    , try (keywordCase "lowercase") >> return TTLowercase
    , try (keywordCase "none") >> return TTNone
    ]

whiteSpaceVal :: Stream s Identity Char => ParsecT s u Identity WhiteSpaceVal
whiteSpaceVal = choice
    [ try whiteSpace >>= \s -> return (WhSVVal s)
    , try (keywordCase "inherit") >> return WhSVInherit
    ]

whiteSpace :: Stream s Identity Char => ParsecT s u Identity WhiteSpace
whiteSpace = choice
    [ try (keywordCase "normal") >> return WhSNormal
    , try (keywordCase "pre") >> return WhSPre
    , try (keywordCase "nowrap") >> return WhSNowrap
    , try (keywordCase "pre-wrap") >> return WhSPreWrap
    , try (keywordCase "pre-line") >> return WhSPreLine
    ]

atMedia :: Stream s Identity Char => ParsecT s u Identity AtMedia
atMedia = do
    keywordCase "@media"
    spaces
    ts <- mediaTypeList
    spaces
    rs <- braces (many ruleSet)
    return (AtMedia ts rs)

ruleSet :: Stream s Identity Char => ParsecT s u Identity Statement
ruleSet = do
    ss <- selectorList
    b <- declarationBlock
    return (RuleSet ss b)

selectorList :: Stream s Identity Char => ParsecT s u Identity [Selector]
selectorList = sepBy selector comma

selector :: Stream s Identity Char => ParsecT s u Identity Selector
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

simpleSel :: Stream s Identity Char => ParsecT s u Identity SimpleSel
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

composeSel :: Stream s Identity Char =>
              (Selector -> Selector -> Selector) -> SimpleSel -> Selector ->
              ParsecT s u Identity Selector
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
typeSel :: Stream s Identity Char => ParsecT s u Identity String
typeSel = name

{- NOTE: univSel is not lexeme parser. -}
univSel :: Stream s Identity Char => ParsecT s u Identity String
univSel = S.string "*"

subSel :: Stream s Identity Char => ParsecT s u Identity SubSel
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

attrSel :: Stream s Identity Char => ParsecT s u Identity AttrSel
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

attrVal :: Stream s Identity Char => ParsecT s u Identity String
attrVal =   identifier
        <|> stringLit

{- Note: No space is allow after '.'. -}
classSel :: Stream s Identity Char => ParsecT s u Identity SubSel
classSel = do
    char '.'
    n <- name
    return (ClassSel n)

{- Note: No space is allowed after '#'. -}
idSel :: Stream s Identity Char => ParsecT s u Identity SubSel
idSel = do
    char '#'
    i <- ident
    return (IdSel i)

pseudoClassSel :: Stream s Identity Char => ParsecT s u Identity PseudoClassSel
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

pseudoElementSel :: Stream s Identity Char => ParsecT s u Identity PseudoElementSel
pseudoElementSel = choice
    [ try (keywordCase ":first-line") >> return PESFirstLine
    , try (keywordCase ":first-letter") >> return PESFirstLetter
    , try (keywordCase ":before") >> return PESBefore
    , try (keywordCase ":after") >> return PESAfter
    ]

declarationBlock :: Stream s Identity Char => ParsecT s u Identity [Declaration]
declarationBlock =
    spaces >> braces (sepEndBy (declaration <?> "declaration") semi)

parens :: Stream s Identity Char => ParsecT s u Identity a -> ParsecT s u Identity a
parens = between (symbol "(") (symbol ")")

braces :: Stream s Identity Char => ParsecT s u Identity a -> ParsecT s u Identity a
braces = between (symbol "{") (symbol "}")

lexeme :: Stream s Identity Char => ParsecT s u Identity a -> ParsecT s u Identity a
lexeme p = do { x <- p; spaces; return x }

symbol :: Stream s Identity Char => String -> ParsecT s u Identity String
symbol name = lexeme (S.string name)

{-hash :: Stream s Identity Char => ParsecT s u Identity String
hash = symbol "#"-}

dot :: Stream s Identity Char => ParsecT s u Identity String
dot = symbol "."

semi :: Stream s Identity Char => ParsecT s u Identity String
semi = symbol ";"

comma :: Stream s Identity Char => ParsecT s u Identity String
comma = symbol ","

colon :: Stream s Identity Char => ParsecT s u Identity String
colon = symbol ":"

strCase :: Stream s Identity Char => String -> ParsecT s u Identity String
strCase [] = return []
strCase s@(c:cs) = satisfy (charCaseEq c) >> strCase cs >> return s

{-css21Style :: P.LanguageDef st
css21Style = emptyDef
               { P.commentStart = "/*"
               , P.commentEnd = "*/"
               , P.nestedComments = False
               , P.caseSensitive = False
               }

css21Style :: Stream s Identity Char => P.GenLanguageDef s u m
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
lexer :: Stream s Identity Char => ParsecT s u Identity String
lexer = P.makeTokenParser css21Style
lexer = P.TokenParser

braces :: Stream s Identity Char => ParsecT s u Identity a -> ParsecT s u Identity a
braces :: ParsecT String () Identity a -> ParsecT String () Identity a
braces = P.braces lexer-}
         
color :: Stream s Identity Char => ParsecT s u Identity Color
color = hashColor <|> basicNamedColor

hashColor :: Stream s Identity Char => ParsecT s u Identity Color
hashColor = do
    char '#'
    try sixHexDigitColor <|> threeHexDigitColor

sixHexDigitColor :: Stream s Identity Char => ParsecT s u Identity Color
sixHexDigitColor = do
    r <- count 2 hexDigit
    g <- count 2 hexDigit
    b <- count 2 hexDigit
    return $ RGBColor (hexToI r) (hexToI g) (hexToI b)

threeHexDigitColor :: Stream s Identity Char => ParsecT s u Identity Color
threeHexDigitColor = do
    r <- hexDigit
    g <- hexDigit
    b <- hexDigit
    return $ RGBColor (hexToI (replicate 2 r))
                      (hexToI (replicate 2 g))
                      (hexToI (replicate 2 b))

basicNamedColor :: Stream s Identity Char => ParsecT s u Identity Color
basicNamedColor = do
    n <- name
    case (basicNameToColor n) of
      Just c -> return c
      Nothing -> fail "invalid color name"

asciiAlpha :: Stream s Identity Char => ParsecT s u Identity Char
asciiAlpha = satisfy isAsciiAlpha

value :: Stream s Identity Char => ParsecT s u Identity Value
value = do
    xs <- many1 valueElem
    return (Value xs)

valueElem :: Stream s Identity Char => ParsecT s u Identity ValueElem
valueElem =
    choice [ do{ a <- try any; return (VEAny a) }
           , do{ b <- try block; return (VEBlock b) }
           , do{ k <- atKeyword; return (VEAtKeyword k) }
           ]

block :: Stream s Identity Char => ParsecT s u Identity Block
block = do
    xs <- between (char '{' >> spaces) (char '}' >> spaces)
                  (many blockElem)
    return (Block xs)

blockElem :: Stream s Identity Char => ParsecT s u Identity BlockElem
blockElem = do
    e <- choice [ do{ a <- try any; return (BEAny a) }
                , do{ b <- try block; return (BEBlock b) }
                , do{ k <- atKeyword; return (BEAtKeyword k) }
                ]
    spaces
    return e

any :: Stream s Identity Char => ParsecT s u Identity Any
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


identifier :: Stream s Identity Char => ParsecT s u Identity String
identifier = lexeme ident

stringLit :: Stream s Identity Char => ParsecT s u Identity String
stringLit = lexeme string

atKeyword :: Stream s Identity Char => ParsecT s u Identity AtKeyword
atKeyword = do
    char '@'
    i <- ident
    return (AtKeyword i)

hash :: Stream s Identity Char => ParsecT s u Identity Any
hash = do
    char '#'
    n <- name
    return (Hash n)

number :: Stream s Identity Char => ParsecT s u Identity Any
number = do
    n <- num
    return (Number n)

dimension :: Stream s Identity Char => ParsecT s u Identity Any
dimension = do
    n <- num
    i <- ident
    return (Dimension n i)

uri :: Stream s Identity Char => ParsecT s u Identity URI
uri = do
    between (S.string "url(" >> spaces) (spaces >> S.string ")")
            uriContent


uriContent :: Stream s Identity Char => ParsecT s u Identity String
uriContent = try string
             <|> (many $ choice
                          [ try (satisfy isUnquotedURIContentChar)
                          , try nonascii
                          , escape
                          ]
                 )

signedLengthVal :: Stream s Identity Char => ParsecT s u Identity Length
signedLengthVal = do
    s <- sign
    n <- num
    let x = read (s++n) :: Double
    if x == 0
    then optional lengthUnit >> return (Length 0 Nothing)
    else (lengthUnit >>= \u -> return (Length x (Just u)))
         <|> fail "length unit needed after non-zero value"

lengthVal :: Stream s Identity Char => ParsecT s u Identity Length
lengthVal = do
    n <- num
    let x = read n :: Double
    if x == 0
    then optional lengthUnit >> return (Length 0 Nothing)
    else (lengthUnit >>= \u -> return (Length x (Just u)))
         <|> fail "length unit needed after non-zero value"

lengthUnit :: Stream s Identity Char => ParsecT s u Identity LengthUnit
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

pvWhiteSpace :: Stream s Identity Char => ParsecT s u Identity PVWhiteSpace
pvWhiteSpace = choice
    [ try (keywordCase "normal") >> return PVWhiteSpaceNormal
    , try (keywordCase "pre") >> return PVWhiteSpacePre
    , try (keywordCase "nowrap") >> return PVWhiteSpaceNoWrap
    , try (keywordCase "pre-wrap") >> return PVWhiteSpacePreWrap
    , try (keywordCase "pre-line") >> return PVWhiteSpacePreLine
    ,     (keywordCase "inherit") >> return PVWhiteSpaceInherit
    ]

signedPercentage :: Stream s Identity Char => ParsecT s u Identity Percentage
signedPercentage = do
    s <- sign
    n <- num
    char '%'
    return (Percentage (read (s++n) :: Double))

sign :: Stream s Identity Char => ParsecT s u Identity String
sign =   (char '+' >> return "")
     <|> (char '-' >> return "-")
     <|> return ""
       

percentage :: Stream s Identity Char => ParsecT s u Identity Percentage
percentage = do
    n <- num
    char '%'
    return (Percentage (read n :: Double))

positiveIntNoSign :: Stream s Identity Char => ParsecT s u Identity Int
positiveIntNoSign = do
    ds <- many1 digit <?> "postive integer without sign"
    let n = read ds
    if n > 0
        then return n
        else fail "Only positive integers are allowed."

integer :: Stream s Identity Char => ParsecT s u Identity Int
integer = do
    s <- option "" (S.string "-")
    ds <- many1 digit
    return (read (s ++ ds))

important :: Stream s Identity Char => ParsecT s u Identity Important
important = option False (symbol "!" >> keyword "important" >> return True)

keyword :: Stream s Identity Char => String -> ParsecT s u Identity String
keyword name = do
    n <- S.string name <?> name
    notFollowedBy nmchar <?> ("end of " ++ show name)
    return n

keywordCase :: Stream s Identity Char => String -> ParsecT s u Identity String
keywordCase name = do
    n <- strCase name <?> name
    notFollowedBy nmchar <?> ("end of " ++ show name)
    return n

ident :: Stream s Identity Char => ParsecT s u Identity String
ident = do
    h <- option "" (S.string "-")
    s <- nmstart
    cs <- many nmchar
    return (h ++ (s:cs))

name :: Stream s Identity Char => ParsecT s u Identity String
name = many1 nmchar

nmstart :: Stream s Identity Char => ParsecT s u Identity Char
nmstart = satisfy isNmStartChar <|> escape

nonascii :: Stream s Identity Char => ParsecT s u Identity Char
nonascii = satisfy isNonAsciiChar

nmchar :: Stream s Identity Char => ParsecT s u Identity Char
nmchar = satisfy isNmChar <|> escape

unicode :: Stream s Identity Char => ParsecT s u Identity Char
unicode = do
    char '\\'
    ds <- countRange 1 6 hexDigit
    optional (crlf <|> count 1 space)
    return $ chr (hexToI ds)

escape :: Stream s Identity Char => ParsecT s u Identity Char
escape = try unicode <|> simpleEscape

simpleEscape :: Stream s Identity Char => ParsecT s u Identity Char
simpleEscape = char '\\' >> satisfy isSimpleEscapeChar

crlf :: Stream s Identity Char => ParsecT s u Identity String
crlf = S.string "\r\n"

hexToI :: Num a => String -> a
hexToI ds = let ((n,_):_) = readHex ds
            in n

string :: Stream s Identity Char => ParsecT s u Identity String
string = string1 <|> string2

badstring :: Stream s Identity Char => ParsecT s u Identity String
badstring = badstring1 <|> badstring2

string1 :: Stream s Identity Char => ParsecT s u Identity String
string1 = stringSub doubleQuote (stringContent '"') doubleQuote

badstring1 :: Stream s Identity Char => ParsecT s u Identity String
badstring1 = stringSub doubleQuote (stringContent '"') (try badStringEnd)

stringContent :: Stream s Identity Char => Char -> ParsecT s u Identity String
stringContent quoteChar = choice [ sequence [try escape]
                     , try (char '\\' >> nl)
                     , sequence [satisfy (isPlainStrChar quoteChar)]
                     ]

stringSub :: Stream s Identity Char => ParsecT s u Identity String ->
                                ParsecT s u Identity String ->
                                ParsecT s u Identity String ->
                                ParsecT s u Identity String
stringSub start unit end = do
  start
  t <- manyTill unit end
  return (concat t)

doubleQuote :: Stream s Identity Char => ParsecT s u Identity String
doubleQuote = S.string "\""

singleQuote :: Stream s Identity Char => ParsecT s u Identity String
singleQuote = S.string "'"

badStringEnd :: Stream s Identity Char => ParsecT s u Identity String
badStringEnd = S.string "\\?"

string2 :: Stream s Identity Char => ParsecT s u Identity String
string2 = stringSub singleQuote (stringContent '\'') singleQuote

badstring2 :: Stream s Identity Char => ParsecT s u Identity String
badstring2 = stringSub singleQuote (stringContent '\'') (try badStringEnd)

nl :: Stream s Identity Char => ParsecT s u Identity String
nl =   S.string "\n"
   <|> (char '\r' >>= \cr ->
         (try (char '\n') >>= \lf -> return [cr, lf])
         <|> return [cr]
       )
   <|> S.string "\f"

num :: Stream s Identity Char => ParsecT s u Identity String
num = try (
        do{ int <- many digit
          ; dot <- char '.'
          ; frac <- many1 digit
          ; return (int ++ dot:frac)
          }
      )
      <|> many1 digit

spaces1 :: Stream s Identity Char => ParsecT s u Identity String
spaces1 = many1 space

spaces :: Stream s Identity Char => ParsecT s u Identity String
spaces = many space

space :: Stream s Identity Char => ParsecT s u Identity Char
space = satisfy isSpaceChar


perm0 :: Stream s Identity Char => ParsecT s u Identity (Char, Char, Char)
perm0 = permute ((,,) <$?> ('_', oneOf "na")
                      <|?> ('_', oneOf "nb")
                      <|?> ('_', oneOf "nc"))

