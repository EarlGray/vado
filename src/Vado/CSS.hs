{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Vado.CSS where

import           Control.Applicative
import           Control.Monad (void)
import qualified Data.Char as C
import           Data.Either (partitionEithers)
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word8)

import qualified Data.Attoparsec.Text as Atto

import           Vado.Types

-- | CSS values: weakly-typed data
data CSSValue
  = CSS_Keyword Text
  | CSS_Em Double
  | CSS_Px Double
  | CSS_Pt Double
  | CSS_Num Double
  | CSS_Percent Double
  | CSS_Url Text
  | CSS_String Text
  | CSS_Seq [CSSValue]      -- space-separated list
  | CSS_List [CSSValue]     -- comma-separated list
  | CSS_Tuple [CSSValue]    -- slash-separated list
  | CSS_RGB Word8 Word8 Word8
  deriving (Show, Eq)

instance HasDebugView CSSValue where
  showdbg (CSS_Keyword k) = T.unpack k
  showdbg (CSS_Em em) = show em ++ "em"
  showdbg (CSS_Px px) = show px ++ "px"
  showdbg (CSS_Num n) = show n
  showdbg (CSS_Percent p) = show p ++ "%"
  showdbg (CSS_Url url) = "url(" ++ T.unpack url ++ ")"
  showdbg (CSS_String s) = "\"" ++ T.unpack s ++ "\""
  showdbg (CSS_RGB r g b) = concat ["rgb(", show r, ", ", show g, ", ", show b, ")"]
  showdbg other = "TODO:showdbg(" ++ show other ++ ")"


-- | Style for an element, consists of own and inheritable properties.
-- Must be complete (e.g. not just style diff)
-- No types for now, just stringly-typed prototypes.
data Style = Style
  { styleOwn :: M.Map CSSOwnProperty CSSValue
  , styleInherit :: M.Map CSSProperty CSSValue
  }

noStyle :: Style
noStyle = Style { styleOwn = M.empty, styleInherit = M.empty }

instance HasDebugView Style where
  showdbg Style{..} = L.intercalate "; " (ownprops ++ inheritprops)
    where
      ownprops = map showprop $ M.toAscList styleOwn
      inheritprops = map showprop $ M.toAscList styleInherit
      showprop (prop, val) = concat [showdbg prop, ": ", showdbg val]

instance Show Style where
  show = showdbg

instance Read Style where
  readsPrec _ s = [(cssFarcer s, "")]

-- | Add CSS properties to existing properties, including own properties
-- Shadow previous values if they exist.
overriding :: Style -> Style -> Style
overriding newprops base = Style
    { styleInherit = styleInherit newprops `M.union` styleInherit base
    , styleOwn = styleOwn newprops `M.union` styleOwn base
    }

-- | Given a node and a parent style, compute cascaded node style
cascadeStyle :: Stylesheet -> Style -> Style
           -- this module should not need this
cascadeStyle Stylesheet{..} parentStyle = Style
    { styleInherit = attrStyle `merge` htmlStyle `merge` styleInherit parentStyle
    , styleOwn = attrOwn `M.union` htmlOwn
    }
  where
    Style{ styleInherit = attrStyle, styleOwn = attrOwn} = stylesheetAuthor
    Style{ styleInherit = htmlStyle, styleOwn = htmlOwn} = stylesheetBrowser
    merge = M.mergeWithKey cascadingValue id id

{- | Augment/override/merge new CSS style with base style.
-- The new style inherits/merges parent's properties, unless overriden.
-- Some values are computed based on parent values (see `cascadingValue`).
cascadingOver :: Style -> Style -> Style
cascadingOver style parent = style { styleInherit = inheritable `merge` styleInherit parent }
  where
    inheritable = case M.lookup CSSDisplay (styleOwn style) of
                Just (CSS_Keyword "inline") -> M.delete CSSTextAlign (styleInherit style)
                _ -> styleInherit style
    merge = M.mergeWithKey cascadingValue id id
-- -}

cascadingValue :: CSSProperty -> CSSValue -> CSSValue -> Maybe CSSValue
cascadingValue CSSFontSize new (CSS_Px oldsz) =
  case new of
    CSS_Keyword kw ->
      case kw `L.lookup` (relkeywords ++ abskeywords) of
        Just val ->
          cascadingValue CSSFontSize val (CSS_Px oldsz)
        Nothing ->
          warning ("font-size ignored: " ++ show kw) Nothing
    _ ->
      CSS_Px <$> cssRelEmLength oldsz oldsz new
  where
    relkeywords =
      [ ("smaller", CSS_Percent 70)
      , ("larger",  CSS_Percent 130)
      ]
    abskeywords =
      [ ("xx-small",  CSS_Em 0.30)
      , ("x-small",   CSS_Em 0.50)
      , ("small",     CSS_Em 0.70)
      , ("medium",    CSS_Em 1.00)
      , ("large",     CSS_Em 1.30)
      , ("x-large",   CSS_Em 1.70)
      , ("xx-large",  CSS_Em 2.00)
      ]
cascadingValue _ st _ = Just st


-- A difference in styles, useful for rendering.
type StyleDiff = M.Map CSSProperty CSSValue

-- | Get the difference between two (complete) styles both ways.
-- Given a new and an old style, produce a pair of style diffs: "push" and "pop", s.t.
--   old + push -> new
--   new + pop -> old
styleDiff :: Style -> Style -> (StyleDiff, StyleDiff)
styleDiff Style{styleInherit=new} Style{styleInherit=old} = (toNew, toOld)
  where
    toNew = M.differenceWith (\v1 v2 -> if v1 /= v2 then Just v1 else Nothing) new old
    toOld = M.differenceWith (\v1 v2 -> if v1 /= v2 then Just v1 else Nothing) old new

noStyling :: (StyleDiff, StyleDiff)
noStyling = (M.empty, M.empty)

applyDiff :: Style -> StyleDiff -> Style
applyDiff st diff = st { styleInherit = diff `M.union` (styleInherit st) }

-- | A uniform interface to CSSProperty and CSSOwnProperty
class IsCSSProperty prop where
  cssValueMaybe :: Style -> prop -> Maybe CSSValue
  cssDefault :: prop -> CSSValue    -- TODO: rename to cssInitial

  cssValue :: Style -> prop -> CSSValue
  cssValue st prop = fromMaybe (cssDefault prop) (cssValueMaybe st prop)


-- | Inheritable properites:
data CSSProperty
  = CSSBackgroundColor
  | CSSColor
  | CSSFontFamily
  | CSSFontSize
  | CSSFontStyle
  | CSSFontWeight
  | CSSWhiteSpace
  | CSSTextAlign
  | CSSTextDecorationLine
  deriving (Show, Eq, Ord, Enum)

cssPropertyNames :: M.Map CSSProperty Text
cssPropertyNames = M.fromList
  [ (CSSBackgroundColor,    "background-color")
  , (CSSColor,              "color")
  , (CSSFontFamily,         "font-family")
  , (CSSFontSize,           "font-size")
  , (CSSFontStyle,          "font-style")
  , (CSSFontWeight,         "font-weight")
  , (CSSWhiteSpace,         "white-space")
  , (CSSTextAlign,          "text-align")
  , (CSSTextDecorationLine, "text-decoraton-line")
  ]

cssNamesOfProperties :: M.Map Text CSSProperty
cssNamesOfProperties =
  M.fromList [ (v, k) | (k, v) <- M.toList cssPropertyNames ]

cssPropertyDefaults :: M.Map CSSProperty CSSValue
cssPropertyDefaults = M.fromList
  [ (CSSBackgroundColor,    CSS_RGB 255 255 255)
  , (CSSColor,              CSS_RGB 0 0 0)
  , (CSSFontFamily,         CSS_Keyword "serif")      -- ?
  , (CSSFontSize,           CSS_Keyword "medium")      -- ?
  , (CSSFontStyle,          CSS_Keyword "normal")
  , (CSSFontWeight,         CSS_Keyword "normal")
  , (CSSTextAlign,          CSS_Keyword "left")
  , (CSSTextDecorationLine, CSS_Keyword "none")
  , (CSSWhiteSpace,         CSS_Keyword "normal")
  ]

instance IsCSSProperty CSSProperty where
  cssValueMaybe Style{ styleInherit=properties } prop =
    M.lookup prop properties

  cssDefault prop =
    case M.lookup prop cssPropertyDefaults of
      Just val -> val
      Nothing -> CSS_Keyword "uninitialized"

instance HasDebugView CSSProperty where
  showdbg prop =
    case M.lookup prop cssPropertyNames of
      Just name -> T.unpack name
      Nothing -> concat ["TODO:showdbg(", show prop, ")"]


-- | Non-inheritable properties:
data CSSOwnProperty
  = CSSDisplay
  -- | CSSMargin
  -- | CSSBorder
  -- | CSSPadding
  deriving (Show, Eq, Ord, Enum)

cssOwnPropertyNames :: M.Map CSSOwnProperty Text
cssOwnPropertyNames = M.fromList
  [ (CSSDisplay,                "display")
  -- , (CSSMargin,                 "margin")
  -- , (CSSBorder,                 "border")
  -- , (CSSPadding,                "padding")
  ]
cssOwnPropertyDefaults :: M.Map CSSOwnProperty CSSValue
cssOwnPropertyDefaults = M.fromList
  [ (CSSDisplay,        CSS_Keyword "block")
  -- , (CSSMargin,         CSS_Px 0)
  -- , (CSSBorder,         CSS_Px 0)
  -- , (CSSPadding,        CSS_Px 0)
  ]

cssNamesOfOwnProperties :: M.Map Text CSSOwnProperty
cssNamesOfOwnProperties =
  M.fromList [ (v, k) | (k, v) <- M.toList cssOwnPropertyNames ]

instance IsCSSProperty CSSOwnProperty where
  cssValueMaybe Style{ styleOwn=properties } prop =
    M.lookup prop properties

  cssDefault prop =
    case M.lookup prop cssOwnPropertyDefaults of
      Just val -> val
      Nothing -> CSS_Keyword "uninitialized"

instance HasDebugView CSSOwnProperty where
  showdbg prop =
    case M.lookup prop cssOwnPropertyNames of
      Just name -> T.unpack name
      Nothing -> concat ["TODO:showdbg(", show prop, ")"]

--------------------------------------------------------------------------------
-- CSS stylesheets, rule matching and rule chains

data Stylesheet = Stylesheet
  { stylesheetAuthor :: Style
  , stylesheetBrowser :: Style
  }
  deriving (Show)

emptyStylesheet :: Stylesheet
emptyStylesheet = Stylesheet noStyle noStyle

stylesheetValue :: IsCSSProperty prop => Stylesheet -> prop -> CSSValue
stylesheetValue Stylesheet{..} prop =
  fromMaybe (stylesheetBrowser `cssValue` prop) (stylesheetAuthor `cssValueMaybe` prop)

--------------------------------------------------------------------------------
-- CSS parsers and printers

-- TODO: use css-text here
-- a farcical CSS parser:
cssFarcer :: String -> Style
cssFarcer s = css $ mapMaybe parseProp $ T.splitOn ";" (T.pack s)
  where
    parseProp p = case T.break (== ':') p of
      (_, "") -> Nothing
      (key, val) -> Just (T.strip key, T.strip $ T.tail val)


-- | Read and split properties into own and inheritable
-- Warn about unknown properties and values, ignore them.
type OwnOrInheritable = Either (CSSOwnProperty, CSSValue) (CSSProperty, CSSValue)

css :: [(Text, Text)] -> Style
css properties = Style
    { styleOwn = M.fromList own
    , styleInherit = M.fromList inherit
    }
  where
    (own, inherit) = partitionEithers $ map desugarValues $ concatMap readProperty properties

    readProperty :: (Text, Text) -> [OwnOrInheritable]
    readProperty (name, textval) =
      case cssReadValue textval of
        Left _ ->
          warning (concat ["Unknown css value: ", T.unpack name, "=", T.unpack textval]) []
        Right val ->
          case M.lookup name cssNamesOfProperties of
            Just prop -> [Right (prop, val)]
            Nothing ->
              case M.lookup name cssNamesOfOwnProperties of
                Just prop -> [Left (prop, val)]
                Nothing ->
                  case M.lookup name cssShorthands of
                    Just unshorthand -> unshorthand val
                    Nothing ->
                      if "-" `T.isPrefixOf` name
                      then []
                      else warning ("Unknown property: " ++ T.unpack name ++ "=" ++ show val) []

    desugarValues = \case
      (Right (prop, CSS_Keyword color)) | prop `elem` [CSSColor, CSSBackgroundColor] ->
        let mbUnaliased = M.lookup color cssColorAliases
            -- yes, it's always Right:
            Right color' = cssReadValue $ fromMaybe color $ mbUnaliased
        in Right (prop, color')
      other -> other

    cssShorthands :: M.Map Text (CSSValue -> [OwnOrInheritable])
    cssShorthands = M.fromList
      [ ("font",            expandFont)
      , ("text-decoration", expandTextDecoration)
      ]

    -- https://developer.mozilla.org/en-US/docs/Web/CSS/font
    expandFont = \case
        (CSS_Keyword kw) | kw `elem` uiFonts ->
          map Right $ M.toList $ styleInherit uiFont
        CSS_Seq vals ->
          parseFontFamily $ reverse vals
        other ->
          warning ("unknown font=" ++ show other) []
      where
        uiFonts = ["caption", "icon", "menu", "message-box", "small-caption", "status-bar"]
        fontStretch = "normal" : [T.concat [pre, stem] |
           pre <- ["", "ultra-", "semi-", "extra-"],
           stem <- ["condensed", "expanded"]]
        fontSizes =
           [ "xx-small", "x-small", "small", "medium"
           , "large", "x-large", "xx-large", "xxx-large"
           , "smaller", "larger"]

        parseFontFamily [] = warning ("CSS font: expected font-family") []
        parseFontFamily (val:vals) =
          case val of
            CSS_Keyword _ -> Right (CSSFontFamily, val) : parseFontSize vals
            CSS_String _ -> Right (CSSFontFamily, val) : parseFontSize vals
            -- TODO: CSS_List ["font1", "font2", ...]
            _ -> warning ("CSS unknown font=" ++ show val) $ parseFontSize vals

        -- TODO: slash-separated line-height
        parseFontSize [] = warning ("CSS font: expected font-size") []
        parseFontSize (val:vals) =
            case val of
              CSS_Num _ -> Right (CSSFontSize, val) : parseFont vals
              CSS_Px _ -> Right (CSSFontSize, val) : parseFont vals
              CSS_Pt pt -> Right (CSSFontSize, CSS_Px (pt * 4/3)) : parseFont vals
              CSS_Percent _ -> Right (CSSFontSize, val) : parseFont vals
              CSS_Em _ -> Right (CSSFontSize, val) : parseFont vals
              CSS_Keyword kw | kw `elem` fontSizes -> Right (CSSFontSize, val) : parseFont vals
              _ -> warning ("unknown font[-size]=" ++ show val) $ parseFont vals

        parseFont [] = []
        parseFont (val:vals) =
          case val of
            -- font-style?
            CSS_Keyword kw | kw `elem` ["normal", "italic", "oblique"] ->
              Right (CSSFontStyle, val) : parseFont vals
            -- font-variant-css21?
            CSS_Keyword "small-caps" ->
              warning "font: small-caps ignored" $ parseFont vals
            -- font-weight?
            CSS_Keyword kw | kw `elem` ["normal", "bold", "lighter", "bolder"] ->
              Right (CSSFontWeight, val) : parseFont vals
            CSS_Num num | (num `elem` [100,200..900] || num == 950) ->
              Right (CSSFontWeight, val) : parseFont vals
            -- font-stretch?
            CSS_Keyword kw | kw `elem` fontStretch ->
              warning ("font: ignoring font-stretch=" ++ show val) $ parseFont vals
            CSS_Percent _ ->
              warning ("font: ignoring font-stretch=" ++ show val) $ parseFont vals
            _ -> warning ("unknown font[-prop]=" ++ show val) $ parseFont vals


    expandTextDecoration = \case
      CSS_Keyword kw | kw `elem` ["none", "underline", "overline", "line-through"] ->
        [Right (CSSTextDecorationLine, CSS_Keyword kw)]
      CSS_Seq vals ->
        concatMap expandTextDecoration vals
      other -> warning ("Unknown value for text-decoration=" ++ show other) []

-- | Parses a CSSValue from text for a CSS value
cssReadValue :: Text -> Either Text CSSValue
cssReadValue txtval =
  case Atto.parseOnly cssparseValue txtval of
    Left _ -> Left txtval
    Right val -> Right val

cssparseValue :: Atto.Parser CSSValue
cssparseValue = valp <* Atto.endOfInput
  where
    valp = do
      let parsers = [ cssparseUrl, cssparseColor, cssparseString
                    , cssparseIdentifier, cssparseLength, cssparseNum
                    ]
      vals <- Atto.many1 $ (Atto.choice parsers <* Atto.skipSpace)
      case vals of
        [val] -> return val
        _ -> return $ CSS_Seq vals
    strp = do
      void $ Atto.char '"'
      cs <- Atto.many' (Atto.notChar '"' <|> (Atto.char '\\' *> Atto.anyChar))
      void $ Atto.char '"'
      return $ T.pack cs
    cssparseIdentifier = do
      start <- ((\c -> [c]) <$> Atto.satisfy (\c -> C.isAlpha c || c == '_')) <|> do
        c1 <- Atto.char '-'
        c2 <- Atto.satisfy (\c -> C.isAlpha c || c == '_')
        return [c1, c2]
      rest <- Atto.takeWhile (\c -> C.isAlphaNum c || c == '_' || c == '-')
      return (CSS_Keyword $ T.toLower $ T.concat [ T.pack start, rest ])
    cssparseString = CSS_String <$> strp
    cssparseLength = do
      num <- Atto.double
      Atto.choice [
          CSS_Px num <$ Atto.string "px"
        , CSS_Em num <$ Atto.string "em"
        , CSS_Pt num <$ Atto.string "pt"
        , CSS_Percent num <$ Atto.string "%"
        ]
    cssparseNum = CSS_Num <$> Atto.double
    cssparseUrl = do
      let urlp = (strp <|> Atto.takeWhile (/= ')'))
      url <- Atto.string "url(" *> Atto.skipSpace *> urlp <* Atto.skipSpace <* Atto.char ')'
      return $ CSS_Url url

    colorhashp = do
      let hex d1 d0 = fromIntegral (0x10 * C.digitToInt d1 + C.digitToInt d0) :: Word8
      _ <- Atto.char '#'
      digits <- Atto.takeWhile C.isHexDigit
      return $ case T.unpack digits of
        (r1:r2:g1:g2:b1:b2:_) -> CSS_RGB (hex r1 r2) (hex g1 g2) (hex b1 b2)
        (r:g:b:_) -> CSS_RGB (hex r r) (hex g g) (hex b b)
        _ -> warning ("cannot read color: " ++ T.unpack digits) $ CSS_RGB 0 0 0
    rgbp = do
      _ <- Atto.string "rgb("
      let rgbdecp = Atto.decimal
      let rgbpercp = (\percent -> round $ 2.55 * percent) <$> (Atto.double <* Atto.char '%')
      nums <- (rgbdecp <|> rgbpercp) `Atto.sepBy` (Atto.char ',' >> Atto.skipSpace) :: Atto.Parser [Int]
      _ <- Atto.char ')'
      case nums of
        [r, g, b] -> return $ CSS_RGB (fromIntegral r) (fromIntegral g) (fromIntegral b)
        _ -> fail $ "cannot read color: rgb(" ++ show nums ++ ")"
    cssparseColor = colorhashp <|> rgbp


-- | TODO: cursors bitmap collection
-- https://developer.mozilla.org/en-US/docs/Web/CSS/cursor

-- CSS misc

cssRelEmLength :: Double -> Double -> CSSValue -> Maybe Double
cssRelEmLength relsz _ (CSS_Percent pcnt) = Just (relsz * pcnt/100)
cssRelEmLength _ emsz val = cssEmLength emsz val

cssEmLength :: Double -> CSSValue -> Maybe Double
cssEmLength emsz (CSS_Em em) = Just (em * emsz)
cssEmLength _ val = cssLength val

cssLength :: CSSValue -> Maybe Double
cssLength = \case
  CSS_Px num -> Just num
  CSS_Num num -> Just num
  CSS_Pt pt -> Just (pt * 4/3)
  _ -> Nothing


-- interactive UI elements must stand out from the surrounding elements,
-- unless explicitly overriden.
-- TODO: should this be here?
uiFont :: Style
uiFont = css
  [ ("font-family", "sans")   -- TODO: take sans font from settings
  , ("font-size", "1rem")  -- TODO: use rem
  , ("font-style", "normal")
  , ("font-weight", "normal")
  ]

-- CSS colors
cssColorAliases :: M.Map Text Text
cssColorAliases = M.fromList (cssColorsLevel1 ++ cssColorsLevel2)
  where
    cssColorsLevel1 =                  -- CSS Level 1 colors:
        [ ("aqua",      "#00ffff") , ("black",     "#000000")
        , ("blue",      "#0000ff") , ("fuchsia",   "#ff00ff")
        , ("gray",      "#808080") , ("green",     "#008000")
        , ("maroon",    "#800000") , ("navy",      "#000080")
        , ("lime",      "#00ff00") , ("olive",     "#808000")
        , ("purple",    "#800080") , ("red",       "#ff0000")
        , ("silver",    "#c0c0c0") , ("teal",      "#008080")
        , ("white",     "#ffffff") , ("yellow",    "#ffff00")
        ]
    cssColorsLevel2 =                     -- CSS Level 2 (Revision 1)
        [ ("orange",        "#ffa500") , ("aliceblue",     "#f0f8ff")
        , ("antiquewhite",  "#faebd7") , ("aquamarine",    "#7fffd4")
        , ("azure",         "#f0ffff") , ("beige",         "#f5f5d ")
        , ("bisque",        "#ffe4c4") , ("blanchedalmond","#ffebcd")
        , ("blueviolet",    "#8a2be2") , ("brown",         "#a52a2a")
        , ("burlywood",     "#deb887") , ("cadetblue",     "#5f9ea0")
        , ("chartreuse",    "#7fff00") , ("chocolate",     "#d2691e")
        , ("coral",         "#ff7f50") , ("cornflowerblue","#6495ed")
        , ("cornsilk",      "#fff8dc") , ("crimson",       "#dc143c")
        , ("cyan",          "#00ffff") , ("darkblue",      "#00008b")
        , ("darkcyan",      "#008b8b") , ("darkgoldenrod", "#b8860b")
        , ("darkgray",      "#a9a9a9") , ("darkgreen",     "#006400")
        , ("darkgrey",      "#a9a9a9") , ("darkkhaki",     "#bdb76b")
        , ("darkmagenta",   "#8b008b") , ("darkolivegreen","#556b2f")
        , ("darkorange",    "#ff8c00") , ("darkorchid",    "#9932cc")
        , ("darkred",       "#8b0000") , ("darksalmon",    "#e9967a")
        , ("darkseagreen",  "#8fbc8f") , ("darkslateblue", "#483d8b")
        , ("darkslategray", "#2f4f4f") , ("darkslategrey", "#2f4f4f")
        , ("darkturquoise", "#00ced1") , ("darkviolet",    "#9400d3")
        , ("deeppink",      "#ff1493") , ("deepskyblue",   "#00bfff")
        , ("dimgray",       "#696969") , ("dimgrey",       "#696969")
        , ("dodgerblue",    "#1e90ff") , ("firebrick",     "#b22222")
        , ("floralwhite",   "#fffaf0") , ("forestgreen",   "#228b22")
        , ("gainsboro",     "#dcdcdc") , ("ghostwhite",    "#f8f8ff")
        , ("gold",          "#ffd700") , ("goldenrod",     "#daa520")
        , ("greenyellow",   "#adff2f") , ("grey",          "#808080")
        , ("honeydew",      "#f0fff0") , ("hotpink",       "#ff69b4")
        , ("indianred",     "#cd5c5c") , ("indigo",        "#4b0082")
        , ("ivory",         "#fffff0") , ("khaki",         "#f0e68c")
        , ("lavender",      "#e6e6fa") , ("lavenderblush", "#fff0f5")
        , ("lawngreen",     "#7cfc00") , ("lemonchiffon",  "#fffacd")
        , ("lightblue",     "#add8e6") , ("lightcoral",    "#f08080")
        , ("lightcyan",     "#e0fff-")
        ]
