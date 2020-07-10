{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Vado.CSS where

import           Control.Applicative
import           Control.Monad (void, forM_)
import qualified Control.Monad.State as St
import qualified Data.Char as C
import qualified Data.Either as Ei
import qualified Data.IntMap as IM
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, maybeToList, mapMaybe)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word8)

import qualified Data.Attoparsec.Text as Atto
import qualified Text.CSS.Parse as CSSP

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


-- | A CSS declaration block.
data Style = Style
  { styleOwn :: M.Map CSSOwnProperty CSSValue
  , styleInherit :: M.Map CSSProperty CSSValue
  }
  deriving (Eq)

noStyle :: Style
noStyle = Style { styleOwn = M.empty, styleInherit = M.empty }

instance Semigroup Style where (<>) = overriding
instance Monoid Style where mempty = noStyle

instance HasDebugView Style where
  showdbg Style{..} = L.intercalate "; " (ownprops ++ inheritprops)
    where
      ownprops = map showprop $ M.toAscList styleOwn
      inheritprops = map showprop $ M.toAscList styleInherit
      showprop (prop, val) = concat [showdbg prop, ": ", showdbg val]

instance Show Style where
  show = showdbg

-- | Add CSS properties to existing properties, including own properties
-- Shadow previous values if they exist.
overriding :: Style -> Style -> Style
overriding new old = Style
    { styleInherit = styleInherit new `M.union` styleInherit old
    , styleOwn = styleOwn new `M.union` styleOwn old
    }

-- | Given a node and a parent style, compute cascaded node style
cascadeStyle :: Stylesheet -> Style -> Style
           -- this module should not need this
cascadeStyle Stylesheet{..} parentStyle = Style
    { styleInherit = compInherit `merge` styleInherit parentStyle
    , styleOwn = compOwn
    }
  where
    Style{ styleInherit = compInherit, styleOwn = compOwn } = stylesheetComputed
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
  cssValue st prop = fromMaybe (cssDefault prop) $ cssValueMaybe st prop


-- | Inheritable properites:
data CSSProperty
  = CSSBackgroundColor   -- TODO: it's CSSOwnProperty
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
-- CSS stylesheets, selectors, rule matching and rule chains

data Stylesheet = Stylesheet
  { stylesheetUserBrowser :: (ImportantStyle, Style)
  -- ^ OriginImportantUser and OriginUser+OriginBrowser
  , stylesheetAttr :: (ImportantStyle, Style)
  -- ^ OriginImportantAuthor, OriginAuthor from style=""
  , stylesheetAuthor :: (ImportantStyle, Style)
  -- ^ OriginImportantAuthor, OriginAuthor not from style=""
  , stylesheetAuthorChain :: [RuleID]

  , stylesheetComputed :: Style
  -- ^ see computedStyle: all of the above styles cascaded into one.
  }
  deriving (Show)

emptyStylesheet :: Stylesheet
emptyStylesheet = Stylesheet
  { stylesheetUserBrowser = (noStyle, noStyle)
  , stylesheetAttr = (noStyle, noStyle)
  , stylesheetAuthorChain = []
  , stylesheetAuthor = (noStyle, noStyle)
  , stylesheetComputed = noStyle
  }

stylesheetValue :: IsCSSProperty prop => Style -> Stylesheet -> prop -> CSSValue
stylesheetValue parentStyle sheet prop =
  case stylesheetComputed sheet `cssValueMaybe` prop  of
    Just (CSS_Keyword "initial") -> cssDefault prop
    Just (CSS_Keyword "inherit") -> parentStyle `cssValue` prop
    Just val -> val
    Nothing -> noStyle{styleInherit = styleInherit parentStyle} `cssValue` prop

stylesheetOwnValue :: IsCSSProperty prop => Stylesheet -> prop -> CSSValue
stylesheetOwnValue = stylesheetValue noStyle

computedStyle :: Stylesheet -> Style
computedStyle Stylesheet{..} = mconcat
  [ fst $ stylesheetUserBrowser                     -- OriginImportantUser
  , fst $ stylesheetAttr, fst $ stylesheetAuthor    -- OriginImportantAuthor
  , snd $ stylesheetAttr, snd $ stylesheetAuthor    -- OriginAuthor
  , snd $ stylesheetUserBrowser                     -- OriginUser, OriginBrowser
  ]


data CSSOrigin
  = OriginBrowser
  | OriginUser
  | OriginAuthor
  | OriginImportantAuthor
  | OriginImportantUser
  deriving (Eq, Ord, Enum)

data Selector
  = SelAny                      -- `*` or empty
  | SelTag Text                 -- `div`
  | SelHasAttr Text             -- `[disabled]`
  | SelAttrEq Text Text         -- `[foo=bar]`
  | SelAttrWord Text Text       -- `[class~="small"]`
  | SelAttrPre Text Text        -- `[lang|="en"]`
  | SelClass Text           -- `.small`
  | SelId Text                  -- `#main`
  | SelDescends Selector Selector
  -- ^ e.g. `.hey ul li`: SelDescends (SelTag "li") (SelDescends (SelTag "ul") (SelClass "small"))
  | SelChild Selector Selector
  -- ^ e.g. `.blabla > .but`: SelChild (SelClass "but") (SelClass "blabla")
  | SelPseudoElem Text
  | SelPseudo Text     -- `button:focus`: SelPseudo (SelTag "a") "focus"
  -- SelPseudoLang Selector Text   -- `div:lang(fr)`
  | SelSiblings Selector Selector
  -- ^ `img + .caption` is SelSiblings (SelClass "caption") (SelTag "img")
  | SelAnd [Selector]
  deriving (Show, Eq)
  -- TODO: https://drafts.csswg.org/selectors-3/: important things like :not(), etc.

-- instance Show Selector  -- TODO

type IdNum = Int
type AttrNum = Int
type TagNum = Int
type Specificity = (IdNum, AttrNum, TagNum)

selectorSpecificity :: Selector -> Specificity
selectorSpecificity = \case
    SelAny -> (0, 0, 0)
    SelTag _ -> (0, 0, 1)
    SelPseudoElem _ -> (0, 0, 1)
    SelPseudo _ -> (0, 1, 0)
    SelClass _ -> (0, 1, 0)
    SelHasAttr _ -> (0, 1, 0)
    SelAttrEq _ _ -> (0, 1, 0)
    SelAttrWord _ _ -> (0, 1, 0)
    SelAttrPre _ _ -> (0, 1, 0)
    SelId _ -> (1, 0, 0)
    SelSiblings s1 s2 -> addSpecty (selectorSpecificity s1) (selectorSpecificity s2)
    SelChild s1 s2 -> addSpecty (selectorSpecificity s1) (selectorSpecificity s2)
    SelDescends s1 s2 -> addSpecty (selectorSpecificity s1) (selectorSpecificity s2)
    SelAnd [] -> (0, 0, 0)
    SelAnd (s:ss) -> addSpecty (selectorSpecificity s) (selectorSpecificity $ SelAnd ss)
  where
    addSpecty (a1, b1, c1) (a2, b2, c2) = (a1+a2, b1+b2, c1+c2)


-- | The CSS rules storage

type RuleID = Int

data CSSRules = CSSRules
  { cssRulesStorage :: IM.IntMap {-RuleID-} CSSBlock

  -- "indexes"
  , cssRulesById :: M.Map Text (S.Set RuleID)
  , cssRulesByClass :: M.Map Text (S.Set RuleID)
  , cssRulesByAttr :: M.Map Text (S.Set RuleID)
  , cssRulesByTag :: M.Map Text (S.Set RuleID)
  , cssRulesOther :: S.Set RuleID
  }

emptyCSSRules :: CSSRules
emptyCSSRules = CSSRules
  { cssRulesStorage = IM.empty
  , cssRulesById = M.empty
  , cssRulesByClass = M.empty
  , cssRulesByAttr = M.empty
  , cssRulesByTag = M.empty
  , cssRulesOther = S.empty
  }

instance Show CSSRules where
  show CSSRules{..} = "show CSSRules "++show (IM.size cssRulesStorage)++": TODO"

addCSSRules :: [CSSBlock] -> CSSRules -> CSSRules
addCSSRules blocks rules0 = flip St.execState rules0 $
    forM_ blocks $ \block -> do
      ruls <- St.gets cssRulesStorage
      let rid = if IM.null ruls then 1 else 1 + fst (IM.findMax ruls) :: RuleID

      let (ids, attrs, classes, tags) = selectorParts $ blockSelector block
      let amendIndex keys index =
            M.unionWith S.union index $ M.fromList $ map (\k -> (k, S.singleton rid)) keys

      if L.null (ids++attrs++classes++tags)
      then St.modify $ \rules -> rules
        { cssRulesStorage = IM.insert rid block $ cssRulesStorage rules
        , cssRulesOther = S.insert rid $ cssRulesOther rules
        }
      else St.modify $ \rules -> rules
        { cssRulesStorage = IM.insert rid block $ cssRulesStorage rules
        , cssRulesByTag = amendIndex tags $ cssRulesByTag rules
        , cssRulesByAttr = amendIndex attrs $ cssRulesByAttr rules
        , cssRulesByClass = amendIndex classes $ cssRulesByClass rules
        , cssRulesById = amendIndex ids $ cssRulesById rules
        }
  where
    selectorParts :: Selector -> ([Text], [Text], [Text], [Text])
    selectorParts = \case
      SelAny -> mempty
      SelTag t -> ([], [], [], [t])
      SelPseudoElem p -> ([], [], [], ["::" <> p])
      SelPseudo p -> ([], [], [":" <> p], [])
      SelClass clss -> ([], [], [clss], [])
      SelHasAttr attr -> ([], [attr], [], [])
      SelAttrEq attr _ -> ([], [attr], [], [])
      SelAttrWord attr _ -> ([], [attr], [], [])
      SelAttrPre attr _ -> ([], [attr], [], [])
      SelId id_ -> ([id_], [], [], [])
      SelSiblings s _ -> selectorParts s
      SelChild s _ -> selectorParts s
      SelDescends s _ -> selectorParts s
      SelAnd [] -> mempty
      SelAnd (s:ss) -> selectorParts s <> selectorParts (SelAnd ss)

-- | Match on all rules in CSSRules.
-- For each matching rule, sort it into important/regular chain.
-- Sort each chain by Specificity in descending order.
-- `matcher` is usually a closure on the element being matched.
rulechainsForMatcher :: CSSRules -> TagAttrs -> (Selector -> Maybe Specificity)
                     -> ([RuleID], [RuleID])
rulechainsForMatcher CSSRules{..} (tag, attrs) matcher =
    (elemsDesc imprules, elemsDesc regrules)
  where
    ruleset = mconcat $
      mapMaybe (`M.lookup` cssRulesByAttr) (M.keys attrs) ++
      mapMaybe (`M.lookup` cssRulesByClass) (maybe [] T.words $ M.lookup "class" attrs) ++
      [ fromMaybe S.empty $ (`M.lookup` cssRulesById) =<< M.lookup "id" attrs
      , fromMaybe S.empty $ tag `M.lookup` cssRulesByTag
      , cssRulesOther
      ]

    -- match and compute specificity:
    processRule :: RuleID -> Maybe (Either (Specificity, RuleID) (Specificity, RuleID))
    processRule rid =  -- TODO: AtText?
      let block = cssRulesStorage IM.! rid
          imp = blockImportant block
          mbSpecty = matcher $ blockSelector block
      in (\specty -> (if imp then Left else Right) (specty, rid)) <$> mbSpecty

    elemsDesc :: [(Specificity, RuleID)] -> [RuleID]
    elemsDesc = map snd . M.toDescList . M.fromList

    (imprules, regrules) = Ei.partitionEithers $ mapMaybe processRule $ S.toList ruleset

styleFromRulechain :: CSSRules -> [RuleID] -> Style
styleFromRulechain CSSRules{..} rulechain =
  mconcat $ mapMaybe (\rid -> blockStyle <$> IM.lookup rid cssRulesStorage) rulechain

--------------------------------------------------------------------------------
-- CSS parsers and printers

type AtText = Text
type IsImportant = Bool
type ImportantStyle = Style

type CSSBlock = ([AtText], Selector, IsImportant, Style)

blockImportant :: CSSBlock -> IsImportant
blockImportant (_, _, imp, _) = imp

blockStyle :: CSSBlock -> Style
blockStyle (_, _, _, st) = st

blockSelector :: CSSBlock -> Selector
blockSelector (_, sel, _, _) = sel

-- TODO: cssParser should separate ImportantStyle and Style into separate lists
-- TODO: weed out obviously unnecessary @media and other @-rules (e.g. `@media print`)
-- TDDO: compress (["@media screen"], selector, impstyle, style) into ([], ...)

-- | Takes a CSS document and parses it.
-- Flattens nested @-clauses.
-- Groups of selectors are decomposed into multiple blocks with the same body.
cssParser :: Text -> [CSSBlock]
cssParser t =
    either (\e -> warning e []) (L.concatMap (flatten [])) $ CSSP.parseNestedBlocks t
  where
    flatten :: [AtText] -> CSSP.NestedBlock -> [CSSBlock]
    flatten ats = \case
      CSSP.NestedBlock at blocks ->
        L.concatMap (flatten (at:ats)) blocks
      CSSP.LeafBlock (seltext, block) ->
        let isImportant (_, v) = "!important" `T.isSuffixOf` T.toLower (T.stripEnd v)
            dropImportant = T.init . T.dropWhileEnd (/= '!')
            (decls1, decls) = L.partition isImportant $ map (\(k, v) -> (T.toLower k, v)) block
            style1 = css $ map (\(k, v) -> (k, dropImportant v)) decls1
            style = css decls
            sels = either (\e -> warning e []) id $ cssSelectors seltext :: [Selector]
            unzipStyles s =
              let important = if style1 == noStyle then [] else [(reverse ats, s, True, style1)]
                  regular = if style == noStyle then [] else [(reverse ats, s, False, style)]
              in important ++ regular
        in L.concatMap unzipStyles sels


-- | Read and split properties into own and inheritable
-- Warn about unknown properties and values, ignore them.
type OwnOrInheritable = Either (CSSOwnProperty, CSSValue) (CSSProperty, CSSValue)

css :: [(Text, Text)] -> Style
css properties = Style
    { styleOwn = M.fromList own
    , styleInherit = M.fromList inherit
    }
  where
    (own, inherit) = Ei.partitionEithers $ map desugarValues $ concatMap readProperty properties

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

    cssparseIdentifier = (CSS_Keyword . T.toLower) <$> csspIdent
    cssparseString = CSS_String <$> csspString
    cssparseLength = do
      num <- Atto.double
      Atto.choice
        [ CSS_Px num <$ Atto.string "px"
        , CSS_Em num <$ Atto.string "em"
        , CSS_Pt num <$ Atto.string "pt"
        , CSS_Percent num <$ Atto.string "%"
        ]
    cssparseNum = CSS_Num <$> Atto.double
    cssparseUrl = do
      let urlp = (csspString <|> Atto.takeWhile (/= ')'))
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

csspString :: Atto.Parser Text
csspString = csspStr '"' <|> csspStr '\''
  where
    csspStr qc = do
      void $ Atto.char qc
      cs <- Atto.many' ((Atto.char '\\' *> Atto.anyChar) <|> Atto.notChar qc)
      void $ Atto.char qc
      return $ T.pack cs

csspIdent :: Atto.Parser Text
csspIdent = do
  start <- ((\c -> [c]) <$> Atto.satisfy (\c -> C.isAlpha c || c == '_')) <|> do
    c1 <- Atto.char '-'
    c2 <- Atto.satisfy (\c -> C.isAlpha c || c == '_')
    return [c1, c2]
  rest <- Atto.takeWhile (\c -> C.isAlphaNum c || c == '_' || c == '-')
  return $ T.concat [ T.pack start, rest ]

cssparseSelector :: Atto.Parser [Selector]
cssparseSelector =
    Atto.sepBy1 (Atto.skipSpace *> selpComplex id <* Atto.skipSpace) (Atto.char ',')
  where
    selpComplex cont = do
      s <- selp
      Atto.skipSpace
      mc <- Atto.peekChar
      case mc of
        Just '>' ->
          (Atto.char '>' >> Atto.skipSpace) *> selpComplex (\t -> SelChild t (cont s))
        Just '+' ->
          (Atto.char '+' >> Atto.skipSpace )*> selpComplex (\t -> SelSiblings t (cont s))
        Just c | c `L.elem` ("*:#._[" :: String) || C.isAlphaNum c ->
          selpComplex (\t -> SelDescends t (cont s))
        Just c | c `L.elem` ("{," :: String) -> return $ cont s
        Just c -> return $ warning ("selpComplex: unknown char " ++ [c]) $ cont s
        _ -> return $ cont s

    selp = do
      tag <- (Just <$> Atto.try (selpTag <|> selpAny)) <|> pure Nothing
      rest <- Atto.many' (selpId <|> selpClass <|> selpAttr <|> selpPseudoElem <|> selpPseudo)
      case maybeToList tag ++ rest of
        [] -> fail "empty selector"
        [s] -> return s
        other -> return $ SelAnd other
    selpAny = Atto.char '*' *> pure SelAny
    selpTag = (SelTag . T.pack) <$> Atto.many1 (Atto.letter <|> Atto.digit)
    selpClass = SelClass <$> (Atto.char '.' *> csspIdent)
    -- TODO: parenthesis in pseudoclasses
    selpPseudoElem =
      (SelPseudoElem . T.pack) <$> (Atto.string "::" *> Atto.many1 (Atto.letter <|> Atto.char '-'))
    selpPseudo =  -- TODO: parenthesis
      -- https://developer.mozilla.org/en-US/docs/Web/CSS/Pseudo-elements#Index_of_standard_pseudo-elements
      let pseudoChoice p =
            if p `S.member` S.fromList ["after", "before", "first-letter", "first-line",
                        "grammar-error", "marker", "placeholder", "selection", "spelling-error"]
            then SelPseudoElem p
            else SelPseudo p
      in (pseudoChoice . T.pack) <$> (Atto.char ':' *> Atto.many1 (Atto.letter <|> Atto.char '-'))
    selpId = SelId <$> (Atto.char '#' *> csspIdent)
    selpAttr = do
      void $ Atto.char '['
      attr <- csspString <|> csspIdent
      c <- Atto.anyChar
      if c == ']'
      then return $ SelHasAttr attr
      else do
        op <- case c of
          '=' -> pure (SelAttrEq attr)
          '~' -> Atto.char '=' *> pure (SelAttrWord attr)
          '|' -> Atto.char '=' *> pure (SelAttrPre attr)
          _ -> fail "selpAttr"
        val <- csspString <|> csspIdent
        void $ Atto.char ']'
        return $ op val

cssSelectors:: Text -> Either String [Selector]
cssSelectors = Atto.parseOnly (cssparseSelector <* Atto.endOfInput)

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
  , ("font-size", "1em")  -- TODO: use rem
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
