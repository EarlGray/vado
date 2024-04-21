{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Vado.Layout where

import           Control.Applicative
import           Control.Monad.RWS.Strict as RWS
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T

import           Network.URI as URI
import qualified SDL.Cairo.Canvas as Canvas
import qualified SDL.Video.Renderer as SDL
import           SDL.Vect

import           Vado.CSS
import           Vado.Document
import           Vado.Types

--------------------------------------------------------------------------------
-- | A tree of bounding boxes

type RelPos = Point V2 Double

data BoxTree = BoxTree
  { boxContent :: BoxContent
  , boxNode :: ElementID        -- backreference to its node
  , boxDim :: V2 Double         -- outer dimensions
  , boxStyling :: (StyleDiff, StyleDiff)    -- instructions for box rendering
  , boxLines :: [BoxLine]       -- lines to be drawn after the content
  }

boxHeight :: BoxTree -> Height
boxHeight BoxTree{ boxDim=V2 _ h } = h

instance Show BoxTree where
  show BoxTree{boxContent=content, boxNode=node, boxDim=dim, boxStyling=(stpush, stpop), boxLines=blns} =
    unlines (concat ["BoxTree <", name, "> : ", rshow x, "x", rshow y, dostyle, undostyle, boxlines] : contents)
    where
      dostyle = if M.null stpush then "" else " style=" ++ show (M.toList stpush)
      undostyle = if M.null stpop then "" else " unstyle=" ++ show (M.toList stpop)
      boxlines = if null blns then "" else " lines:" ++ show (length blns)
      rshow v = show (round v :: Int)
      contents = map ("  " ++ ) $ lines contents'
      contents' = case content of
        BoxInline inline -> show inline
        BoxOfBlocks children -> L.intercalate "\n" $ map show children
      V2 x y = dim
      name = "@" ++ show node

data BoxContent
  = BoxInline InlineContent
  | BoxOfBlocks [(RelPos, BoxTree)]

instance Show BoxContent where
  show (BoxInline inline) = "BoxInline " ++ show inline
  show (BoxOfBlocks blocks) = "BoxOfBlocks " ++ show blocks

-- | What to draw inside a box
data InlineContent
  = TextBox !Text !BaselineY
  | ImageBox (V2 Double) URI
  -- ^  an image of size (V2 Double) and source.
  | InputTextBox (V2 Double) OffsetX BaselineY ElementID
  -- ^ a text input with some text at OffsetX

instance Show InlineContent where
  show (TextBox t baseline) =
    "TextBox " ++ show t ++ " (baseline " ++ show baseline ++ ")"
  show (ImageBox (V2 w h) href) =
    concat ["ImageBox ", show w, "x", show h, " " ++ show href]
  show (InputTextBox (V2 w h) _textX _baseline nid) =
    concat ["TextInputContent ", show w, "x", show h, ": @", show nid]

-- return the stack of ancestor boxes, from bottom to top
findInBox :: Point V2 Double -> BoxTree -> [BoxTree]
findInBox (P xy) box0 = go [] xy box0
  where
    go stack (V2 x y) box@BoxTree{ boxContent=content, boxDim = V2 dx dy } =
      case content of
        _ | (x > dx) || (y > dy) -> stack
        BoxInline _ -> box:stack
        BoxOfBlocks posblocks ->
          case L.find (contains x y) posblocks of
            Just (P (V2 x0 y0), subbox) ->
              go (box:stack) (V2 (x - x0) (y - y0)) subbox
            Nothing ->
              box:stack
    contains px py (P (V2 bx by), BoxTree{ boxDim=V2 bw bh }) =
      (bx <= px && px <= (bx + bw)) && (by <= py && py <= (by + bh))


boxFirstBaselineY :: BoxTree -> Maybe BaselineY
boxFirstBaselineY = \case
    BoxTree{ boxContent = BoxInline content } ->
      Just $ case content of
        TextBox _ baselineY -> baselineY
        ImageBox (V2 _ dy) _ -> dy
        InputTextBox _ _ baselineY _ -> baselineY
    BoxTree{ boxContent = BoxOfBlocks posboxes } ->
      listToMaybe $ mapMaybe maybeBaseline posboxes
  where
    maybeBaseline (P (V2 _ dy), box) = (dy + ) <$> boxFirstBaselineY box

-- | BoxLine can be used for underlines, strikethroughs, borders, etc.
data BoxLine = BoxLine
  { boxlineStart :: V2 Double
  , boxlineEnd :: V2 Double
  -- , boxlineColor :: Maybe Canvas.Color
  -- , boxlineThickness :: Maybe Double
  }

--------------------------------------------------------------------------------
-- Layout engine

type Width = Double
type Height = Double
type BaselineY = Double
type OffsetX = Double

noBaseline :: BaselineY
noBaseline = -1

class Monad m => CanMeasureText m where
  measureHeightAndBaseline :: Canvas.Font -> m (Height, BaselineY)
  measureTextWidth :: Canvas.Font -> String -> m Width

instance CanMeasureText Canvas.Canvas where
  measureHeightAndBaseline font = do
    Canvas.textFont font
    extents <- Canvas.fontExtents
    let h = Canvas.fontExtentsHeight extents
    let desc = h - Canvas.fontExtentsDescent extents
    return (h, desc)

  measureTextWidth font text = do
    Canvas.textFont font
    V2 width _ <- Canvas.textSize text
    return width

-- Immutable parameters for a block layuot:
data LayoutParams = LayoutParams
  { ltWidth :: Width
  }

-- Mutable state for a block layout:
data Layout = Layout
  { ltStyle :: Style                      -- the full CSS set for the block
  , ltStyling :: (StyleDiff, StyleDiff)   -- (css-push, css-pop) for rendering
  , ltElement :: ElementID                 -- current element
  -- current coordinates relative to the containing block:
  , ltX :: Width
  , ltY :: Height
  , ltMaxX :: Width             -- record maximum block width
  , ltLS :: LineState           -- record parts of the next line box
  , ltCtx :: LayoutCtx
  }

data LineState = LS
  { lsBoxes :: [(BaselineY, BoxTree, Width, Width)]
  -- ^           baseline   box     before  after
  -- (sdl-cairo measures text with spaces before, but ignores trailing spaces)
  , lsGap :: Bool
  -- ^ was there a whitespace/newline before this chunk?
  -- current box state:
  , lsWords :: [String]
  , lsFont :: Canvas.Font
  }

-- | A global layout context for a page
data LayoutCtx = LayoutCtx
  { ltResources :: DOMResourceMap
  }

-- | The layout monad
newtype LayoutOver m a
  = LayoutOver { runLayout :: RWST LayoutParams [(RelPos, BoxTree)] Layout m a }
  deriving (
    Functor, Applicative, Monad,
    MonadReader LayoutParams,
    MonadWriter [(RelPos, BoxTree)],
    MonadState Layout, MonadTrans
  )

elementToBoxes :: CanMeasureText m =>
     LayoutCtx -> LayoutParams -> Style -> ElementRef ->
     m (BoxTree, LayoutCtx)
elementToBoxes ctx params parentStyle node = do
    let st0 = elementStylesheet $ elementDeref node
    -- TODO: fix style up if font-size is not in pixels at this point
    let st = st0 `cascadeStyle` parentStyle
    let doLayout = runLayout $ do
            forM_ (elemrefChildren node) layoutElement
            layoutLineBreak
    (layout', posboxes) <- RWS.execRWST doLayout params Layout
      { ltX = 0, ltY = 0
      , ltMaxX = 0
      , ltLS = LS { lsBoxes = [], lsGap = True, lsWords = [], lsFont = canvasFont st }
      , ltCtx = ctx
      , ltStyle = st
      , ltStyling = st `styleDiff` parentStyle
      , ltElement = elementRefID node
      }
    let box = BoxTree
         { boxContent = BoxOfBlocks posboxes
         , boxNode = elementRefID node
         , boxDim = V2 (ltMaxX layout') (ltY layout')
         , boxStyling = ltStyling layout'
         , boxLines = []
         }
    return (box, ltCtx layout')

withStyle :: CanMeasureText m => ElementRef -> LayoutOver m a -> LayoutOver m a
withStyle node action = do
    parentStyle <- gets ltStyle
    parentStyling <- gets ltStyling
    parentNode <- gets ltElement
    let st = elementStylesheet (elementDeref node) `cascadeStyle` parentStyle
    let styling = st `styleDiff` parentStyle
    modify $ \lt -> lt{ ltStyle=st, ltStyling=styling, ltElement = elementRefID node }
    result <- action
    modify $ \lt -> lt{ ltStyle=parentStyle, ltStyling=parentStyling, ltElement = parentNode }
    return result

layoutElement :: CanMeasureText m => ElementRef -> LayoutOver m ()
layoutElement elt = do
    let node = elementDeref elt
        content = elementContent node
        display = (elementStylesheet node) `stylesheetOwnValue` CSSDisplay
    if display == CSS_Keyword "none" then
      return ()
    else case (content, display) of
      (Left (TextContent txt), _) ->
        layoutText txt
      (Left NewlineContent, _) ->
        layoutLineBreak
      (Left HorizLineContent, _) -> do
        layoutLineBreak
        w <- asks ltWidth
        font <- gets (canvasFont . ltStyle)
        (h, _) <- lift $ measureHeightAndBaseline font
        let lineY = h/2
        withStyle elt $ do
          styling <- gets ltStyling
          layoutBlockBox $ BoxTree
            { boxContent = BoxInline $ TextBox "" 0
            , boxNode = elementRefID elt
            , boxDim = V2 w h
            , boxStyling = styling
            , boxLines = [BoxLine{boxlineStart=V2 0 lineY, boxlineEnd=V2 w lineY}]
            }

      (Left (InputTextContent _), _) -> do
        -- TODO: extract the font of own CSS (not the cascaded one), calculate wh and baseline
        -- TODO: scroll text if too long and truncate from left
        let w = 320
        let h = 32
        let baseline = 22
        let textX = 10
        let box = BoxInline $ InputTextBox (V2 w h) textX baseline (elementRefID elt)
        let boxlines =
              [ BoxLine{boxlineStart=V2 0 0, boxlineEnd=V2 0 h}
              , BoxLine{boxlineStart=V2 0 0, boxlineEnd=V2 w 0}
              , BoxLine{boxlineStart=V2 0 h, boxlineEnd=V2 w h}
              , BoxLine{boxlineStart=V2 w 0, boxlineEnd=V2 w h}
              ]
        withStyle elt $ layoutInlineBox (V2 w h) baseline elt box boxlines

      (Left (ImageContent href mbSize), _) -> do
        -- TODO: block <img>
        resources <- gets (ltResources . ltCtx)
        let mbResSize = (\(ImageResource wh _, _) -> wh) <$> M.lookup href resources
        case mbSize <|> mbResSize of
          Just wh -> do
            let baseline = imgBaseline wh elt
            layoutInlineBox wh baseline elt (BoxInline $ ImageBox wh href) []
          _ -> return ()

      (Right _, CSS_Keyword kw) | kw `elem` ["inline", "inline-block"] -> do
        withStyle elt $ forM_ (elemrefChildren elt) layoutElement

      (Right _, CSS_Keyword "list-item") ->
        layoutListItem elt

      (Right _, _) -> do
        when (display /= CSS_Keyword "block") $
          return $ warning ("TODO: unknown display=" ++ show display ++ ", defaulting to block") ()
        -- wrap up the previous line (if any):
        layoutLineBreak
        -- start a new containing block:
        params <- ask
        box <- sublayout elt params
        layoutBlockBox box

      -- _ -> error $ concat ["layoutBlock: display=", show display , ", element=", show (elementDeref elt)]
  where
    sublayout child params = do
      parentSt <- gets ltStyle
      ctx <- gets ltCtx
      (box, ctx') <- lift $ elementToBoxes ctx params parentSt child
      modify (\lt -> lt{ ltCtx=ctx' })
      return box

    layoutListItem child = do
      let markerText = "\x2022"   -- TODO: numbered lists
      let markerWidth = 20        -- TODO: why 20?

      params0 <- ask
      let outerWidth = ltWidth params0
      let params = params0{ ltWidth = outerWidth - markerWidth }
      box <- sublayout child params   -- layout the inner block

      font <- gets (canvasFont . ltStyle)
      (mh, mb) <- lift $ measureHeightAndBaseline font
      mw <- lift $ measureTextWidth font markerText

      let baselineY = fromMaybe mb $ boxFirstBaselineY box
      let outerHeight = max (boxHeight box) mh
      let mx = (markerWidth - mw) / 2
      let my = baselineY - mb

      let markerBox = BoxTree
            { boxContent = BoxInline $ TextBox (T.pack markerText) baselineY
            , boxNode = boxNode box
            , boxDim = V2 mw mh
            , boxStyling = noStyling
            , boxLines = []
            }

      let outerBox = BoxTree
            { boxContent = BoxOfBlocks
                [ (P (V2 mx my),            markerBox)
                  -- move styling into the outer box:
                , (P (V2 markerWidth 0.0),  box{ boxStyling = noStyling })
                ]
            , boxNode = boxNode box
            , boxDim = V2 outerWidth outerHeight
            , boxStyling = boxStyling box
            , boxLines = []
            }
      layoutBlockBox outerBox

    -- TODO: use text-align from style
    imgBaseline (V2 _ h) child =
        case M.lookup "align" (elementAttrs $ elementDeref child) of
          Just "top" -> noBaseline
          Just "middle" -> h/2
          Just "bottom" -> h
          Just other -> warning ("unknown <img align='" ++ T.unpack other ++ "'>") h
          Nothing -> h


layoutBlockBox :: CanMeasureText m => BoxTree -> LayoutOver m ()
layoutBlockBox box = do
    pos <- stackBox (boxDim box)
    tell [(P pos, box)]
  where
    stackBox (V2 dx dy) = LayoutOver $ do
      lt <- get
      put lt{ ltX = 0, ltY = ltY lt + dy, ltMaxX = max (ltMaxX lt) dx }
      return $ V2 (ltX lt) (ltY lt)

layoutText :: CanMeasureText m => Text -> LayoutOver m ()
layoutText txt = do
  st <- gets ltStyle
  gap0 <- gets (lsGap . ltLS)
  let (_gap, chunks) = chunksFromTokens (st `cssValue` CSSWhiteSpace) (gap0, textTokens txt)

  layoutInlineStart
  forM_ chunks $ \case
    "\n" -> layoutLineBreak
    " " -> layoutInlineSpace
    chunk -> layoutInlineWord chunk
  layoutInlineClose

layoutInlineStart :: CanMeasureText m => LayoutOver m ()
layoutInlineStart = do
  font <- gets (canvasFont . ltStyle)
  ls0 <- gets ltLS
  let ls = ls0{ lsWords = [], lsFont = font }
  modify $ \lt -> lt{ ltLS = ls }

layoutInlineWord :: CanMeasureText m => String -> LayoutOver m ()
layoutInlineWord chunk = do
  maxwidth <- asks ltWidth
  font <- gets (lsFont . ltLS)
  w <- lift $ measureTextWidth font chunk
  x <- gets ltX
  when (x + w >= maxwidth) $ do
    layoutLineBreak
  modify (\lt ->
    let ls = ltLS lt
    in lt{ ltX=ltX lt + w, ltLS=ls{ lsWords=lsWords ls ++ [chunk], lsGap = False } }
    )

layoutInlineSpace :: CanMeasureText m => LayoutOver m ()
layoutInlineSpace = do
  font <- gets (canvasFont . ltStyle)
  wgap <- lift $ (/ 4) <$> measureTextWidth font "____"
  modify (\lt ->
    let x = ltX lt + wgap
        ls = ltLS lt
        ls' = ls{ lsWords=lsWords ls ++ [" "], lsGap = True }
    in lt{ ltX=x, ltLS=ls' })

layoutInlineBox :: CanMeasureText m =>
                   V2 Double -> BaselineY -> ElementRef -> BoxContent -> [BoxLine]
                   -> LayoutOver m ()
layoutInlineBox (V2 dx dy) baseline node content boxlines = do
  x <- gets ltX
  maxwidth <- asks ltWidth
  when (x + dx >= maxwidth) $ do
    layoutLineBreak

  styling <- gets ltStyling
  let box = BoxTree
        { boxContent = content
        , boxNode = elementRefID node
        , boxDim = V2 dx dy
        , boxStyling = styling
        , boxLines = boxlines
        }
  modify $ \lt ->
    let ls = ltLS lt
    in lt{ ltX=ltX lt + dx, ltLS=ls{ lsWords=[], lsBoxes=lsBoxes ls ++ [(baseline, box, 0, 0)] } }

layoutInlineClose :: CanMeasureText m => LayoutOver m ()
layoutInlineClose = do
  lt <- get
  let font = canvasFont $ ltStyle lt
  let ls = ltLS lt
  let txt = concat $ lsWords ls

  unless (null txt) $ do
    -- assemble the line and measure it again
    wgap <- lift $ (/ 4) <$> measureTextWidth font "____"
    let (txt', wbefore, wafter) =
         let (before, txt0) = L.span C.isSpace txt
             wb = wgap * fromIntegral (length before)
             (after, txt1) = L.span C.isSpace (reverse txt0)
             wa = wgap * fromIntegral (length after)
         in (reverse txt1, wb, wa)

    (h, baseline) <- lift $ measureHeightAndBaseline font
    w <- lift $ measureTextWidth font txt'    -- TODO: normalize monospace

    -- underlines?
    let underlineOffset = 3         -- TODO: unhardcode
    let boxlines = case M.lookup CSSTextDecorationLine (styleInherit $ ltStyle lt) of
         Just (CSS_Keyword "underline") ->
           let underY = baseline + underlineOffset
           in [BoxLine{ boxlineStart=V2 0 underY, boxlineEnd=V2 w underY}]
         Just (CSS_Keyword "line-through") ->
           let strikeY = baseline * 0.7
           in [BoxLine{ boxlineStart=V2 0 strikeY, boxlineEnd=V2 w strikeY}]
         Just (CSS_Keyword "none") -> []
         Just other ->
           warning ("Ignoring text-decoration=" ++ show other) []
         Nothing -> []

    -- emit the inline box
    let box = BoxTree
          { boxContent = BoxInline (TextBox (T.pack txt') baseline)
          , boxNode = ltElement lt
          , boxDim = V2 w h
          , boxStyling = ltStyling lt
          , boxLines = boxlines
          }
    let ls' = ls { lsBoxes = lsBoxes ls ++ [(baseline, box, wbefore, wafter)], lsWords = [] }
    put $ lt{ ltLS = ls' }

layoutLineBreak :: CanMeasureText m => LayoutOver m ()
layoutLineBreak = do
  ws <- gets (lsWords . ltLS)
  when (nonEmpty ws) $ do
    layoutInlineClose

  boxes <- gets (lsBoxes . ltLS)
  if null boxes then do
    preserveNewine <- gets (stylePreservesNewlines . ltStyle)
    when preserveNewine $ do
      -- advance Y by a line height:
      font <- gets (canvasFont . ltStyle)
      (h, _) <- lift $ measureHeightAndBaseline font
      modify $ \lt -> lt{ ltY = ltY lt + h }
  else do
    -- determine the line ascent and descent:
    let boxVerticals (bl, box, _, _) =
          let h = boxHeight box in
          if bl == noBaseline
          then (0, 0, boxHeight box)
          else (bl, h - bl, h)
    let (ascents, descents, heights) = L.unzip3 $ map boxVerticals boxes
    let baseline = maximum ascents
    let height = max (maximum heights) (baseline + maximum descents)

    -- position horizontally:
    maxwidth <- asks ltWidth
    modify $ \lt ->
      let (_, _, _, wafter) = last $ lsBoxes $ ltLS lt
          w = ltX lt - wafter
          linestart = case M.lookup CSSTextAlign (styleInherit $ ltStyle lt) of
            Just (CSS_Keyword "right") -> maxwidth - w
            Just (CSS_Keyword "center") -> (maxwidth - w) / 2
            Just (CSS_Keyword "left") -> 0
            Just other -> warning ("text-align=" ++ show other ++ ", fallback to 'left'") 0
            Nothing -> 0
      in lt{ ltX = linestart }

    -- arrange boxes to the known baseline:
    posboxes <- forM boxes $ \(bl, box, wbefore, wafter) -> do
      x <- gets ltX
      let y0 = if bl == noBaseline then 0 else baseline - bl
      let V2 dx _ = boxDim box
      modify $ \lt -> lt{ ltX=ltX lt + wbefore + dx + wafter }
      return (P (V2 (x+wbefore) y0), box)
    width <- gets ltX
    modify $ \lt -> lt{ ltX = 0, ltMaxX = max (ltMaxX lt) width }

    -- emit the line box:
    layoutBlockBox $ BoxTree
          { boxContent = BoxOfBlocks posboxes
          , boxNode = noElement
          , boxDim = V2 width height
          , boxStyling = noStyling
          , boxLines = []
          }

  modify $ \lt -> lt{ ltX = 0, ltLS = (ltLS lt){ lsBoxes=[], lsGap=True, lsWords=[] } }


-- Whitespace and tokens:

textTokens :: Text -> [String]
textTokens = go . T.unpack
  where
    go "" = []
    go ('\n':t) = "\n" : go t
    go (c:t) | C.isSpace c = " " : go t
    go t = let (word, t') = L.break C.isSpace t in word : go t'

chunksFromTokens :: CSSValue -> (Bool, [String]) -> (Bool, [String])
chunksFromTokens (CSS_Keyword "normal") (gap, ts) = (gap', reverse chunks)
  where
    (gap', chunks) = L.foldl' go (gap, []) ts
    go (True, buf) " " = (True, buf)
    go (True, buf) "\n" = (True, buf)
    go (False, buf) " " = (True, " " : buf)
    go (False, buf) "\n" = (True, " " : buf)
    go (_, buf) t = (False, t : buf)

chunksFromTokens (CSS_Keyword "pre") (gap, tokens) = (gap', lns)
  where
    gap' = if null lns then gap else let ln = last lns in ln == "\n" || C.isSpace (last ln)
    lns = go ([], []) tokens
    go (buf, res) [] = res ++ (if null buf then [] else [concat buf])
    go (buf, res) ("\n":ts) = go ([], res ++ (if null buf then [] else [concat buf]) ++ ["\n"]) ts
    go (buf, res) (t:ts) = go (buf ++ [t], res) ts

-- TODO: properly:
chunksFromTokens (CSS_Keyword "nowrap") (gap0, tokens) = (gap, lns)
  where
    (gap, ts) = chunksFromTokens (CSS_Keyword "normal") (gap0, tokens)
    lns = let ln = concat ts in if L.null ln then [] else [ln]

chunksFromTokens whitespace ts = chunksFromTokens (warning msg $ CSS_Keyword "normal") ts
  where msg = "TODO: chunksFromTokens fallback to 'normal' instead of: " ++ show whitespace

--------------------------------------------------------------------------------
-- Rendering engine

renderTree :: Document -> (Double, Double) -> (Double, Double, Style) -> BoxTree
           -> Canvas.Canvas [(SDL.Rectangle Double, InlineContent)]
renderTree doc (minY, maxY) (x, y, st0) box = do
  -- draw the lines
  forM_ (boxLines box) $ \boxline -> do
    let V2 dx1 dy1 = boxlineStart boxline
    let V2 dx2 dy2 = boxlineEnd boxline
    let start = V2 (x + dx1) (y + dy1 - minY)
    let end = V2 (x + dx2) (y + dy2 - minY)
    Canvas.line start end

  -- draw the content
  case boxContent box of
    BoxOfBlocks children -> do
      replaced <- forM children $ \(P (V2 dx dy), child) -> do
        if not ((y+dy) + boxHeight child < minY || maxY < (y+dy)) then
          withStyling child (x+dx, y+dy-minY, st0) $ \st ->
            renderTree doc (minY, maxY) (x + dx, y + dy, st) child
        else
          return []
      return $ concat replaced

    BoxInline (TextBox txt baseline) -> do
      Canvas.textBaseline (T.unpack txt) (V2 x (y + baseline - minY))

      return []
    BoxInline content@(ImageBox (V2 w h) _) ->
      let rect = SDL.Rectangle (P $ V2 x y) (V2 w h)
      in return [(rect, content)]

    BoxInline (InputTextBox (V2 _bw bh) textX baseline nid) -> do
      node <- inDocument doc $ getElement nid

      let Left (InputTextContent txt) = elementContent node
      V2 w _ <- Canvas.textSize (T.unpack txt)
      Canvas.textBaseline (T.unpack txt) (V2 (x + textX) (y + baseline - minY))

      V2 cursorOffsetX _ <- Canvas.textSize "."
      let cursorInsetY = bh/6
      let cursorX = textX + w + cursorOffsetX
      let cursorTop = V2 (x + cursorX) (y + cursorInsetY - minY)
      let cursorBottom = V2 (x + cursorX) (y + bh - cursorInsetY - minY)
      Canvas.line cursorTop cursorBottom
      return []
    -- _ -> error $ "TODO: renderTree " ++ show (boxContent box)

withStyling :: BoxTree -> (Double, Double, Style) -> (Style -> Canvas.Canvas a) -> Canvas.Canvas a
withStyling BoxTree{boxDim=V2 w h, boxStyling=(stpush, stpop)} (x, y, st0) action = do
    let st = st0 `applyDiff` stpush
    whenJust (M.lookup CSSBackgroundColor stpush) $ applyBackgroundColor st  -- only on push!
    applyStyling st stpush
    ret <- action st
    applyStyling st0 stpop
    return ret
  where
    applyStyling :: Style -> StyleDiff -> Canvas.Canvas ()
    applyStyling st diff = do
      when (any (`M.member` diff) [CSSFontFamily, CSSFontSize, CSSFontStyle, CSSFontWeight]) $
        Canvas.textFont $ canvasFont st
      forM_ (M.toList diff) $ \case
        (CSSColor, CSS_RGB r g b) ->
          Canvas.stroke $ Canvas.rgb r g b
        (CSSBackgroundColor, _) ->
          return ()
        other ->
          return $ warning ("stpush property ignored: " ++ show other) ()

    applyBackgroundColor st = \case
      (CSS_RGB r g b) -> do
        Canvas.stroke $ Canvas.rgb r g b
        Canvas.fill $ Canvas.rgb r g b
        Canvas.rect $ Canvas.D x y w h
        case st `cssValueMaybe` CSSColor of
          Just (CSS_RGB r' g' b') -> Canvas.stroke $ Canvas.rgb r' g' b'
          Just other -> logWarn ("unknown stroke color: " ++ show other)
          Nothing -> logWarn "no stroke color set, weird"
      (CSS_Keyword "transparent") -> return ()
      other -> logWarn ("Unknown background color: " ++ show other)

canvasFont :: Style -> Canvas.Font
canvasFont st = Canvas.Font face size weight italic
  where
    face =
      case st `cssValueMaybe` CSSFontFamily of
        Just (CSS_String name) -> T.unpack name
        Just (CSS_Keyword name) -> T.unpack name
        Just other -> error $ "canvasFont: unknown font-family=" ++ show other
        Nothing -> defaultFontFace
    size =
      case st `cssValueMaybe` CSSFontSize of
        Just (CSS_Px px) -> px
        --Just other -> warning ("canvasFont: unknown font-size=" ++ show other) defaultFontSize
        _ -> defaultFontSize
    weight = (st `cssValueMaybe` CSSFontWeight == Just (CSS_Keyword "bold"))
    italic = (st `cssValueMaybe` CSSFontStyle == Just (CSS_Keyword "italic"))
