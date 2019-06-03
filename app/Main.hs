{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import           Control.Monad
import           Control.Monad.RWS.Strict as RWS
import qualified Data.ByteString.Lazy as B
import qualified Data.Char as C
--import           Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.String
import           Data.Text ( Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Linear.V2 (V2(..))
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Client.Internal
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Header as HTTP
import           Network.URI
import qualified SDL as SDL
import qualified SDL.Cairo as Cairo
import qualified SDL.Cairo.Canvas as Canvas
import           SDL.Event as SDL
import           SDL.Vect
import           System.Environment
import qualified Text.HTML.DOM as HTML
import qualified Text.XML as XML

-- import Debug.Trace

debug :: Bool
debug = False

--------------------------------------------------------------------------------
-- page, DOM and resources

-- | State of the page
data Page = Page
  { pageBody :: DOMNode
  , pageHead :: XML.Element
  --, pageElementById :: M.Map Text DOM
  --, pageFocusable :: Zipper DOM
  --, pageForms :: M.Set DOM
  , pageResources :: M.Map URI HTTPResource
  , pageBoxes :: Maybe BoxTree
  , pageScroll :: Double
  , pageWindow :: VadoWindow
  }

data HTTPResource {-= HTTPResource
  { resContentType :: Text
  , resContentLength :: Integer
  , resContentData :: B.ByteString
  -}

data DOMNode = DOM
  { domContent :: Either DOMContent [DOMNode]
  -- ^ Either a leaf element or a list of children
  , domStyle :: Style
  , domEvents :: Events
  , domAttrs :: M.Map Text Text
  , domSource :: Maybe XML.Node
  -- ^ Backreference to the XML source of this DOM node
  }

class HasDebugView a where
  showdbg :: a -> String

instance HasDebugView DOMNode where
  showdbg dom = unlines $ [name] ++ map (replicate 2 ' ' ++) ([attrs, style] ++ lines contents)
    where
      xmlName (XML.NodeElement el) = T.unpack $ tagName el
      xmlName (XML.NodeContent _) = "<text>"
      xmlName _ = "<?>"

      xmlAttrs (XML.NodeElement el) = show $ XML.elementAttributes el
      xmlAttrs _ = "{}"

      name = maybe "<anon>" xmlName (domSource dom)
      attrs = ":attrs " ++ maybe "{}" xmlAttrs (domSource dom)
      style = ":style " ++ showdbg (domStyle dom)
      contents = case domContent dom of
                  Right children -> concat $ map showdbg children
                  Left content -> show content

-- TODO: rename nodeTemplate
defaultNode :: DOMNode
defaultNode = DOM
  { domContent = Right []
  , domStyle = noStyle
  , domEvents = Events
  , domAttrs = M.empty
  , domSource = Nothing
  }

makeTextNode :: Text -> DOMNode
makeTextNode txt = defaultNode { domContent = Left (TextContent txt) }

makeNode :: [DOMNode] -> DOMNode
makeNode children = defaultNode { domContent = Right children }

withStyleFor :: Text -> DOMNode -> DOMNode
withStyleFor name node =
  case HM.lookup name elementStyles of
    Just st -> node { domStyle = st }
    Nothing -> node

-- | Content of a leaf DOM node,
-- i.e. what is to be drawn in this node.
-- either a block or inline element, e.g. some text.
data DOMContent
  = TextContent Text
  -- ^ A text node
  | NewlineContent
  -- ^ Force newline here, usually corresponds to <br>

instance Show DOMContent where
  show (TextContent txt) = T.unpack $ T.concat ["TextContent \"", txt, "\""]
  show NewlineContent = "NewlineContent"

-- | A tree of bounding boxes
type RelPos = Point V2 Double

data BoxTree = BoxTree
  { boxContent :: BoxContent
  , boxNode :: Maybe DOMNode -- backreference to its node
  , boxDim :: V2 Double      -- outer dimensions
  , boxStyling :: (StyleDiff, StyleDiff)    -- instructions for box rendering
  }

instance Show BoxTree where
  show BoxTree{boxContent=content, boxNode=node, boxDim=dim, boxStyling=(stpush, stpop)} =
    unlines (concat ["BoxTree <", name, "> : ", rshow x, "x", rshow y, dostyle, undostyle] : contents)
    where
      dostyle = if M.null stpush then "" else " style=" ++ (show $ M.toList stpush)
      undostyle = if M.null stpop then "" else " unstyle=" ++ (show $ M.toList stpop)
      rshow v = show (round v :: Int)
      contents = map ("  " ++ ) $ lines contents'
      contents' = case content of
        BoxInline inline -> show inline
        BoxOfBlocks children -> L.intercalate "\n" $ map show children
      V2 x y = dim
      name = case node of
        Just (DOM { domSource=(Just (XML.NodeElement el)) }) -> T.unpack $ tagName el
        _ -> "-"

data BoxContent
  = BoxInline InlineContent
  | BoxOfBlocks [(RelPos, BoxTree)]

instance Show BoxContent where
  show (BoxInline inline) = "BoxInline " ++ show inline
  show (BoxOfBlocks blocks) = "BoxOfBlocks " ++ show blocks
  --show (BoxOfLines _) = "BoxOfLines: TODO"

-- | What to draw inside a box
data InlineContent
  = TextBox !Text !Double
  | ImageBox !(Maybe (V2 Double)) SDL.Surface   -- an image

instance Show InlineContent where
  show (TextBox t baseline) = "TextBox " ++ show t ++ " (baseline " ++ show baseline ++ ")"
  show (ImageBox (Just (V2 w h)) _) =   concat ["ImageBox ", show w, "x", show h, " <image>"]
  show (ImageBox Nothing _) =           "ImageBox ?x? <image>"

{-
findBox :: BoxTree -> Point V2 Double -> Maybe BoxTree
findBox = undefined
-}


-- | A set of events that an element may handle.
data Events = Events

--------------------------------------------------------------------------------
-- Main entry point and event handler

-- | Main entry point.
--
-- * Read in commandline argument with the URL.
-- * Make a request to the URL.
-- * Create an SDL window.
-- * Render the content.
--
main :: IO ()
main = do
  args <- getArgs
  let url = if null args then "https://en.wikipedia.org" else head args
  (phead, pbody, resources) <- fetchURL url

  window <- vadoWindow

  let page0 = Page {
      pageBody = pbody
    , pageHead = phead
    , pageResources = resources
    , pageBoxes = Nothing
    , pageScroll = 0
    , pageWindow = window
    }
  when debug $ putStrLn (showdbg pbody)
  page <- layoutPage page0
  vadoRedrawEventLoop page


--------------------------------------------------------------------------------
-- UI: rendering and events

data VadoWindow = VadoWindow
  { vadoRenderer :: SDL.Renderer
  , vadoTexture :: SDL.Texture
  , vadoViewport :: V2 Double
  , vadoScale :: V2 Double
  }

pageViewport :: Page -> V2 Double
pageViewport = vadoViewport . pageWindow

defaultWindowSize :: Num n => V2 n
defaultWindowSize = V2 800 600

-- | Create window and texture
vadoWindow :: IO VadoWindow
vadoWindow = do
  setEnv "SDL_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR" "0"
  SDL.initialize [SDL.InitVideo, SDL.InitTimer, SDL.InitEvents]
  window <-
    SDL.createWindow
      "Vado"
      SDL.defaultWindow
      { SDL.windowHighDPI = True
      , SDL.windowResizable = True
      , SDL.windowInitialSize = defaultWindowSize
      }
  -- Setup Cairo rendering on the window.
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  Just (SDL.Rectangle _ viewport@(V2 w h)) <-
    SDL.get (SDL.rendererViewport renderer)
  let scale = V2 1.3 1.3
        --let (V2 w0 h0) = defaultWindowSize
        --in V2 (fromIntegral w / w0) (fromIntegral h / h0)
  when debug $ print scale
  texture0 <- Cairo.createCairoTexture renderer viewport
  return $ VadoWindow
    { vadoRenderer = renderer
    , vadoViewport = V2 (fromIntegral w) (fromIntegral h)
    , vadoScale = scale
    , vadoTexture = texture0
    }

-- | Event loop.
vadoEventLoop :: Page -> IO ()
vadoEventLoop page = do
  event <- waitEvent
  case eventPayload event of
    QuitEvent -> return ()
    WindowClosedEvent {} -> return ()
    MouseButtonEvent e ->
      case mouseButtonEventMotion e of
        {-
        Released -> do
          let xy = let P (V2 w h) = mouseButtonEventPos e
                   in P (V2 (fromIntegral w * scalex) (fromIntegral h * scaley))
          case findBox (fromJust $ evBoxes ev) xy of
            Just box ->
              case eventsClick $ boxEvents box of
                Just handler ->
                  let loadUrl uri = do
                        request' <- setUriRelative (evRequest ev) uri
                        putStrLn ("Downloading: " ++ show request')
                        content' <- getContent request'
                        let scrollY' = 0
                        ev' <- layoutBoxes scale (ev {evContent = content', evScrollY = 0})
                        renderBoxes ev'
                        eloop scale ev'
                  in handler loadUrl (eloop scale ev)
            _ -> eloop scale ev
            -}
        _ -> vadoEventLoop page
    MouseWheelEvent e -> do
      let V2 _ dy = mouseWheelEventPos e
      let V2 _ viewH = pageViewport page
      let pageH = maybe viewH (\b -> let V2 _ boxH = boxDim b in boxH) $ pageBoxes page
      let scrollY = min (max 0 (pageH - viewH)) $ max 0 (pageScroll page - 5 * fromIntegral dy)
      vadoRedrawEventLoop $ page{ pageScroll=scrollY }
    WindowResizedEvent e -> do
      let size = windowResizedEventSize e
      let win = pageWindow page
      texture' <- Cairo.createCairoTexture (vadoRenderer win) (fromIntegral <$> size)
      let win' = win{ vadoTexture=texture', vadoViewport=(fromIntegral <$> size) }
      -- TODO: optimize, don't do full layout again:
      page' <- layoutPage page{ pageWindow=win' }
      vadoRedrawEventLoop page'
    _ -> do
      vadoEventLoop page

vadoRedrawEventLoop :: Page -> IO ()
vadoRedrawEventLoop page = renderDOM page >> vadoEventLoop page

--------------------------------------------------------------------------------
-- HTTP request, caches and local storage

fetchURL :: String -> IO (XML.Element, DOMNode, M.Map URI HTTPResource)
fetchURL pageURL = do
  pageReq <- makeRequest pageURL
  httpman <- HTTP.newManager TLS.tlsManagerSettings
  pageResp <- HTTP.httpLbs pageReq httpman
  let headers = responseHeaders pageResp

  let contentType = T.toLower $ maybe "text/html" (T.decodeLatin1 . snd) $ L.find (\(name, _) -> name == "Content-Type") headers

  if "text/html" `T.isPrefixOf` contentType then do
    let root = XML.documentRoot (HTML.parseLBS (HTTP.responseBody pageResp))
    let topnodes = mapMaybe (\case XML.NodeElement el -> Just el; _ -> Nothing) $ XML.elementNodes root
    let headnode = fromMaybe (fakeNode "head" []) $ L.find (\el -> tagName el == "head") topnodes
    let bodynode = fromMaybe (fakeNode "body" $ XML.elementNodes root) $ L.find (\el -> tagName el == "body") topnodes
    let Just body = domFromXML $ XML.NodeElement bodynode
    return (headnode, body, M.empty)
  else if "text/" `T.isPrefixOf` contentType then do
    let respbody = T.decodeUtf8 $ B.toStrict $ responseBody pageResp
    let text = makeTextNode respbody
    let pre = withStyleFor "pre" $ makeNode [text]
    return $ fakePage [pre]
  else if "image/" `T.isPrefixOf` contentType then do
    let img = undefined
    return $ fakePage [img]
  else
    error $ "TODO: Content-Type " ++ (T.unpack contentType)
  where
    fakePage elements = (fakeNode "head" [], withStyleFor "body" (makeNode elements), M.empty)
    fakeNode name nodes = XML.Element (XML.Name name Nothing Nothing) M.empty nodes

makeRequest :: String -> IO Request
makeRequest url = do
  request0 <- HTTP.parseRequest (fromString url)
  let req = request0 { requestHeaders = [ (HTTP.hUserAgent, "github.com/chrisdone/vado") ] }
  when debug $ print req
  return req

--------------------------------------------------------------------------------


-- | Normalize an XML tree of elements and text, possibly with
-- attributes like style and event handlers.
domFromXML :: XML.Node -> Maybe DOMNode
domFromXML xml@(XML.NodeContent txt) =
    Just $ DOM (Left (TextContent txt)) noStyle Events M.empty (Just xml)
domFromXML xml@(XML.NodeElement el) =
    Just $ DOM (Right children) style Events attrs (Just xml)
  where
    children = mapMaybe domFromXML $ XML.elementNodes el
    attrs = M.mapKeys (XML.nameLocalName) $ XML.elementAttributes el

    builtin_style = fromMaybe noStyle $ HM.lookup (tagName el) elementStyles
    attr_style = fromMaybe noStyle $ decodeXMLAttribute "style" el
    style = attr_style `overriding` builtin_style
domFromXML _ = Nothing


--------------------------------------------------------------------------------
-- Layout engine

type Width = Double
type Height = Double
type BaselineY = Double

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

-- | The layout monad
newtype LayoutOver m a
  = LayoutOver { runLayout :: RWST LayoutParams [(RelPos, BoxTree)] Layout m a }
  deriving (
    Functor, Applicative, Monad,
    MonadReader LayoutParams,
    MonadWriter [(RelPos, BoxTree)],
    MonadState Layout, MonadTrans
  )

stackBox :: CanMeasureText m => V2 Double -> LayoutOver m (V2 Double)
stackBox (V2 _dx dy) = LayoutOver $ do
  lt <- get
  put lt{ ltX = 0, ltY = ltY lt + dy }
  return $ V2 (ltX lt) (ltY lt)

-- | Entry point for layout procedure
layoutPage :: Page -> IO Page
layoutPage page@Page{..} = do
    (boxes, _ctx) <- Canvas.withCanvas texture $ elementToBoxes LayoutCtx params noStyle pageBody
    when debug $ print boxes
    return page{ pageBoxes=Just boxes }
  where
    params = LayoutParams { ltWidth = w }
    VadoWindow { vadoViewport = V2 w _, vadoTexture = texture } = pageWindow


elementToBoxes :: CanMeasureText m =>
     LayoutCtx -> LayoutParams -> Style -> DOMNode ->
     m (BoxTree, LayoutCtx)
elementToBoxes ctx params parentStyle node@DOM{ domContent=Right children } = do
    (layout', posboxes) <- RWS.execRWST (runLayout (layoutBlock children >> layoutLineBreak)) params layout
    let box = BoxTree
         { boxContent = BoxOfBlocks posboxes
         , boxNode = Just node
         , boxDim = V2 (ltMaxX layout') (ltY layout')
         , boxStyling = ltStyling layout'
         }
    return (box, ltCtx layout')
  where
    st = domStyle node `cascadingOver` parentStyle
    layout = Layout
      { ltX = 0, ltY = 0
      , ltMaxX = 0
      , ltLS = LS { lsBoxes = [], lsGap = True, lsWords = [], lsFont = defaultFont }
      , ltCtx = ctx
      , ltStyle = st
      , ltStyling = parentStyle `styleDiff` st
      }
elementToBoxes _ctx _par _st node = error $ "elementToBoxes (" ++ showdbg node ++ ")"

withStyle :: CanMeasureText m => DOMNode -> LayoutOver m a -> LayoutOver m a
withStyle node action = do
    parentStyle <- gets ltStyle
    parentStyling <- gets ltStyling
    let st = domStyle node `cascadingOver` parentStyle
    let styling = parentStyle `styleDiff` st
    modify $ \lt -> lt{ ltStyle=st, ltStyling=styling }
    result <- action
    modify $ \lt -> lt{ ltStyle=parentStyle, ltStyling=parentStyling }
    return result

layoutBlock :: CanMeasureText m => [DOMNode] -> LayoutOver m ()
layoutBlock children = do
  parentSt <- gets ltStyle
  forM_ children $ \child@DOM{ domContent=content, domStyle=st } -> do
    case (content, styleDisplay st) of
      (_, NoneDisplay) ->
        return ()
      (Left (TextContent txt), _) ->
        layoutText txt
      (Left NewlineContent, _) ->
        layoutLineBreak
      (Right children', InlineDisplay) ->
        withStyle child $ layoutBlock children'
      (Right _, BlockDisplay) -> do
        layoutLineBreak
        params <- ask
        ctx <- gets ltCtx
        (box, ctx') <- lift $ elementToBoxes ctx params parentSt child
        modify (\lt -> lt{ ltCtx=ctx' })
        pos <- stackBox (boxDim box)
        tell [(P pos, box)]
      _ ->
        error $ "layoutBlock " ++ showdbg child

layoutText :: CanMeasureText m => Text -> LayoutOver m ()
layoutText txt = do
  st <- gets ltStyle
  gap0 <- gets (lsGap . ltLS)
  let (_gap, chunks) = chunksFromTokens (styleWhiteSpace st) (gap0, textTokens txt)

  layoutInlineStart
  forM_ chunks $ \case
    "\n" -> layoutLineBreak
    " " -> layoutInlineSpace
    chunk -> layoutInlineWord chunk
  layoutInlineClose

layoutInlineStart :: CanMeasureText m => LayoutOver m ()
layoutInlineStart = do
  font <- gets (styleFont . ltStyle)
  ls0 <- gets ltLS
  let ls = ls0{ lsWords = [], lsFont = font }
  modify $ \lt -> lt{ ltLS = ls }

layoutInlineWord :: CanMeasureText m => String -> LayoutOver m ()
layoutInlineWord chunk = do
  maxwidth <- asks ltWidth
  font <- gets (lsFont . ltLS)
  x <- gets ltX
  w <- lift $ measureTextWidth font chunk
  when (x + w >= maxwidth) $ do
    layoutLineBreak
  modify (\lt ->
    let ls = ltLS lt
    in lt{ ltX=ltX lt + w, ltLS=ls{ lsWords=lsWords ls ++ [chunk], lsGap = False } }
    )

layoutInlineSpace :: CanMeasureText m => LayoutOver m ()
layoutInlineSpace = do
  fontsize <- gets (styleFontSize . ltStyle)
  modify (\lt ->
    let x = ltX lt + fontsize/2    -- not exactly, but no other way to measure
        ls = ltLS lt
        ls' = ls{ lsWords=lsWords ls ++ [" "], lsGap = True }
    in lt{ ltX=x, ltLS=ls' })

layoutInlineClose :: CanMeasureText m => LayoutOver m ()
layoutInlineClose = do
  lt <- get
  let font = styleFont $ ltStyle lt
  let ls = ltLS lt
  let txt = concat $ lsWords ls

  unless (null txt) $ do
    wgap <- lift $ (/ 4) <$> measureTextWidth font "____"
    let (txt', wbefore, wafter) =
         let (before, txt0) = L.break (not . C.isSpace) txt
             wb = wgap * fromIntegral (length before)
             (after, txt1) = L.break (not . C.isSpace) (reverse txt0)
             wa = wgap * fromIntegral (length after)
         in (reverse txt1, wb, wa)

    (h, baseline) <- lift $ measureHeightAndBaseline font
    w <- lift $ measureTextWidth font txt'    -- TODO: normalize monospace
    let box = BoxTree
          { boxContent = BoxInline (TextBox (T.pack txt') baseline)
          , boxNode = Nothing
          , boxDim = V2 w h
          , boxStyling = ltStyling lt
          }
    let ls' = ls { lsBoxes = lsBoxes ls ++ [(baseline, box, wbefore, wafter)], lsWords = [] }
    put $ lt{ ltLS = ls' }

layoutLineBreak :: CanMeasureText m => LayoutOver m ()
layoutLineBreak = do
  ws <- gets (lsWords . ltLS)
  when (not (null ws)) $ do
    layoutInlineClose

  bs <- gets (lsBoxes . ltLS)
  if null bs then do
    whsp <- gets (styleWhiteSpace . ltStyle)
    when (whsp == WhiteSpacePre) $ do
      -- advance Y by a line height:
      font <- gets (styleFont . ltStyle)
      (_, h) <- lift $ measureHeightAndBaseline font
      modify $ \lt -> lt{ ltY = ltY lt + h }
  else do
    -- determine the line ascent and descent:
    let (ascents, descents) = unzip $ map (\(bl, box, _, _) -> let V2 _ h = boxDim box in (bl, h - bl)) bs
    let baseline = maximum ascents
    let height = baseline + maximum descents

    -- arrange boxes to the known baseline:
    modify $ \lt -> lt{ ltX = 0 }
    posboxes <- forM bs $ \(bl, box, wbefore, wafter) -> do
      let V2 dx _ = boxDim box
      x <- gets ltX
      modify $ \lt -> lt{ ltX=ltX lt + wbefore + dx + wafter }
      return (P (V2 (x+wbefore) (baseline - bl)), box)
    width <- gets ltX
    modify $ \lt -> lt{ ltMaxX = max (ltMaxX lt) width }

    -- emit the line box:
    let linebox = BoxTree
          { boxContent = BoxOfBlocks posboxes
          , boxNode = Nothing
          , boxDim = V2 width height
          , boxStyling = noStyling
          }
    modify $ \lt -> lt{ ltX = 0 }
    pos <- stackBox (boxDim linebox)
    tell [(P pos, linebox)]

  modify $ \lt -> lt{ ltX = 0, ltLS = (ltLS lt){ lsBoxes=[], lsGap=True, lsWords=[] } }


-- Whitespace and tokens:

textTokens :: Text -> [String]
textTokens = go . T.unpack
  where
    go "" = []
    go ('\n':t) = "\n" : go t
    go (c:t) | C.isSpace c = " " : go t
    go t = let (word, t') = L.break C.isSpace t in word : go t'

chunksFromTokens :: WhiteSpace -> (Bool, [String]) -> (Bool, [String])
chunksFromTokens WhiteSpaceNormal (gap, ts) = (gap', reverse chunks)
  where
    (gap', chunks) = L.foldl' go (gap, []) ts
    go (True, buf) " " = (True, buf)
    go (True, buf) "\n" = (True, buf)
    go (False, buf) " " = (True, " " : buf)
    go (False, buf) "\n" = (True, " " : buf)
    go (_, buf) t = (False, t : buf)

chunksFromTokens WhiteSpacePre (gap, tokens) = (gap', lns)
  where
    gap' = if null lns then gap else let ln = last lns in ln == "\n" || C.isSpace (last ln)
    lns = go ([], []) tokens
    go (buf, res) [] = res ++ (if null buf then [] else [concat buf])
    go (buf, res) ("\n":ts) = go ([], res ++ (if null buf then [] else [concat buf]) ++ ["\n"]) ts
    go (buf, res) (t:ts) = go (buf ++ [t], res) ts

-- Tests

run_tests :: (Eq b, Show b) => (a -> b) -> [(b, a)] -> IO ()
run_tests f = mapM_ (\(want, got) -> putStrLn $ concat ["want: ", show want, "\t got: ", show got] ) . mapMaybe (test_one f)

test_one :: Eq b => (a -> b) -> (b, a) -> Maybe (b, b)
test_one f (want, arg) = let got = f arg in if got == want then Nothing else Just (want, got)

test_textTokens :: IO ()
test_textTokens = run_tests textTokens [
    ([],                            "")
  , (["hello"],                     "hello")
  , ([" ", "world", " "],           " world ")
  , (["hello", " ", "world", " "],  "hello world ")
  , (["hello", " ", " ", "world"],  "hello  world")
  , (["hello", " ", "\n", "world", "\n"],    "hello \nworld\n")
  ]

test_chunksFromTokens_Normal :: IO ()
test_chunksFromTokens_Normal = run_tests (chunksFromTokens WhiteSpaceNormal) [
    ((False, []),               (False, []))
  , ((True, []),                (True, []))
  , ((True, [" "]),                (False, [" "]))
  , ((True, []),                (True, ["\n"]))
  , ((True, [" "]),                (False, [" ", " ", " "]))
  , ((True, ["hello", " "]),         (False, ["hello", " ", " "]))
  , ((False, [" ", "hello"]),   (False, [" ", "hello"]))
  , ((False, ["hello"]),        (True, [" ", "hello"]))
  , ((True, ["hello", " "]),        (True, ["\n", "hello", "\n"]))
  , ((True, [" ", "hello", " "]),   (False, ["\n", "hello", "\n"]))
  ] -- want                       args
test_chunksFromTokens_Pre :: IO ()
test_chunksFromTokens_Pre = run_tests (chunksFromTokens WhiteSpacePre) [
    ((False, []),               (False, []))
  , ((True, []),                (True, []))
  , ((True, ["\n", "\n"]),      (True, ["\n", "\n"]))
  , ((False, ["hello"]),        (False, ["hello"]))
  , ((False, ["hello"]),        (False, ["hello"]))
  , ((True, ["hello "]),        (False, ["hello", " "]))
  , ((True, ["hello ", "\n"]),  (False, ["hello", " ", "\n"]))
  , ((False, ["  hello ", "\n", "world"]),             (False, [" ", " ", "hello", " ", "\n", "world"]))
  , ((True, ["  hello ", "\n", "world", "\n"]),             (False, [" ", " ", "hello", " ", "\n", "world", "\n"]))
  , ((True, ["hello  world ", "\n", "world", "\n", "\n"]),  (False, ["hello", " ", " ", "world", " ", "\n", "world", "\n", "\n"]))
  ]

--------------------------------------------------------------------------------
-- Rendering engine

renderDOM :: Page -> IO ()
renderDOM page = do
  let minY = pageScroll page
  let V2 _ viewY = pageViewport page
  let (texture, renderer) = let win = pageWindow page in (vadoTexture win, vadoRenderer win)
  Canvas.withCanvas texture $ do
    let body = fromJust $ pageBoxes page
    Canvas.background $ Canvas.rgb 255 255 255 -- TODO: set to body background color
    withStyling body noStyle $ \st ->
      renderTree (minY, minY + viewY) (0, 0, st) body
  SDL.copy (vadoRenderer $ pageWindow page) texture Nothing Nothing
  SDL.present renderer

renderTree :: (Double, Double) -> (Double, Double, Style) -> BoxTree -> Canvas.Canvas ()
renderTree (minY, maxY) (x, y, st0) box = do
  case boxContent box of
    BoxOfBlocks children ->
      forM_ children $ \(P (V2 dx dy), child) -> do
        let V2 _ boxH = boxDim child
        unless ((y+dy) + boxH < minY || maxY < (y+dy)) $ do
          withStyling child st0 $ \st ->
            renderTree (minY, maxY) (x + dx, y + dy, st) child
    BoxInline (TextBox txt baseline) ->
      Canvas.textBaseline (T.unpack txt) (V2 x (y + baseline - minY))
    _ -> error $ "TODO: renderTree " ++ show (boxContent box)

withStyling :: BoxTree -> Style -> (Style -> Canvas.Canvas a) -> Canvas.Canvas a
withStyling BoxTree{boxStyling=(stpush, stpop)} st0 action = do
    let st = st0 `applyDiff` stpush
    applyStyling st stpush
    ret <- action st
    applyStyling st0 stpop
    return ret
  where
    applyStyling :: Style -> StyleDiff -> Canvas.Canvas ()
    applyStyling st diff = do
      forM_ (M.toList diff) $ \(prop, val) -> do
        if "font-" `T.isPrefixOf` prop then
          Canvas.textFont $ styleFont st
        else case prop of
          "color" -> Canvas.stroke $ fromMaybe defaultColor $ cssColor (T.unpack val)
          _ -> return ()


--------------------------------------------------------------------------------
-- CSS styles

-- | Style for an element, consists of own and inheritable properties.
-- Must be complete (e.g. not just style diff)
-- No types for now, just stringly-typed prototypes.
data Style = Style
  { styleOwn :: M.Map Text Text
  , styleInherit :: M.Map Text Text
  }

-- A difference in styles
type StyleDiff = M.Map Text Text

instance HasDebugView Style where
  showdbg Style{..} =
    T.unpack $ T.intercalate "; " $ map showprop (M.toAscList styleOwn ++ M.toAscList styleInherit)
    where showprop (prop, val) = T.concat [prop, ": ", val]

instance Show Style where
  show = showdbg

instance Read Style where
  readsPrec _ s = [(cssFarcer s, "")]

-- | Helper CSS types
data Display = NoneDisplay | BlockDisplay | InlineDisplay
  deriving (Show, Eq)

styleDisplay :: Style -> Display
styleDisplay Style{..} =
  case M.lookup "display" styleOwn of
    Just "none" -> NoneDisplay
    Just "inline" -> InlineDisplay
    _ -> BlockDisplay

data FontStyle = NormalStyle | ItalicStyle
  deriving (Show, Eq)

styleFontStyle :: Style -> FontStyle
styleFontStyle Style{..} =
  case M.lookup "font-style" styleInherit of
    Just "italic" -> ItalicStyle
    _ -> defaultFontStyle

defaultFontStyle :: FontStyle
defaultFontStyle = NormalStyle

data FontWeight = NormalWeight | BoldWeight
  deriving (Show, Eq, Enum)

defaultWeight :: FontWeight
defaultWeight = NormalWeight

styleFontWeight :: Style -> FontWeight
styleFontWeight Style{..} =
  case M.lookup "font-weight" styleInherit of
    Just "bold" -> BoldWeight
    _ -> defaultWeight


data WhiteSpace = WhiteSpaceNormal | WhiteSpacePre
  deriving (Show, Eq)

styleWhiteSpace :: Style -> WhiteSpace
styleWhiteSpace Style{..} =
  case M.lookup "white-space" styleInherit of
    Just "pre" -> WhiteSpacePre
    _ -> WhiteSpaceNormal

styleFontFamily :: Style -> String
styleFontFamily Style{..} =
  case M.lookup "font-family" styleInherit of
     Just val -> T.unpack val
     _ -> defaultFontFace

defaultFontFace :: String
defaultFontFace = "Noto Serif"

defaultFontFaceMono :: String
defaultFontFaceMono = "Noto Mono"

styleFontSize :: Style -> Double
styleFontSize Style{..} =
  fromMaybe defaultFontSize $ (mbRead . T.unpack) =<< M.lookup "font-size" styleInherit

defaultFontSize :: Double
defaultFontSize = 18

styleFont :: Style -> Canvas.Font
styleFont st = Canvas.Font face size (weight == BoldWeight) (italic == ItalicStyle)
  where
    face = styleFontFamily st
    size = styleFontSize st
    weight = styleFontWeight st
    italic = styleFontStyle st

defaultFont :: Canvas.Font
defaultFont = Canvas.Font defaultFontFace defaultFontSize False False

-- | Merge the inherited style and the element style.
css :: [(Text, Text)] -> Style
css properties = Style { styleOwn = own, styleInherit = inherit }
  where
    (inherit, own) = M.partitionWithKey isInheritable $ M.fromList properties
    isInheritable prop _ = prop `elem` inheritable
    inheritable =
      [ "background-color"
      , "color"
      , "font-family"
      , "font-size"
      , "font-style"
      , "font-weight"
      , "white-space"
      ]

-- | Override style of parent with a new (maybe incomplete) style
-- The new style inherits parent's properties, unless overriden.
cascadingOver :: Style -> Style -> Style
cascadingOver style parent =
  style { styleInherit = styleInherit style `merge` styleInherit parent }
  where merge = M.mergeWithKey (\_ st _ -> Just st) id id

-- | Add CSS properties to existing properties, including own properties
overriding :: Style -> Style -> Style
overriding style parent = Style
    { styleInherit = styleInherit style `merge` styleInherit parent
    , styleOwn = styleOwn style `merge` styleOwn parent
    }
  where merge = M.mergeWithKey (\_ st _ -> Just st) id id


-- | Get the difference between two (complete) styles both ways.
-- Given an old and a new style, produce a pair of style diffs: "push" and "pop", s.t.
--   old + push -> new
--   new + pop -> old
styleDiff :: Style -> Style -> (StyleDiff, StyleDiff)
styleDiff Style{styleInherit=old} Style{styleInherit=new} = (toNew, toOld)
  where
    toNew = M.differenceWith (\v1 v2 -> if v1 /= v2 then Just v1 else Nothing) new old
    toOld = M.differenceWith (\v1 v2 -> if v1 /= v2 then Just v1 else Nothing) old new

noStyling :: (StyleDiff, StyleDiff)
noStyling = (M.empty, M.empty)

applyDiff :: Style -> StyleDiff -> Style
applyDiff st diff = st { styleInherit = M.mergeWithKey (\_ val _ -> Just val) id id diff (styleInherit st) }

-- | Built-in styles
noStyle :: Style
noStyle = Style { styleOwn = M.empty, styleInherit = M.empty }

bodyStyle :: Style
bodyStyle = Style { styleOwn = M.fromList own, styleInherit = M.fromList inheritable }
  where
    own =
      [ ("margin",      "8")
      ]
    inheritable =
      [ ("background-color","white")
      , ("color",           "black")
      , ("font-weight",     "normal")
      , ("font-family",     T.pack defaultFontFace)
      , ("font-size",       tshow defaultFontSize)
      , ("font-style",      "normal")
      , ("white-space",     "normal")
      ]

-- | Default stylings for standard HTML elements.
elementStyles :: HM.HashMap Text Style
elementStyles =
  HM.fromList
    ([ ("h1",       css [fontsize (2.00 * defaultFontSize), fontweight_bold])
     , ("h2",       css [fontsize (1.50 * defaultFontSize), fontweight_bold])
     , ("h3",       css [fontsize (1.17 * defaultFontSize), fontweight_bold])
     , ("h4",       css [fontsize (1.00 * defaultFontSize), fontweight_bold])
     , ("h5",       css [fontsize (0.83 * defaultFontSize), fontweight_bold])
     , ("h6",       css [fontsize (0.75 * defaultFontSize), fontweight_bold])
     ] ++
     [ ("a",        css [color "#00e", display_inline])
     , ("abbr",     inline)
     , ("acronym",  inline)
     , ("b",        css [fontweight_bold, display_inline])
     , ("bdo",      inline)
     , ("big",      css [fontsize (1.17 * defaultFontSize), display_inline])
     , ("blockquote", css [("margin", "40px 15px")])
     , ("body",     bodyStyle)
     , ("button",   inline)
     , ("cite",     inline)
     , ("code",     css [fontfamily defaultFontFaceMono, display_inline])
     , ("dfn",      inline)
     , ("em",       css [fontstyle_italic, display_inline])
     , ("i",        css [fontstyle_italic, display_inline])
     , ("img",      inline)
     , ("input",    inline)
     , ("kbd",      inline)
     , ("label",    inline)
     , ("map",      inline)
     , ("object",   inline)
     , ("pre",      css [("white-space", "pre"), ("margin", "13px 0"), fontfamily defaultFontFaceMono])
     , ("q",        inline)
     , ("samp",     inline)
     , ("script",   inline)
     , ("select",   inline)
     , ("small",    css [fontsize (0.83 * defaultFontSize), display_inline])
     , ("span",     inline)
     , ("strong",   css [fontweight_bold, display_inline])
     , ("sub",      inline)
     , ("sup",      inline)
     , ("time",     inline)
     , ("textarea", inline)
     , ("tt",       inline)
     , ("var",      inline)
     ] ++
     [ ("script",   nodisplay)
     , ("style",    nodisplay)
     , ("svg",      nodisplay)  -- TODO
     ])
  where
    inline = css [display_inline]
    nodisplay = css [("display", "none")]
    display_inline = ("display", "inline")
    fontsize sz = ("font-size", tshow sz)
    fontstyle_italic = ("font-style", "italic")
    fontweight_bold = ("font-weight", "bold")
    fontfamily fam = ("font-family", T.pack fam)
    color c = ("color", c)

defaultColor :: Canvas.Color
defaultColor = Canvas.rgb 0 0 0

-- CSS parsers and printers

-- a farcical CSS parser:
cssFarcer :: String -> Style
cssFarcer s = css $ mapMaybe parseProp $ T.splitOn ";" (T.pack s)
  where
    parseProp p = case T.splitOn ":" p of
      (key:val:_) -> Just (T.strip key, T.strip val)
      _ -> Nothing


cssColor :: String -> Maybe Canvas.Color
cssColor ['#',r1,r0,g1,g0,b1,b0] = Just $ Canvas.rgb (hex r1 r0) (hex g1 g0) (hex b1 b0)
  where hex d1 d0 = fromIntegral (0x10 * C.digitToInt d1 + C.digitToInt d0) :: Canvas.Byte
cssColor ['#', r, g, b] = cssColor ['#',r,r,g,g,b,b]
cssColor ('r':'g':'b':'(':_rest) = error "TODO: color: rgb(X,Y,Z)"
cssColor txt = cssColor =<< M.lookup txt colors
  where
   colors = M.fromList (
    -- CSS Level 1 colors:
    [ ("aqua",      "#00ffff")
    , ("black",     "#000000")
    , ("blue",      "#0000ff")
    , ("fuchsia",   "#ff00ff")
    , ("gray",      "#808080")
    , ("green",     "#008000")
    , ("maroon",    "#800000")
    , ("navy",      "#000080")
    , ("lime",      "#00ff00")
    , ("olive",     "#808000")
    , ("purple",    "#800080")
    , ("red",       "#ff0000")
    , ("silver",    "#c0c0c0")
    , ("teal",      "#008080")
    , ("white",     "#ffffff")
    , ("yellow",    "#ffff00")
    ] ++
    -- CSS Level 2 (Revision 1)
    [ ("orange",        "#ffa500") , ("aliceblue",     "#f0f8ff") , ("antiquewhite",  "#faebd7") , ("aquamarine",    "#7fffd4")
    , ("azure",         "#f0ffff") , ("beige",         "#f5f5d ") , ("bisque",        "#ffe4c4") , ("blanchedalmond","#ffebcd")
    , ("blueviolet",    "#8a2be2") , ("brown",         "#a52a2a") , ("burlywood",     "#deb887") , ("cadetblue",     "#5f9ea0")
    , ("chartreuse",    "#7fff00") , ("chocolate",     "#d2691e") , ("coral",         "#ff7f50") , ("cornflowerblue","#6495ed")
    , ("cornsilk",      "#fff8dc") , ("crimson",       "#dc143c") , ("cyan",          "#00ffff") , ("darkblue",      "#00008b")
    , ("darkcyan",      "#008b8b") , ("darkgoldenrod", "#b8860b") , ("darkgray",      "#a9a9a9") , ("darkgreen",     "#006400")
    , ("darkgrey",      "#a9a9a9") , ("darkkhaki",     "#bdb76b") , ("darkmagenta",   "#8b008b") , ("darkolivegreen","#556b2f")
    , ("darkorange",    "#ff8c00") , ("darkorchid",    "#9932cc") , ("darkred",       "#8b0000") , ("darksalmon",    "#e9967a")
    , ("darkseagreen",  "#8fbc8f") , ("darkslateblue", "#483d8b") , ("darkslategray", "#2f4f4f") , ("darkslategrey", "#2f4f4f")
    , ("darkturquoise", "#00ced1") , ("darkviolet",    "#9400d3") , ("deeppink",      "#ff1493") , ("deepskyblue",   "#00bfff")
    , ("dimgray",       "#696969") , ("dimgrey",       "#696969") , ("dodgerblue",    "#1e90ff") , ("firebrick",     "#b22222")
    , ("floralwhite",   "#fffaf0") , ("forestgreen",   "#228b22") , ("gainsboro",     "#dcdcdc") , ("ghostwhite",    "#f8f8ff")
    , ("gold",          "#ffd700") , ("goldenrod",     "#daa520") , ("greenyellow",   "#adff2f") , ("grey",          "#808080")
    , ("honeydew",      "#f0fff0") , ("hotpink",       "#ff69b4") , ("indianred",     "#cd5c5c") , ("indigo",        "#4b0082")
    , ("ivory",         "#fffff0") , ("khaki",         "#f0e68c") , ("lavender",      "#e6e6fa") , ("lavenderblush", "#fff0f5")
    , ("lawngreen",     "#7cfc00") , ("lemonchiffon",  "#fffacd") , ("lightblue",     "#add8e6") , ("lightcoral",    "#f08080")
    , ("lightcyan",     "#e0fff-")
    ])



--------------------------------------------------------------------------------
-- Utilities

lookupXMLAttribute :: Text -> XML.Element -> Maybe Text
lookupXMLAttribute attr node = M.lookup xmlattr (XML.elementAttributes node)
  where xmlattr = XML.Name attr Nothing Nothing

decodeXMLAttribute :: Read a => Text -> XML.Element -> Maybe a
decodeXMLAttribute attr node = do
  s <- lookupXMLAttribute attr node
  case reads (T.unpack s) of
    [(value, "")] -> Just value
    _ -> Nothing

tagName :: XML.Element -> Text
tagName = XML.nameLocalName . XML.elementName

tshow :: Show a => a -> Text
tshow = T.pack . show

mbRead :: Read a => String -> Maybe a
mbRead s = case reads s of [(v, "")] -> Just v; _ -> Nothing
