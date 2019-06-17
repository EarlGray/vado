{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.RWS.Strict as RWS
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString.Lazy as B
import qualified Data.Char as C
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Word (Word8)
import           Data.Either (partitionEithers)
import           Data.Text ( Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Linear.V2 (V2(..))
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Client.Internal
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Header as HTTP
import           Network.URI as URI
import qualified SDL as SDL
import qualified SDL.Cairo as Cairo
import qualified SDL.Cairo.Canvas as Canvas
import           SDL.Event as SDL
import qualified SDL.Image as Image
import           SDL.Vect
import           System.Environment
import qualified Text.HTML.DOM as HTML
import qualified Text.XML as XML
import           Text.XML.Cursor (($|), (&/))
import qualified Text.XML.Cursor as XML

import Debug.Trace as Trace

debug :: Bool
debug = False

warning :: String -> a -> a
warning msg = Trace.trace msg

--------------------------------------------------------------------------------
-- page, DOM and resources

-- | State of the page
data Page = Page
  { pageBody :: DOMNode
  -- ^ the body DOM tree
  , pageHead :: XML.Element
  -- ^ XML source of <head></head>
  , pageBoxes :: Maybe BoxTree
  -- ^ Rendering tree
  , pageUrl :: URI
  , pageResources :: M.Map Text HTTPResource  -- TODO: use URI

  , pageWindow :: VadoWindow
  , pageScroll :: Height
  }

emptyPage :: VadoWindow -> Page
emptyPage window = Page
  { pageHead = undefined
  , pageBody = blankNode
  , pageBoxes = Nothing
  , pageWindow = window
  , pageScroll = 0
  , pageResources = M.empty
  , pageUrl = nullURI
  }

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

emptyNode :: DOMNode
emptyNode = DOM
  { domContent = Right []
  , domStyle = noStyle
  , domEvents = noEvents
  , domAttrs = M.empty
  , domSource = Nothing
  }

blankNode :: DOMNode
blankNode = makeNode "body" [makeTextNode "This page is intentionally left blank"]

makeTextNode :: Text -> DOMNode
makeTextNode txt = emptyNode { domContent = Left (TextContent txt) }

makeNode :: Text -> [DOMNode] -> DOMNode
makeNode tag children = emptyNode { domContent=Right children, domStyle=style, domEvents=events }
  where
    style = fromMaybe noStyle $ HM.lookup tag elementStyles
    events = fromMaybe noEvents $ HM.lookup tag elementEvents

-- | Content of a leaf DOM node,
-- i.e. what is to be drawn in this node.
-- either a block or inline element, e.g. some text.
data DOMContent
  = TextContent !Text
  -- ^ A text node
  | NewlineContent
  -- ^ Force newline here, usually corresponds to <br>
  | ImageContent !Text !(Maybe (V2 Double))
  -- ^           ^href

instance Show DOMContent where
  show (TextContent txt) = T.unpack $ T.concat ["TextContent \"", txt, "\""]
  show NewlineContent = "NewlineContent"
  show (ImageContent href wh) = "ImageContent " ++ T.unpack href ++ " (" ++ show wh ++ ")"

--------------------------------------------------------------------------------
-- | Normalize an XML tree of elements and text, possibly with
-- attributes like style and event handlers.
domFromXML :: XML.Node -> Maybe DOMNode
domFromXML xml@(XML.NodeContent txt) =
    Just (makeTextNode txt){ domSource=Just xml }

domFromXML xml@(XML.NodeElement el) | tagName el == "img" =
    Just emptyNode{ domContent=Left (ImageContent href wh), domSource=Just xml }
  where
    Just href = lookupXMLAttribute "src" el -- TODO
    width = decodeXMLAttribute "width" el
    height = decodeXMLAttribute "height" el
    wh = liftM2 V2 width height

domFromXML xml@(XML.NodeElement el) =
    Just node{ domStyle=attr_style `overriding` domStyle node, domAttrs=attrs, domSource=Just xml }
  where
    node = makeNode (tagName el) children
    children = mapMaybe domFromXML $ XML.elementNodes el
    attrs = M.mapKeys (XML.nameLocalName) $ XML.elementAttributes el
    attr_style = fromMaybe noStyle $ decodeXMLAttribute "style" el

domFromXML _ = Nothing


--------------------------------------------------------------------------------
-- | A tree of bounding boxes
type RelPos = Point V2 Double

data BoxTree = BoxTree
  { boxContent :: BoxContent
  , boxNode :: Maybe DOMNode -- backreference to its node
  , boxDim :: V2 Double      -- outer dimensions
  , boxStyling :: (StyleDiff, StyleDiff)    -- instructions for box rendering
  }

boxHeight :: BoxTree -> Height
boxHeight BoxTree{ boxDim=V2 _ h } = h

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

-- | What to draw inside a box
data InlineContent
  = TextBox !Text !Double
  | ImageBox (V2 Double) Text   -- an image

instance Show InlineContent where
  show (TextBox t baseline) = "TextBox " ++ show t ++ " (baseline " ++ show baseline ++ ")"
  show (ImageBox (V2 w h) href) =   concat ["ImageBox ", show w, "x", show h, " " ++ T.unpack href]

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
  window <- vadoWindow
  page0 <- layoutPage (emptyPage window)
  renderDOM page0

  args <- getArgs
  let url = if null args then "https://en.wikipedia.org" else head args
  page1 <- fetchURL url page0

  page <- layoutPage page1
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
    QuitEvent ->
      return ()
    WindowClosedEvent {} ->
      return ()
    WindowResizedEvent e -> do
      let size = windowResizedEventSize e
      let win = pageWindow page
      texture' <- Cairo.createCairoTexture (vadoRenderer win) (fromIntegral <$> size)
      let win' = win{ vadoTexture=texture', vadoViewport=(fromIntegral <$> size) }
      -- TODO: optimize, don't do full layout again:
      page' <- layoutPage page{ pageWindow=win' }
      vadoRedrawEventLoop page'

    MouseButtonEvent e | SDL.mouseButtonEventMotion e == Released -> do
      let P xy@(V2 xi yi) = mouseButtonEventPos e
      putStrLn $ "clicked at " ++ show xy
      page' <- case pageBoxes page of
        Just boxes -> do
          let pagepos = V2 (fromIntegral xi) (fromIntegral yi + pageScroll page)
          let stack = P pagepos `findInBox` boxes
          case boxNode =<< listToMaybe stack of
            Just node ->
              case eventMouseReleased $ domEvents node of
                Just handler -> handler pagepos node page
                Nothing -> return page
            Nothing -> return page
        _ -> return page
      vadoRedrawEventLoop page'
    MouseWheelEvent e -> do
      let V2 _ dy = mouseWheelEventPos e
      vadoRedrawEventLoop $ vadoScroll (negate $ 10 * fromIntegral dy) page
    MouseMotionEvent _ ->
      vadoEventLoop page

    KeyboardEvent e | SDL.keyboardEventKeyMotion e == Released ->
      case SDL.keysymKeycode $ SDL.keyboardEventKeysym e of
        SDL.KeycodePageUp -> do
          vadoRedrawEventLoop $ vadoScroll (negate $ vadoViewHeight page) page
        SDL.KeycodePageDown -> do
          vadoRedrawEventLoop $ vadoScroll (vadoViewHeight page) page
        SDL.KeycodeHome ->
          vadoRedrawEventLoop $ vadoScroll (negate $ pageScroll page) page
        SDL.KeycodeEnd ->
          let pageH = maybe 0 boxHeight $ pageBoxes page
          in vadoRedrawEventLoop $ vadoScroll pageH page
        _ ->
          vadoEventLoop page

    _ -> do
      vadoEventLoop page

vadoRedrawEventLoop :: Page -> IO ()
vadoRedrawEventLoop page = renderDOM page >> vadoEventLoop page

vadoScroll :: Height -> Page -> Page
vadoScroll dy page = page{ pageScroll = max 0 $ min (pageScroll page + dy) (pageH - vadoViewHeight page) }
  where V2 _ pageH = fromMaybe 0 $ boxDim <$> pageBoxes page

vadoViewHeight :: Page -> Double
vadoViewHeight page = h
  where V2 _ h = pageViewport page


-- | A set of events that an element may handle.
type EventHandler = DOMNode -> Page -> IO Page

data Events = Events
  { eventMouseReleased :: Maybe (V2 Double -> EventHandler)
  }

noEvents :: Events
noEvents = Events { eventMouseReleased = Nothing }

elementEvents :: HM.HashMap Text Events
elementEvents = HM.fromList
  [ ("a", Events { eventMouseReleased = Just (\_ node page -> clickLink node page) })
  ]

clickLink :: DOMNode -> Page -> IO Page
clickLink DOM{ domSource=Just (XML.NodeElement el) } page = do
  case lookupXMLAttribute "href" el of
    Nothing -> return page
    Just href -> do
      page' <- fetchURL (T.unpack href) page
      layoutPage page'
clickLink node page =
  return $ warning ("clickLink: could not find address for: " ++ showdbg node) page

--------------------------------------------------------------------------------
-- HTTP request, caches and local storage

data HTTPResource
  = ImageResource !(V2 Double) SDL.Texture

fetchURL :: String -> Page -> IO Page
fetchURL url page = do
  let prevuri = pageUrl page
  let absuri =
        if URI.isAbsoluteURI url then (fromJust $ URI.parseURI url)
        else fromMaybe (error $ "invalid url: " ++ url) ((`URI.relativeTo` prevuri) <$> URI.parseURIReference url)
  request0 <- HTTP.parseRequest (show absuri)
  let pageReq = request0 { requestHeaders = [ (HTTP.hUserAgent, "github.com/chrisdone/vado") ] }
  when debug $ print pageReq

  httpman <- HTTP.newManager TLS.tlsManagerSettings
  pageResp <- HTTP.httpLbs pageReq httpman
  let headers = responseHeaders pageResp

  let contentType = T.toLower $ maybe "text/html" (T.decodeLatin1 . snd) $ L.find (\(name, _) -> name == "Content-Type") headers
  let body = HTTP.responseBody pageResp

  if "text/html" `T.isPrefixOf` contentType then do
    let document = HTML.parseLBS body
    let root = XML.documentRoot document
    let topnodes = mapMaybe (\case XML.NodeElement el -> Just el; _ -> Nothing) $ XML.elementNodes root
    let headnode = fromMaybe (fakeNode "head" []) $ L.find (\el -> tagName el == "head") topnodes
    let bodynode = fromMaybe (fakeNode "body" $ XML.elementNodes root) $ L.find (\el -> tagName el == "body") topnodes

    let images_axis = (XML.fromDocument document $| XML.descendant &/ XML.element "img")
    let imageurls = mapMaybe (\cursor -> let XML.NodeElement el = XML.node cursor in lookupXMLAttribute "src" el) images_axis
    resources <- forM imageurls $ \imgurl -> do
      case parseURIReference (T.unpack imgurl) of
        Nothing -> return $ warning ("Could not parse URL: " ++ T.unpack imgurl) []
        Just u -> do
          req <- setUriRelative pageReq u
          putStrLn $ concat
            [ T.unpack $ T.decodeLatin1 $ HTTP.method req, " "
            , if HTTP.secure req then "https://" else "http://"
            , T.unpack $ T.decodeLatin1 $ HTTP.host req
            , T.unpack $ T.decodeLatin1 $ HTTP.path req
            ]
          resp <- HTTP.httpLbs req httpman
          let content = B.toStrict $ HTTP.responseBody resp
          case Image.format content of
            Just _ -> do
              bitmap <- Image.decode content
              V2 w h <- SDL.surfaceDimensions bitmap
              let wh = V2 (fromIntegral w) (fromIntegral h)
              texture <- SDL.createTextureFromSurface (vadoRenderer $ pageWindow page) bitmap
              return [(imgurl, ImageResource wh texture)]
            _ ->
              return $ warning ("Image format not supported: " ++ T.unpack imgurl) []

    let Just dom = domFromXML $ XML.NodeElement bodynode
    when debug $ putStrLn (showdbg dom)
    return page
      { pageUrl = absuri
      , pageHead = headnode
      , pageBody = dom
      , pageResources = M.fromList (concat resources)
      , pageScroll = 0
      }
  else if "text/" `T.isPrefixOf` contentType then do
    let respbody = T.decodeUtf8 $ B.toStrict $ responseBody pageResp
    let text = makeTextNode respbody
    let pre = makeNode "pre" [text]
    return page
      { pageUrl = absuri
      , pageHead = fakeNode "head" []
      , pageBody = makeNode "body" [pre]
      , pageScroll = 0
      }
  else if "image/" `T.isPrefixOf` contentType then do
    let body' = B.toStrict body
    (img, resources) <- case Image.format body' of
      Just _ -> do
        bitmap <- Image.decode body'
        V2 w h <- SDL.surfaceDimensions bitmap
        let wh = V2 (fromIntegral w) (fromIntegral h)
        let node = emptyNode{ domContent=Left (ImageContent (T.pack url) (Just wh)) }
        texture <- SDL.createTextureFromSurface (vadoRenderer $ pageWindow page) bitmap
        return (node, M.fromList [(T.pack url, ImageResource wh texture)])
      _ ->
        let node = makeTextNode (T.pack $ "Error[" ++ url ++ "]: Image format not supported")
        in return (node, M.fromList [])
    return page
      { pageUrl = absuri
      , pageHead = fakeNode "head" []
      , pageBody = makeNode "body" [img]
      , pageResources = resources
      , pageScroll = 0
      }
  else
    error $ "TODO: Content-Type " ++ (T.unpack contentType)
  where
    fakeNode name nodes = XML.Element (XML.Name name Nothing Nothing) M.empty nodes


--------------------------------------------------------------------------------
-- Layout engine

type Width = Double
type Height = Double
type BaselineY = Double

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
  , ltStack :: [DOMNode]                  -- reverse DOMNode stack
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
  { ltResources :: M.Map Text HTTPResource
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

-- | Entry point for layout procedure
layoutPage :: Page -> IO Page
layoutPage page@Page{..} = do
    (boxes, _ctx) <- Canvas.withCanvas texture $ elementToBoxes ctx params noStyle pageBody
    when debug $ print boxes
    return page{ pageBoxes=Just boxes }
  where
    ctx = LayoutCtx { ltResources = pageResources }
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
      , ltStack = [node]
      }
elementToBoxes _ctx _par _st node = error $ "elementToBoxes (" ++ showdbg node ++ ")"

withStyle :: CanMeasureText m => DOMNode -> LayoutOver m a -> LayoutOver m a
withStyle node action = do
    parentStyle <- gets ltStyle
    parentStyling <- gets ltStyling
    parentNode <- gets ltStack
    let st = domStyle node `cascadingOver` parentStyle
    let styling = parentStyle `styleDiff` st
    modify $ \lt -> lt{ ltStyle=st, ltStyling=styling, ltStack = node:parentNode }
    result <- action
    modify $ \lt -> lt{ ltStyle=parentStyle, ltStyling=parentStyling, ltStack = parentNode }
    return result

layoutBlock :: CanMeasureText m => [DOMNode] -> LayoutOver m ()
layoutBlock children = do
  parentSt <- gets ltStyle
  forM_ children $ \child@DOM{ domContent=content, domStyle=st } -> do
    case (content, st `cssValue` CSSDisplay) of
      (_, CSS_Keyword "none") ->
        return ()
      (Left (TextContent txt), _) ->
        layoutText txt
      (Left NewlineContent, _) ->
        layoutLineBreak
      (Left (ImageContent href mbSize), _) -> do
        -- TODO: block <img>
        resources <- gets (ltResources . ltCtx)
        let mbResSize = case M.lookup href resources of
              Just (ImageResource wh _) -> Just wh
              Nothing -> Nothing
        case mbSize <|> mbResSize of
          Just wh@(V2 _ h) -> do
            let baseline = imgBaseline wh child
            layoutInlineBox wh baseline child (BoxInline $ ImageBox wh href)
          _ -> return ()
      (Right children', CSS_Keyword "inline") ->
        withStyle child $ layoutBlock children'
      (Right _, display) -> do
        unless (display == CSS_Keyword "block") $
          return $ warning ("TODO: display=" ++ show display ++ ", falling back to display=block") ()
        -- wrap up the previous line (if any):
        layoutLineBreak
        -- start a new containing block:
        params <- ask
        ctx <- gets ltCtx
        (box, ctx') <- lift $ elementToBoxes ctx params parentSt child
        modify (\lt -> lt{ ltCtx=ctx' })
        layoutBlockBox box
      -- (_, display) -> error $ concat ["layoutBlock: display=", show display, ", child=", showdbg child]
  where
    imgBaseline (V2 _ h) DOM{domSource=Just (XML.NodeElement el)} =
        case lookupXMLAttribute "align" el of
          Just "top" -> noBaseline
          Just "middle" -> h/2
          Just "bottom" -> h
          Just other -> warning ("unknown <img align='" ++ T.unpack other ++ "'>") h
          Nothing -> h
    imgBaseline (V2 _ h) _ = h


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
  font <- gets (styleFont . ltStyle)
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
  font <- gets (styleFont . ltStyle)
  wgap <- lift $ (/ 4) <$> measureTextWidth font "____"
  modify (\lt ->
    let x = ltX lt + wgap
        ls = ltLS lt
        ls' = ls{ lsWords=lsWords ls ++ [" "], lsGap = True }
    in lt{ ltX=x, ltLS=ls' })

layoutInlineBox :: CanMeasureText m => V2 Double -> BaselineY -> DOMNode -> BoxContent -> LayoutOver m ()
layoutInlineBox (V2 dx dy) baseline node content = do
  x <- gets ltX
  maxwidth <- asks ltWidth
  when (x + dx >= maxwidth) $ do
    layoutLineBreak

  styling <- gets ltStyling
  let box = BoxTree
        { boxContent = content
        , boxNode = Just node
        , boxDim = V2 dx dy
        , boxStyling = styling
        }
  modify (\lt ->
    let ls = ltLS lt
    in lt{ ltX=ltX lt + dx, ltLS=ls{ lsWords=[], lsBoxes=lsBoxes ls ++ [(baseline, box, 0, 0)] } }
    )

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
          , boxNode = listToMaybe $ ltStack lt
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

  boxes <- gets (lsBoxes . ltLS)
  if null boxes then do
    preserveNewine <- gets (stylePreservesNewlines . ltStyle)
    when preserveNewine $ do
      -- advance Y by a line height:
      font <- gets (styleFont . ltStyle)
      (_, h) <- lift $ measureHeightAndBaseline font
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
          , boxNode = Nothing
          , boxDim = V2 width height
          , boxStyling = noStyling
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
chunksFromTokens (CSS_Keyword "nowrap") (gap0, tokens) =
  case (gap0, tokens) of
    (False, " ":ts) ->
      let (gap', [ln]) = chunksFromTokens (CSS_Keyword "nowrap") (True, ts) in (gap', [" " ++ ln])
    _ ->
      let ln = L.intercalate " " $ filter (\t -> t /= " " && t /= "\n") tokens in (False, [ln])

chunksFromTokens whitespace ts = chunksFromTokens (warning msg $ CSS_Keyword "normal") ts
  where msg = "TODO: chunksFromTokens fallback to 'normal' instead of: " ++ show whitespace

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
test_chunksFromTokens_Normal = run_tests (chunksFromTokens (CSS_Keyword "normal")) [
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
test_chunksFromTokens_Pre = run_tests (chunksFromTokens (CSS_Keyword "pre")) [
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
  let (texture, renderer) = let win = pageWindow page in (vadoTexture win, vadoRenderer win)
  replaced <- Canvas.withCanvas texture $ do
    let body = fromJust $ pageBoxes page
    Canvas.background $ Canvas.rgb 255 255 255 -- TODO: set to body background color
    withStyling body noStyle $ \st ->
      renderTree (minY, minY + vadoViewHeight page) (0, 0, st) body
  SDL.copy (vadoRenderer $ pageWindow page) texture Nothing Nothing

  forM_ replaced $ \(rect, content) -> do
    case content of
      ImageBox _ href ->
        case M.lookup href (pageResources page) of
          Just (ImageResource _ imgtexture) -> do
            let cint x = fromIntegral (round x :: Int)
            let SDL.Rectangle (P (V2 x y)) (V2 dx dy) = rect
            let pos = P $ V2 (cint x) (cint (y - minY))
            let dim = V2 (cint dx) (cint dy)
            let rect' = SDL.Rectangle pos dim
            SDL.copy (vadoRenderer $ pageWindow page) imgtexture Nothing (Just rect')
          Nothing ->
            return $ warning ("renderDOM: could not find resources for <img src=" ++ T.unpack href) ()
      other -> return $ warning ("renderDOM: unexpected replaced element: " ++ show other) ()

  SDL.present renderer

renderTree :: (Double, Double) -> (Double, Double, Style) -> BoxTree -> Canvas.Canvas [(SDL.Rectangle Double, InlineContent)]
renderTree (minY, maxY) (x, y, st0) box = do
  case boxContent box of
    BoxOfBlocks children -> do
      replaced <- forM children $ \(P (V2 dx dy), child) -> do
        if not ((y+dy) + boxHeight child < minY || maxY < (y+dy)) then
          withStyling child st0 $ \st ->
            renderTree (minY, maxY) (x + dx, y + dy, st) child
        else
          return []
      return $ concat replaced
    BoxInline (TextBox txt baseline) -> do
      Canvas.textBaseline (T.unpack txt) (V2 x (y + baseline - minY))
      return []
    BoxInline content@(ImageBox (V2 w h) _) ->
      let rect = SDL.Rectangle (P $ V2 x y) (V2 w h)
      in return [(rect, content)]
    -- _ -> error $ "TODO: renderTree " ++ show (boxContent box)

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
        case (prop, val) of
          (CSSFont, CSS_Font _) ->
            Canvas.textFont $ styleFont st
          (CSSColor, CSS_RGB r g b) ->
            Canvas.stroke $ Canvas.rgb r g b
          (CSSColor, CSS_Keyword name) ->
            -- TODO: move to cascadingOver?
            case cssReadValue <$> M.lookup name cssColorAliases of
              Just (Right (CSS_RGB r g b)) ->
                Canvas.stroke $ Canvas.rgb r g b
              _ ->
                return $ warning ("unknown color: " ++ T.unpack name) ()
          _ -> return ()


--------------------------------------------------------------------------------
-- CSS styles

-- | Style for an element, consists of own and inheritable properties.
-- Must be complete (e.g. not just style diff)
-- No types for now, just stringly-typed prototypes.
data Style = Style
  { styleOwn :: M.Map CSSOwnProperty CSSValue
  , styleInherit :: M.Map CSSProperty CSSValue
  }

-- A difference in styles
type StyleDiff = M.Map CSSProperty CSSValue

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

class IsCSSProperty prop where
  cssValueMaybe :: Style -> prop -> Maybe CSSValue
  cssDefault :: prop -> CSSValue

  cssValue :: Style -> prop -> CSSValue
  cssValue st prop = fromMaybe (cssDefault prop) (cssValueMaybe st prop)


-- | CSS properties and values
type UnknownOr a = Either Text a

-- | Inheritable properites:
data CSSProperty
  = CSSBackgroundColor
  | CSSColor
  | CSSFont
  | CSSWhiteSpace
  | CSSTextAlign
  deriving (Show, Eq, Ord, Enum)

cssPropertyNames :: M.Map CSSProperty Text
cssPropertyNames = M.fromList
  [ (CSSBackgroundColor,    "background-color")
  , (CSSColor,              "color")
  , (CSSWhiteSpace,         "white-space")
  , (CSSTextAlign,          "text-align")
  ]

cssNamesOfProperties :: M.Map Text CSSProperty
cssNamesOfProperties =
  M.fromList [ (v, k) | (k, v) <- M.toList cssPropertyNames ]

cssPropertyDefaults :: M.Map CSSProperty CSSValue
cssPropertyDefaults = M.fromList
  [ (CSSBackgroundColor,    CSS_RGB 255 255 255)
  , (CSSColor,              CSS_RGB 0 0 0)
  , (CSSWhiteSpace,         CSS_Keyword "normal")
  , (CSSTextAlign,          CSS_Keyword "left")
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
  | CSSMargin
  | CSSBorder
  | CSSPadding
  deriving (Show, Eq, Ord, Enum)

cssOwnPropertyNames :: M.Map CSSOwnProperty Text
cssOwnPropertyNames = M.fromList
  [ (CSSDisplay,                "display")
  , (CSSMargin,                 "margin")
  , (CSSBorder,                 "border")
  , (CSSPadding,                "padding")
  ]
cssOwnPropertyDefaults :: M.Map CSSOwnProperty CSSValue
cssOwnPropertyDefaults = M.fromList
  [ (CSSDisplay,        CSS_Keyword "block")
  , (CSSMargin,         CSS_Px 0)
  , (CSSBorder,         CSS_Px 0)
  , (CSSPadding,        CSS_Px 0)
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

-- | CSS values: weakly-typed data
data CSSValue
  = CSS_Keyword Text
  | CSS_Em Double
  | CSS_Px Double
  | CSS_Num Double
  | CSS_Percent Double
  | CSS_Url Text
  | CSS_String Text
  | CSS_List [CSSValue]
  | CSS_Font CSSFontValue
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
  showdbg other = "TODO:showdbg(" ++ show other ++ ")"


data CSSFontValue = CSSFontValue
  { cssfontStyle :: Maybe CSSValue
  , cssfontSize :: Maybe CSSValue
  , cssfontWeight :: Maybe CSSValue
  , cssfontFamily :: Maybe CSSValue
  } deriving (Show, Eq)

noCSSFont :: CSSFontValue
noCSSFont = CSSFontValue
  { cssfontStyle = Nothing
  , cssfontSize = Nothing
  , cssfontWeight = Nothing
  , cssfontFamily = Nothing
  }

mergeCSSFontValues :: CSSFontValue -> CSSFontValue -> CSSFontValue
mergeCSSFontValues font1 font2 = CSSFontValue
    { cssfontStyle = cssfontStyle font1 <|> cssfontStyle font2
    , cssfontSize = cssfontSize font1 <|> cssfontSize font2
    , cssfontWeight = cssfontWeight font1 <|> cssfontWeight font2
    , cssfontFamily = cssfontFamily font1 <|> cssfontFamily font2
    }

instance Monoid CSSFontValue where
  mempty = noCSSFont
  mappend = mergeCSSFontValues


-- | Augment/override/merge new CSS style with base style.
-- The new style inherits/merges parent's properties, unless overriden.
-- Some values are computed based on parent values (see `cascadingValue`).
cascadingOver :: Style -> Style -> Style
cascadingOver style parent =
  style { styleInherit = inheritable `merge` styleInherit parent }
  where
    inheritable = case M.lookup CSSDisplay (styleOwn style) of
                Just (CSS_Keyword "inline") -> M.delete CSSTextAlign (styleInherit style)
                _ -> styleInherit style
    merge = M.mergeWithKey cascadingValue id id

cascadingValue :: CSSProperty -> CSSValue -> CSSValue -> Maybe CSSValue
cascadingValue CSSFont new@(CSS_Font font1) old@(CSS_Font font2) =
  case cssfontSize font1 of
    Just (CSS_Keyword kw) ->
      case kw `L.lookup` relkeywords of
        Just val ->
          cascadingValue CSSFont (CSS_Font font1{ cssfontSize=Just val }) old
        Nothing ->
          case kw `L.lookup` abskeywords of
            Just val -> Just $ CSS_Font $ font1{ cssfontSize=Just val }
            Nothing -> warning ("font-size ignored: " ++ show kw) $ Just new
    Just (CSS_Percent perc) ->
      let sz = case cssfontSize font2 of
            Just (CSS_Px size) -> size
            other -> warning ("font-size=" ++ show other ++ " is not computed, falling back to default") defaultFontSize
      in Just $ CSS_Font $ font1{ cssfontSize=Just (CSS_Px (sz * (perc / 100))) }
    Just (CSS_Em em) ->
      let sz = case cssfontSize font2 of
            Just (CSS_Px size) -> size
            other -> warning ("font-size=" ++ show other ++ " is not computed, falling back to default") defaultFontSize
      in Just $ CSS_Font $ font1{ cssfontSize=Just (CSS_Px (sz * em)) }
    _ -> Just new
  where
    relkeywords =
      [ ("smaller", CSS_Percent 70)
      , ("larger",  CSS_Percent 130)
      ]
    abskeywords =
      [ ("xx-small",  CSS_Px (0.30 * defaultFontSize))
      , ("x-small",   CSS_Px (0.50 * defaultFontSize))
      , ("small",     CSS_Px (0.70 * defaultFontSize))
      , ("medium",    CSS_Px (1.00 * defaultFontSize))
      , ("large",     CSS_Px (1.30 * defaultFontSize))
      , ("x-large",   CSS_Px (1.70 * defaultFontSize))
      , ("xx-large",  CSS_Px (2.00 * defaultFontSize))
      ]
cascadingValue _ st _ = Just st


-- | Add CSS properties to existing properties, including own properties
-- Shadow previous values if they exist.
overriding :: Style -> Style -> Style
overriding newprops base = Style
    { styleInherit = styleInherit newprops `merge` styleInherit base
    , styleOwn = styleOwn newprops `merge` styleOwn base
    }
  where
    merge :: Ord k => M.Map k v -> M.Map k v -> M.Map k v
    merge = M.mergeWithKey (\_ st _ -> Just st) id id


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
applyDiff st diff = st { styleInherit = diff `patch` (styleInherit st) }
  where
    patch = M.mergeWithKey (\_ val _ -> Just val) id id

--------------------------------------------------------------------------------
-- CSS parsers and printers

-- a farcical CSS parser:
cssFarcer :: String -> Style
cssFarcer s = css $ mapMaybe parseProp $ T.splitOn ";" (T.pack s)
  where
    parseProp p = case T.break (== ':') p of
      (_, "") -> Nothing
      (key, val) -> Just (T.strip key, T.strip $ T.tail val)

-- | Read and split properties into own and inheritable
-- Warn about unknown properties and values, ignore them.
css :: [(Text, Text)] -> Style
css properties = Style
    { styleOwn = M.fromListWith mergePropVal own
    , styleInherit = M.fromListWith mergePropVal inherit
    }
  where
    (own, inherit) = partitionEithers $ mapMaybe readProperty properties
    readProperty (name, textval) =
      case cssReadValue textval of
        Left _ ->
          warning (concat ["Unknown value of a property: ", T.unpack name, "=", T.unpack textval]) Nothing
        Right val ->
          case M.lookup name cssNamesOfProperties of
            Just prop -> Just $ Right (prop, val)
            Nothing ->
              case M.lookup name cssNamesOfOwnProperties of
                Just prop -> Just $ Left (prop, val)
                Nothing ->
                  let mkFont = \font -> Just (Right (CSSFont, CSS_Font font))
                  in case name of
                    "font-weight" -> mkFont $ noCSSFont{ cssfontWeight = Just val }
                    "font-style" -> mkFont $ noCSSFont{ cssfontStyle = Just val }
                    "font-size" -> mkFont $ noCSSFont{ cssfontSize = Just val }
                    "font-family" ->  mkFont $ noCSSFont{ cssfontFamily = Just val }
                    _ -> if "-" `T.isPrefixOf` name then Nothing
                         else warning ("Unknown property: " ++ T.unpack name ++ "=" ++ show val) Nothing
    mergePropVal (CSS_Font new) (CSS_Font old) = CSS_Font (mergeCSSFontValues new old)
    mergePropVal new _old = new


-- | Parses a CSSValue from text for a CSS value
cssReadValue :: Text -> UnknownOr CSSValue
cssReadValue txtval =
  case Atto.parseOnly cssparseValue txtval of
    Left _ -> Left txtval
    Right val -> Right val

cssparseValue :: Atto.Parser CSSValue
cssparseValue = valp <* Atto.skipSpace <* Atto.endOfInput
  where
    valp = Atto.choice [cssparseUrl, cssparseColor, cssparseString, cssparseIdentifier, cssparseLength, cssparseNum]
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
          (const (CSS_Px num)) <$> Atto.string "px"
        , (const (CSS_Em num)) <$> Atto.string "em"
        , (const (CSS_Percent num)) <$> Atto.string "%"
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

cssColorAliases :: M.Map Text Text
cssColorAliases = M.fromList (cssColorsLevel1 ++ cssColorsLevel2)
  where
    cssColorsLevel1 =
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
        ]
    cssColorsLevel2 =
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
        ]


--------------------------------------------------------------------------------
-- | Default CSS values and getters

defaultFontFace :: String
defaultFontFace = "Noto Serif"

defaultFontFaceMono :: String
defaultFontFaceMono = "Noto Mono"

defaultFontSize :: Double
defaultFontSize = 18

styleFontSize :: Style -> Double
styleFontSize st =
  let CSS_Font font = st `cssValue` CSSFont
  in case cssfontSize font of
    Just (CSS_Num size) -> size
    Just (CSS_Px size) -> size
    Just (CSS_Em em) -> defaultFontSize * em
    Just other -> warning ("styleFontSize: not computed: " ++ show other) defaultFontSize
    Nothing -> defaultFontSize

defaultFont :: Canvas.Font
defaultFont = Canvas.Font defaultFontFace defaultFontSize False False

styleFont :: Style -> Canvas.Font
styleFont st = Canvas.Font face size (weight == Just (CSS_Keyword "bold")) (italic == Just (CSS_Keyword "italic"))
  where
    CSS_Font font = st `cssValue` CSSFont
    face =
      case cssfontFamily font of
        Just (CSS_String name) -> T.unpack name
        Just (CSS_Keyword name) -> T.unpack name
        Just other -> error $ "styleFont: unknown " ++ show other
        Nothing -> defaultFontFace
    size = styleFontSize st
    weight = cssfontWeight font
    italic = cssfontStyle font

stylePreservesNewlines :: Style -> Bool
stylePreservesNewlines st =
  case st `cssValueMaybe` CSSWhiteSpace of
    Just (CSS_Keyword "pre") -> True
    _ -> False


--------------------------------------------------------------------------------
-- | Built-in styles

noStyle :: Style
noStyle = Style { styleOwn = M.empty, styleInherit = M.empty }

bodyStyle :: Style
bodyStyle = css (own ++ inheritable)
  where
    own =
      [ ("margin",          "8px")
      ]
    inheritable =
      [ ("background-color","white")
      , ("color",           "black")
      , ("font-weight",     "normal")
      , ("font-family",     T.pack ("\"" ++ defaultFontFace ++ "\""))
      , ("font-size",       T.pack (show defaultFontSize ++ "px"))
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
     , ("big",      css [("font-size", "117%"), display_inline])
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
     , ("small",    css [("font-size", "83%"), display_inline])
     , ("span",     inline)
     , ("strong",   css [fontweight_bold, display_inline])
     , ("sub",      inline)
     , ("sup",      inline)
     , ("td",       inline)   -- TODO: proper tables
     , ("th",       inline)   -- TODO: proper tables
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
    fontsize sz = ("font-size", T.pack (show sz ++ "px"))
    fontstyle_italic = ("font-style", "italic")
    fontweight_bold = ("font-weight", "bold")
    fontfamily fam = ("font-family", T.pack ("\"" ++ fam ++ "\""))
    color c = ("color", c)

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

