{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import           Control.Applicative
import qualified Control.Exception as Exc
import           Control.Monad
import qualified Control.Monad.Identity as MId
import           Control.Monad.RWS.Strict as RWS
import           Control.Monad.State (StateT(..))
import qualified Control.Monad.State as St
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as Bc
import qualified Data.Char as C
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import           Data.Maybe
import qualified Data.Set as S
import           Data.Word (Word8)
import           Data.Either (partitionEithers)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Linear.V2 (V2(..))
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Client.Internal (HttpException)
import qualified Network.HTTP.Client.TLS as TLS
import           Network.HTTP.Types.URI (urlDecode)
import qualified Network.HTTP.Types.Header as HTTP
import           Network.URI as URI
import qualified SDL
import qualified SDL.Cairo as Cairo
import qualified SDL.Cairo.Canvas as Canvas
import           SDL.Event as SDL
import qualified SDL.Image as Image
--import qualified SDL.Input.Mouse as SDL
import           SDL.Vect
import           System.CPUTime (getCPUTime)
import qualified System.Environment as Env
import qualified Text.HTML.DOM as HTML
import           Text.Printf (printf)
import           Text.Read (readMaybe)
import qualified Text.XML as XML
import           Text.XML.Cursor (($|), (&/))
import qualified Text.XML.Cursor as XML

import Debug.Trace as Trace

-- TODO: make this a command-line switch
debug :: Bool
debug = False

warning :: String -> a -> a
warning msg = Trace.trace msg

-- Meaningful names:

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = forM_

mbHead :: [a] -> Maybe a
mbHead = listToMaybe

mbReadText :: Read a => Text -> Maybe a
mbReadText = readMaybe . T.unpack

--------------------------------------------------------------------------------
-- page, DOM and resources

-- | State of the page
data Page = Page
  { pageDocument :: Document
  , pageBoxes :: Maybe BoxTree
  -- ^ Rendering tree
  , pageUrl :: URI
  , pageResources :: M.Map Text HTTPResource  -- TODO: use URI

  , pageWindow :: VadoWindow
  , pageScroll :: Height
  }

emptyPage :: VadoWindow -> Page
emptyPage window = Page
  { pageDocument = emptyDocument
  , pageBoxes = Nothing
  , pageWindow = window
  , pageScroll = 0
  , pageResources = M.empty
  , pageUrl = nullURI
  }

class HasDebugView a where
  showdbg :: a -> String


-- | Content of a leaf DOM node,
-- i.e. what is to be drawn in this node.
-- either a block or inline element, e.g. some text.
data DOMContent
  = TextContent !Text
  -- ^ A text node
  | NewlineContent
  -- ^ Force newline here, usually corresponds to <br>
  | ImageContent !Text (Maybe (V2 Double))
  -- ^           ^href
  | HorizLineContent
  -- ^ <hr>
  | InputTextContent !Text
  -- ^ <input type="text">

instance Show DOMContent where
  show (TextContent txt) = T.unpack $ T.concat ["TextContent \"", txt, "\""]
  show NewlineContent = "NewlineContent"
  show HorizLineContent = "HorizLineContent"
  show (ImageContent href wh) = "ImageContent " ++ T.unpack href ++ " (" ++ show wh ++ ")"
  show (InputTextContent t) = "TextInputContent \"" ++ T.unpack t ++ "\""


--------------------------------------------------------------------------------
-- | This is a new take on the page loading and state,
-- | to replace DOMNode and Page in the future.


-- | The navigator is a "browser" abstraction.
-- |   - stores settings and the collection of windows.
-- |   - does network requests and caching.
--data Navigator = Navigator

-- | This is a window to render on the screen.
-- |   - contains a document
-- |   - contains history
--data Window = Window

-- | This is a webpage
-- |   - contains the DOM tree
-- |   - contains the CSS rule storage
-- |   - contains the rendering cache(s)
data Document = Document
  { documentAllNodes :: IM.IntMap Element
  , documentNewID :: ElementID
  , documentBuildState :: [ElementID]     -- current open elements during adding

  , documentHead :: ElementID
  , documentBody :: ElementID
  , documentFocus :: ElementID      -- the keyboard focus
  -- , documentContentType :: ContentType
  -- , documentMeta :: M.Map Text Text
  -- , documentTitle :: Text
  -- , documentElementsByClass :: M.Map Text [ElementID]
  -- , documentElementById :: M.Map Text ElementID

  } deriving (Show)

emptyDocument :: Document
emptyDocument = Document
  { documentAllNodes = IM.empty
  , documentNewID = succ noElement
  , documentBuildState = []
  , documentHead = noElement
  , documentBody = noElement
  , documentFocus = noElement
  }

instance HasDebugView Document where
  showdbg doc = unlines $
      [ "documentHead = @" ++ show (documentHead doc)
      , "documentBody = @" ++ show (documentBody doc)
      , "documentNewID = @" ++ show (documentNewID doc)
      , "documentFocus = @" ++ show (documentFocus doc)
      ] ++ shownode (elementRef doc htmlElement)
    where
      shownode :: ElementRef -> [String]
      shownode elt =
        let nid = elementRefID elt
            node = elementDeref elt
            tag = elementTag node
            attrs = elementAttrs node
            hdr = ["@"++show nid++":"++T.unpack tag++" "++show (M.toList attrs)++
                   " style="++showdbg (elementStyleAttr node)++
                   " css="++showdbg (elementStyleHTML node)]
        in case elementContent node of
          Right _ ->
            hdr ++ indent (L.concatMap shownode (elementChildren elt))
          Left content ->
            hdr ++ indent [show content]

      indent :: [String] -> [String]
      indent = map (\s -> ' ':' ':s)

type DocumentT m a = StateT Document m a

-- | Run a computation in DocumentT, return the result, discard document changes.
inDocument :: Monad m => Document -> DocumentT m a -> m a
inDocument doc f = St.evalStateT f doc

inPageDocument :: Monad m => Page -> DocumentT m a -> m a
inPageDocument page f = inDocument (pageDocument page) f

-- | Run an action on DocumentT, return a changed document.
runDocument :: Monad m => Document -> DocumentT m a -> m Document
runDocument doc f = St.execStateT f doc

runPageDocument :: Monad m => Page -> DocumentT m a -> m Page
runPageDocument page f = do
  doc' <- runDocument (pageDocument page) f
  return page{ pageDocument = doc' }


takeNewID :: Monad m => DocumentT m ElementID
takeNewID = do
  i <- gets documentNewID
  modify $ \doc -> doc{ documentNewID = succ (documentNewID doc) }
  return i

getElement :: Monad m => ElementID -> DocumentT m Element
getElement nid = do
  nodes <- gets documentAllNodes
  case IM.lookup nid nodes of
    Just node -> return node
    Nothing -> error $ "nodeID not found: " ++ show nid

getElementRef :: Monad m => ElementID -> DocumentT m ElementRef
getElementRef nid = do
  nodes <- gets documentAllNodes
  return (nodes, nid)

-- TODO: ElementID/Element, Id/ElementID confusion
-- TODO: populate and use the `documentElementById` cache
getElementById :: Monad m => Text -> DocumentT m (Maybe ElementID)
getElementById eid = do
  nodes <- IM.toList <$> gets documentAllNodes
  return $ fmap fst $ L.find (\(_, elt) -> M.lookup "id" (elementAttrs elt) == Just eid) nodes

alterNode :: Monad m => ElementID -> (Maybe Element -> Maybe Element) -> DocumentT m ()
alterNode nid f = do
  nodes <- gets documentAllNodes
  let nodes' = IM.alter f nid nodes
  modify $ \doc -> doc{ documentAllNodes = nodes' }

setElement :: Monad m => ElementID -> Element -> DocumentT m ()
setElement nid element = alterNode nid $ \_ -> Just element

modifyElement :: Monad m => ElementID -> (Element -> Element) -> DocumentT m ()
modifyElement nid f = alterNode nid (fmap f)

-- | domtree low-level functions: Building document DOM tree

fromEmptyDocument :: Monad m => DocumentT m a -> m Document
fromEmptyDocument f = runDocument emptyDocument f

domtreeAppendChild :: Monad m =>
                      Maybe (Text, M.Map Text Text) -> DOMContent
                      -> DocumentT m ElementID
domtreeAppendChild mbXML content = do
  opened <- gets documentBuildState
  let pid = head opened
  parent <- getElement pid
  case elementContent parent of
    Right fid -> do
      nid <- takeNewID
      (fid', lid') <- if fid == noElement then do
        setElement pid $ parent{ elementContent = Right nid }
        return (nid, nid)
      else do
        first <- getElement fid
        let lid = prevSibling $ elementSiblings first
        setElement fid $
          first{ elementSiblings = (elementSiblings first){ prevSibling = nid } }
        modifyElement lid $ \node ->
          node{ elementSiblings = (elementSiblings node){ nextSibling = nid } }
        return (fid, lid)

      let (tag, attrs) = fromMaybe ("", M.empty) mbXML
      setElement nid emptyElement
            { elementParent = pid
            , elementSiblings = ElementSiblings{ prevSibling = lid', nextSibling = fid' }
            , elementContent = Left content
            , elementTag = tag
            , elementAttrs = attrs
            }
      return nid
    _ ->
      return $ warning ("not an insertable element: " ++ show parent) noElement

domtreeStartElement :: Monad m => (Text, M.Map Text Text) -> DocumentT m ElementID
domtreeStartElement (tag, attrs) = do
  opened <- gets documentBuildState
  let pid = if null opened then noElement else head opened

  nid <- takeNewID
  let node = emptyElement
       { elementParent = pid
       , elementContent = Right noElement
       , elementTag = tag
       , elementAttrs = attrs
       }

  if pid == noElement then do
    setElement nid $
      node{ elementSiblings = ElementSiblings{ prevSibling = nid, nextSibling = nid } }
  else do
    parent <- getElement pid
    let Right fid = elementContent parent   -- Left should not be in opened, ever.
    if fid == noElement then do
      setElement pid parent{ elementContent = Right nid }
      setElement nid $
        node{ elementSiblings = ElementSiblings{ prevSibling = nid, nextSibling = nid } }
    else do
      fnode <- getElement fid
      let lid = prevSibling $ elementSiblings fnode
      setElement fid $
        fnode{ elementSiblings = (elementSiblings fnode){ prevSibling = nid } }
      modifyElement lid $ \lnode ->
        lnode{ elementSiblings = (elementSiblings lnode){ nextSibling = nid } }
      setElement nid $
        node{ elementSiblings = ElementSiblings{ prevSibling = lid, nextSibling = fid } }

  modify $ \doc -> doc{ documentBuildState = (nid:opened) }
  return nid

domtreeEndElement :: Monad m => DocumentT m ElementID
domtreeEndElement = do
  (_:opened) <- gets documentBuildState
  modify $ \doc -> doc{ documentBuildState = opened }
  return $ fromMaybe noElement $ mbHead opened


-- | Traversing document DOM tree
--domtreeTraverse :: (Traversable t, Applicative f) => (ElementRef -> f b) -> Document -> f (t b)
--domtreeTraverse = undefined

-- | Request a redraw for a node
documentRedraw :: Monad m => ElementID -> DocumentT m ()
documentRedraw _nid = return $ warning "TODO: documentRedraw" ()

addEventListener :: Monad m => ElementID -> Text -> EventHandler -> DocumentT m ()
addEventListener nid eventname handler = do
  node <- getElement nid
  let events = elementEvents node
  let events' = case eventname of
        "keyup" -> events{ eventsKeyReleased = handler : eventsKeyReleased events }
        "click" -> events{ eventsMouseReleased = handler : eventsMouseReleased events }
        "input" -> events{ eventsTextInput = handler : eventsTextInput events }
        _ ->
          let otherevts0 =  eventsOther events
              otherevts = M.alter (\vs -> Just (handler : fromMaybe [] vs)) eventname otherevts0
          in events{ eventsOther = otherevts }
  setElement nid $ node{ elementEvents = events' }

-- ElementRef should not live longer that the current version of the IntMap storage.
type ElementRef = (IM.IntMap Element, ElementID)

elementRef :: Document -> ElementID -> ElementRef
elementRef doc nid = (documentAllNodes doc, nid)

elementDeref :: ElementRef -> Element
elementDeref (nodes, nid) = nodes IM.! nid

elementRefID :: ElementRef -> ElementID
elementRefID (_, nid) = nid

elementChildren :: ElementRef -> [ElementRef]
elementChildren (nodes, nid) = maybe [] (nodeChildren . elementContent) $ IM.lookup nid nodes
  where
    nodeChildren (Right fid) | fid /= noElement = go fid fid
    nodeChildren _ = []
    go fid cid =
      let cnode = nodes IM.! cid
          next = nextSibling $ elementSiblings cnode
      in (nodes, cid) : if next == fid then [] else go fid next

elementAncestors :: ElementRef -> [ElementRef]
elementAncestors (_nodes, nid) | nid == noElement = []
elementAncestors (nodes, nid) = (nodes, nid) : elementAncestors (nodes, pid)
  where
    pid = elementParent (nodes IM.! nid)

-- | This is a DOM node
type ElementID = Int

type TagAttrs = (Text, M.Map Text Text)

noElement :: ElementID
noElement =   0

htmlElement :: ElementID
htmlElement = 1

-- intrusive collections:
data ElementSiblings = ElementSiblings
  { prevSibling :: ElementID
  , nextSibling :: ElementID
  }
  deriving (Show)

data Element = Element
  { elementParent :: ElementID
  , elementSiblings :: ElementSiblings

  , elementContent :: Either DOMContent ElementID -- content or the first child
  , elementTag :: !Text
  , elementAttrs :: M.Map Text Text

  , elementStyleHTML :: Style       -- builtin HTML styling
  , elementStyleAttr :: Style       -- style="..."

  , elementEvents :: Events
  }
  deriving (Show)

emptyElement :: Element
emptyElement = Element
  { elementParent = noElement
  , elementSiblings = ElementSiblings{ prevSibling = noElement, nextSibling = noElement }
  , elementContent = Right noElement
  , elementTag = ""
  , elementAttrs = M.empty
  , elementEvents = noEvents
  , elementStyleHTML = noStyle
  , elementStyleAttr = noStyle
  }


--------------------------------------------------------------------------------
-- | Normalize an XML tree of elements and text, possibly with
-- attributes like style and event handlers.

htmlDOMFromXML :: Monad m => XML.Node -> DocumentT m ()
htmlDOMFromXML = \case
  XML.NodeElement el ->
    let attrs = M.mapKeys xmlTextName (XML.elementAttributes el)
        tagattrs = (tagName el, attrs)
    in case htmlDOMContent tagattrs of
      Just content ->
        domAppendHTMLContent (Just tagattrs) content
      _ -> do
        domStartHTMLElement tagattrs
        forM_ (XML.elementNodes el) $ \child ->
          htmlDOMFromXML child
        domEndHTMLElement

  XML.NodeContent txt ->
    domAppendHTMLContent Nothing (TextContent txt)

  _ -> return ()

-- decide if this HTML tag is a piece of content or a container:
htmlDOMContent :: TagAttrs -> Maybe DOMContent
htmlDOMContent = \case
    ("hr", _) ->
      Just $ HorizLineContent

    ("img", attrs) -> do
      case M.lookup "src" attrs of
        Just href ->
          let width = mbReadText =<< M.lookup "width" attrs
              height = mbReadText =<< M.lookup "height" attrs
              wh = liftM2 V2 width height
          in Just $ ImageContent href wh
        _ ->
          let alt = fromMaybe "<img>" $ M.lookup "alt" attrs
          in Just $ TextContent alt

    ("input", attrs) ->
      let value = fromMaybe "" $ M.lookup "value" attrs
      in Just $ case M.lookup "type" attrs  of
          Nothing -> InputTextContent value
          Just "text" -> InputTextContent value
          Just "password" -> InputTextContent value    -- TODO: a password input
          Just "button" -> TextContent value
          Just "checkbox" -> TextContent $ maybe "\x2610" (\_ -> "\x2611") $ M.lookup "checked" attrs
          Just "submit" -> TextContent $ fromMaybe "submit" $ M.lookup "value" attrs
          Just "hidden" -> TextContent ""
          inpty -> warning ("Unknown input type: " ++ show inpty) $ TextContent ""

    _ -> Nothing

domStartHTMLElement :: Monad m => TagAttrs -> DocumentT m ()
domStartHTMLElement tagattrs = do
    nid <- domtreeStartElement tagattrs
    domParseHTMLAttributes nid

domAppendHTMLContent :: Monad m => Maybe TagAttrs -> DOMContent -> DocumentT m ()
domAppendHTMLContent mbTagAttrs content = do
    nid <- domtreeAppendChild mbTagAttrs content
    domParseHTMLAttributes nid

domEndHTMLElement :: Monad m => DocumentT m ()
domEndHTMLElement = do
  nid <- gets documentBuildState
  node <- getElement (head nid)
  void $ domtreeEndElement
  case elementTag node of
    "title" -> return ()    -- TODO: add documentTitle
    "style" -> return ()    -- TODO: parse it or request the resource
    _ -> return ()

domParseHTMLAttributes :: Monad m => ElementID -> DocumentT m ()
domParseHTMLAttributes nid = do
  node <- getElement nid
  let tag = elementTag node
  case elementTag node of
    "head" -> modify $ \doc -> doc{ documentHead = nid }
    "body" -> modify $ \doc -> doc{ documentBody = nid, documentFocus = nid }
    _ -> return ()
  -- TODO: parse class set
  -- TODO: parse id and add globally
  -- TODO: if <img>, add this element to documentImages and request the resource
  let style = M.lookup "style" $ elementAttrs node
  let htmlStyle = builtinHTMLStyleFor tag (elementAttrs node)
  setElement nid $ node
      { elementEvents = fromMaybe noEvents $ HM.lookup tag builtinHTMLEvents
      , elementStyleHTML = fromMaybe noStyle htmlStyle
      , elementStyleAttr = maybe noStyle (cssFarcer . T.unpack) $ style
      }
  whenJust (M.lookup "autofocus" $ elementAttrs node) $ \_ ->
    modify $ \doc -> doc{ documentFocus = nid }

-- elementOwnStyle is useful for computing own style,
-- Do not cascade it over a parent style, use cascadeStyle.
elementOwnStyle :: ElementRef -> Style
elementOwnStyle elt = elementStyleAttr el `overriding` elementStyleHTML el
  where el = elementDeref elt

--------------------------------------------------------------------------------
-- XML Utilities

lookupXMLAttribute :: Text -> XML.Element -> Maybe Text
lookupXMLAttribute attr node = M.lookup xmlattr (XML.elementAttributes node)
  where xmlattr = XML.Name attr Nothing Nothing

tagName :: XML.Element -> Text
tagName = XML.nameLocalName . XML.elementName

xmlTextName :: XML.Name -> Text
xmlTextName name =
  let n = XML.nameLocalName name
  in case XML.nameNamespace name of
    Just ns -> T.concat [ns, ":", n]
    _ -> n

-- Constructing XML
xmlElement' :: Text -> [(Text, Text)] -> [XML.Node] -> XML.Element
xmlElement' name attrs nodes = XML.Element name' attrs' nodes
  where
    name' = XML.Name name Nothing Nothing
    attrs' = M.mapKeys (\a -> XML.Name a Nothing Nothing) $ M.fromList attrs

xmlNode' :: Text -> [(Text, Text)] -> [XML.Node] -> XML.Node
xmlNode' name attrs nodes = XML.NodeElement $ xmlElement' name attrs nodes

xmlElement :: Text -> [XML.Node] -> XML.Element
xmlElement name nodes = xmlElement' name [] nodes

xmlNode :: Text -> [XML.Node] -> XML.Node
xmlNode name nodes = xmlNode' name [] nodes

xmlText :: Text -> XML.Node
xmlText t = XML.NodeContent t

xmlHtml :: XML.Node -> XML.Node
xmlHtml body = xmlNode "html" [ xmlNode "head" [], body ]

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
  | ImageBox (V2 Double) Text
  -- ^  an image of size (V2 Double) and source.
  | InputTextBox (V2 Double) OffsetX BaselineY ElementID
  -- ^ a text input with some text at OffsetX

instance Show InlineContent where
  show (TextBox t baseline) =
    "TextBox " ++ show t ++ " (baseline " ++ show baseline ++ ")"
  show (ImageBox (V2 w h) href) =
    concat ["ImageBox ", show w, "x", show h, " " ++ T.unpack href]
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
  page0 <- layoutPage $ vadoPage vadoWait $ emptyPage window
  renderDOM page0

  args <- Env.getArgs
  let url = fromMaybe "vado:home" $ listToMaybe args
  page1 <- fetchURL url page0
  when debug $ putStrLn $ showdbg (pageDocument page1)

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
  Env.setEnv "SDL_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR" "0"
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
          case boxNode <$> mbHead stack of
            Just nid ->
              dispatchEvent nid event $ warning ("MouseButtonEvent: dispatchEvent @" ++ show nid) page
            Nothing ->
              return $ warning "click event without a box" page
        _ -> return page
      vadoRedrawEventLoop page'

    MouseWheelEvent e -> do
      let V2 _ dy = mouseWheelEventPos e
      vadoRedrawEventLoop $ vadoScroll (negate $ 10 * fromIntegral dy) page

    --MouseMotionEvent _ ->
    --  vadoEventLoop page

    KeyboardEvent e | SDL.keyboardEventKeyMotion e == Released ->
      let focused = documentFocus $ pageDocument page
      in dispatchEvent focused event page >>= vadoRedrawEventLoop

    TextInputEvent _ -> do
      let focused = documentFocus $ pageDocument page
      page' <- dispatchEvent focused event page
      vadoRedrawEventLoop page'

    _ -> do
      vadoEventLoop page

vadoRedrawEventLoop :: Page -> IO ()
vadoRedrawEventLoop page = renderDOM page >> vadoEventLoop page

vadoScroll :: Height -> Page -> Page
vadoScroll dy page = page{ pageScroll = max 0 $ min (pageScroll page + dy) (pageH - vadoViewHeight page) }
  where V2 _ pageH = maybe 0 boxDim $ pageBoxes page

vadoViewHeight :: Page -> Double
vadoViewHeight page = h
  where V2 _ h = pageViewport page


--------------------------------------------------------------------------------
-- Events, event handlers, event dispatch.

type EventHandler = SDL.Event -> ElementID -> Page -> IO Page

-- | A set of events that an element may handle.
-- TODO: refactor to a map + an enum of known enum names
data Events = Events
  { eventsMouseReleased :: [EventHandler]
  , eventsKeyReleased :: [EventHandler]
  , eventsTextInput :: [EventHandler]
  , eventsOther :: M.Map Text [EventHandler]
  }

instance Show Events where
  show _ = "<events>"

noEvents :: Events
noEvents = Events
  { eventsMouseReleased = []
  , eventsKeyReleased = []
  , eventsTextInput = []
  , eventsOther = M.empty
  }

-- TODO: button, modifiers, clientX/clientY
dispatchEvent :: ElementID -> SDL.Event -> Page -> IO Page
dispatchEvent nid event page0 = do
    -- TODO: stopPropagation()
    -- TODO: custom listeners and preventDefault()
    -- TODO: page can change under ElementRefs; a page event loop
    elt <- inPageDocument page0 $ getElementRef nid
    foldM handle page0 (elementAncestors elt)
  where
    handle page node = do
      let events = elementEvents $ elementDeref node
      let listeners = case SDL.eventPayload event of
            MouseButtonEvent _ -> eventsMouseReleased events
            KeyboardEvent _ -> eventsKeyReleased events
            TextInputEvent _ -> eventsTextInput events
            _ -> []
      foldM (\p listener -> listener event (elementRefID node) p) page listeners

builtinHTMLEvents :: HM.HashMap Text Events
builtinHTMLEvents = HM.fromList
  [ ("a", noEvents
      { eventsMouseReleased = [clickLink]
      })
  , ("body", noEvents
      { eventsKeyReleased = [body_onKeyReleased]
      })
  , ("input", noEvents
      { eventsKeyReleased = [input_onKeyReleased]
      , eventsTextInput = [input_onTextInput]
      })
  ]

clickLink :: EventHandler
clickLink _event nid page = do
  node <- inPageDocument page $ getElement nid
  case M.lookup "href" $ elementAttrs node of
    Just href -> do
      layoutPage (vadoPage vadoWait page) >>= renderDOM
      fetchURL (T.unpack href) page >>= layoutPage
    Nothing ->
      return page

noKeyModifiers :: SDL.KeyModifier
noKeyModifiers = SDL.KeyModifier
  { SDL.keyModifierLeftShift = False
  , SDL.keyModifierRightShift = False
  , SDL.keyModifierLeftCtrl = False
  , SDL.keyModifierRightCtrl = False
  , SDL.keyModifierLeftAlt = False
  , SDL.keyModifierRightAlt = False
  , SDL.keyModifierLeftGUI = False
  , SDL.keyModifierRightGUI = False
  , SDL.keyModifierNumLock = False
  , SDL.keyModifierCapsLock = False
  , SDL.keyModifierAltGr = False
  }

isKeyCtrlPressed :: SDL.KeyModifier -> Bool
isKeyCtrlPressed mods =
  (SDL.keyModifierLeftCtrl mods || SDL.keyModifierRightCtrl mods) &&
  (mods{ SDL.keyModifierLeftCtrl=False, SDL.keyModifierRightCtrl=False } == noKeyModifiers)

isKeyAltPressed :: SDL.KeyModifier -> Bool
isKeyAltPressed mods =
  (SDL.keyModifierLeftAlt mods || SDL.keyModifierRightAlt mods) &&
  (mods{ SDL.keyModifierLeftAlt=False, SDL.keyModifierRightAlt=False } == noKeyModifiers)


--- Text input events

modify_input :: (Text -> Text) -> Element -> Element
modify_input f node =
  case elementContent node of
    Left (InputTextContent txt) -> node{ elementContent = Left $ InputTextContent $ f txt }
    _ -> warning ("an input event not on a text input: " ++ show node) node

input_onKeyReleased :: EventHandler
input_onKeyReleased event nid page = runPageDocument page $ do
  let KeyboardEvent e = SDL.eventPayload event
  let keysym = SDL.keyboardEventKeysym e
  case SDL.keysymKeycode keysym of
    SDL.KeycodeBackspace ->
      modifyElement nid $ modify_input (\case "" -> ""; t -> T.init t)
    -- SDL.KeycodeReturn ->
      -- undefined
      -- TODO: find a parent form, use its submit action
      -- emitEvent nid "change"
    -- TODO: Mac (GUI) keyboard layout support:
    SDL.KeycodeV | isKeyCtrlPressed (SDL.keysymModifier keysym) -> do
      clipboard <- lift $ SDL.getClipboardText
      modifyElement nid $ modify_input (const clipboard)
    _ -> return ()
    --  lift $ putStrLn $ "input_onKeyReleased $ " ++ show (SDL.keysymKeycode keysym)
  documentRedraw nid

input_onTextInput :: EventHandler
input_onTextInput event nid page = runPageDocument page $ do
  let TextInputEvent e = SDL.eventPayload event
  let input = SDL.textInputEventText e
  -- lift $ putStrLn $ "input_onTextInput: " ++ T.unpack input
  modifyElement nid $ modify_input (`T.append` input)
  documentRedraw nid

body_onKeyReleased :: EventHandler
body_onKeyReleased event _nid page = do
  let SDL.KeyboardEvent e = SDL.eventPayload event
  case SDL.keysymKeycode $ SDL.keyboardEventKeysym e of
    SDL.KeycodePageUp -> do
      return $ vadoScroll (negate $ vadoViewHeight page) page
    SDL.KeycodePageDown -> do
      return $ vadoScroll (vadoViewHeight page) page
    SDL.KeycodeHome ->
      if isKeyAltPressed (SDL.keysymModifier $ SDL.keyboardEventKeysym e)
      then fetchURL "vado:home" page >>= layoutPage
      else return $ vadoScroll (negate $ pageScroll page) page
    SDL.KeycodeEnd ->
      let pageH = maybe 0 boxHeight $ pageBoxes page
      in return $ vadoScroll pageH page
    _ ->
      return page

--------------------------------------------------------------------------------
-- HTTP request, caches and local storage

data HTTPResource
  = ImageResource !(V2 Double) SDL.Texture

fetchURL :: String -> Page -> IO Page
fetchURL "vado:home" page = return $ vadoPage vadoHome page
fetchURL "vado:error" page = return $ vadoPage (vadoError "vado:error") page
fetchURL url page =
    fetchHTTP url page `Exc.catch`
      \(e :: HttpException) -> return $ vadoPage (vadoError $ T.pack $ show e) page

fetchHTTP :: String -> Page -> IO Page
fetchHTTP url page = do
  startT <- getCPUTime
  let prevuri = pageUrl page
  let absuri =
        if URI.isAbsoluteURI url then fromJust $ URI.parseURI url
        else maybe (error $ "invalid url: " ++ url) (`URI.relativeTo` prevuri) (URI.parseURIReference url)
  request0 <- HTTP.parseRequest (show absuri)
  let pageReq = request0 { HTTP.requestHeaders = [ (HTTP.hUserAgent, "github.com/chrisdone/vado") ] }
  when debug $ print pageReq

  httpman <- HTTP.newManager TLS.tlsManagerSettings
  (body, headers, pageURI) <- HTTP.withResponseHistory pageReq httpman $ \respHistory -> do
    let uri = HTTP.getUri $ HTTP.hrFinalRequest respHistory
    let pageResp = HTTP.hrFinalResponse respHistory
    let headers = HTTP.responseHeaders pageResp
    body <- B.fromChunks <$> HTTP.brConsume (HTTP.responseBody pageResp)
    return (body, headers, uri)

  let contentType = maybe "text/html" (T.toLower . T.decodeLatin1 . snd) $ L.find (\(name, _) -> name == "Content-Type") headers

  if "text/html" `T.isPrefixOf` contentType then do
    let document = HTML.parseLBS body
    let root = XML.documentRoot document
    -- TODO: html attributes, if any
    let htmlnode =
          let topnodes = mapMaybe (\case XML.NodeElement el -> Just el; _ -> Nothing) $ XML.elementNodes root
              headnode = fromMaybe (xmlElement "head" []) $ L.find (\el -> tagName el == "head") topnodes
              bodynode = fromMaybe (xmlElement "body" $ XML.elementNodes root) $ L.find (\el -> tagName el == "body") topnodes
          in xmlNode "html" [XML.NodeElement headnode, XML.NodeElement bodynode]

    let images_axis = (XML.fromDocument document $| XML.descendant &/ XML.element "img")
    let imageurls = mapMaybe (\cursor -> let XML.NodeElement el = XML.node cursor in lookupXMLAttribute "src" el) images_axis
    resources <- forM (S.toList $ S.fromList imageurls) $ \imgurl ->
        fetchResource page{pageUrl=pageURI} httpman imgurl `Exc.catches`
            [ Exc.Handler $ \(e :: HttpException) -> return $ warning (Exc.displayException e) []
            , Exc.Handler $ \(e :: SDL.SDLException) -> return $ warning (Exc.displayException e) []   -- e.g. image decoding
            ]
    endT <- getCPUTime
    let pageloadT = fromIntegral (endT - startT) / (10.0^(12 :: Integer) :: Double)
    putStrLn $ printf "@@@ %s: loaded in %0.3f sec" (show pageURI) pageloadT

    doc <- fromEmptyDocument $ htmlDOMFromXML htmlnode
    when debug $ putStrLn $ showdbg doc
    return page
      { pageUrl = pageURI
      , pageDocument = doc
      , pageResources = M.fromList (concat resources) `mappend` pageResources page
      , pageScroll = 0
      }
  else if "text/" `T.isPrefixOf` contentType then do
    let respbody = T.decodeUtf8 $ B.toStrict body
    doc <- fromEmptyDocument $ htmlDOMFromXML $
      xmlNode "html"
        [ xmlNode "head" []
        , xmlNode "body" [ xmlNode "pre" [ XML.NodeContent respbody ] ]
        ]
    return page
      { pageUrl = pageURI
      , pageDocument = doc
      , pageScroll = 0
      }
  else if "image/" `T.isPrefixOf` contentType then do
  {-
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
        -}
    -- TODO: insert img into resources
    doc <- fromEmptyDocument $ htmlDOMFromXML $
      xmlNode "html"
        [ xmlNode "head" []
        , xmlNode "body" [ xmlNode' "img" [("href", T.pack $ show pageURI)] [] ]
        ]
    return page
      { pageUrl = pageURI
      , pageDocument = doc
      , pageResources = undefined
      , pageScroll = 0
      }
  else
    error $ "TODO: Content-Type " ++ T.unpack contentType


fetchResource :: Page -> HTTP.Manager -> Text -> IO [(Text, HTTPResource)]
fetchResource Page{pageUrl=pageURI, pageResources=pageRes, pageWindow=pageWin} httpman imgurl = do
  case M.lookup imgurl pageRes of
    Just res -> return [(imgurl, res)]
    Nothing -> fetch imgurl >>= maybe (return []) decode
  where
    fetch :: Text -> IO (Maybe Bs.ByteString)
    fetch url | "data:" `T.isPrefixOf` url =
      case Atto.parseOnly attoDataUrl url of
        Right (_, bytes) -> return $ Just bytes
        Left e -> return $ warning (show e) Nothing
    fetch url =
      case parseURIReference (T.unpack url) of
        Nothing ->
          return $ warning ("Could not parse URL: " ++ T.unpack url) Nothing
        Just u -> do
          req <- HTTP.parseUrlThrow (show (u `relativeTo` pageURI))
          putStrLn $ concat
            [ T.unpack $ T.decodeLatin1 $ HTTP.method req, " "
            , if HTTP.secure req then "https://" else "http://"
            , T.unpack $ T.decodeLatin1 $ HTTP.host req
            , case (HTTP.secure req, HTTP.port req) of (True, 443) -> ""; (False, 80) -> ""; (_,p) -> ":" ++ show p
            , T.unpack $ T.decodeLatin1 $ HTTP.path req
            ]
          resp <- HTTP.httpLbs req httpman
          return (Just $ B.toStrict $ HTTP.responseBody resp)

    decode :: Bs.ByteString -> IO [(Text, HTTPResource)]
    decode content =
      case Image.format content of
        Just _ -> do
          bitmap <- Image.decode content
          V2 w h <- SDL.surfaceDimensions bitmap
          let wh = V2 (fromIntegral w) (fromIntegral h)
          texture <- SDL.createTextureFromSurface (vadoRenderer pageWin) bitmap
          return [(imgurl, ImageResource wh texture)]
        _ ->
          return $ warning ("Image format not supported: " ++ T.unpack imgurl) []

attoDataUrl :: Atto.Parser (Text, Bs.ByteString)
attoDataUrl = do
  _ <- Atto.string "data:"
  (mty, msub, _mparams) <- Atto.option ("text", "plain", []) attoMimeType
  -- TODO: use charset from _mparams?
  isBase64 <- Atto.option False (Atto.string ";base64" *> pure True)
  _ <- Atto.char ','
  bytes <- (Bc.pack . T.unpack) <$> Atto.takeText
  let dayta = (if isBase64 then B64.decodeLenient else urlDecode False) bytes
  return (T.concat [mty, "/", msub], dayta)

attoMimeType :: Atto.Parser (Text, Text, [(Text, Text)])
attoMimeType = do
    mtype <- token
    _ <- Atto.char '/'
    msubtype <- token
    params <- Atto.many' (Atto.char ';' *> parameter)
    return (mtype, msubtype, params)
  where
    tsspecials = "()<>@,;:\\\"/[]?="  -- see RFC 2045
    token = T.pack <$> (Atto.many1 $ Atto.satisfy $ \c -> Atto.notInClass tsspecials c && not (C.isSpace c))
    qstr = fail "TODO: quoted strings as mime/type;parameter='value'"
    parameter = do
      attr <- token
      _ <- Atto.char '='
      value <- token <|> qstr
      return (T.toLower attr, value)

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
    let body = elementRef pageDocument (documentBody pageDocument)
    (boxes, _ctx) <- Canvas.withCanvas texture $ elementToBoxes ctx params noStyle body
    when debug $ print boxes
    return page{ pageBoxes=Just boxes }
  where
    ctx = LayoutCtx { ltResources = pageResources }
    params = LayoutParams { ltWidth = w }
    VadoWindow { vadoViewport = V2 w _, vadoTexture = texture } = pageWindow


elementToBoxes :: CanMeasureText m =>
     LayoutCtx -> LayoutParams -> Style -> ElementRef ->
     m (BoxTree, LayoutCtx)
elementToBoxes ctx params parentStyle node = do
    let st = (elementDeref node) `cascadeStyle` parentStyle
    let doLayout = runLayout $ do
            forM_ (elementChildren node) layoutElement
            layoutLineBreak
    (layout', posboxes) <- RWS.execRWST doLayout params Layout
      { ltX = 0, ltY = 0
      , ltMaxX = 0
      , ltLS = LS { lsBoxes = [], lsGap = True, lsWords = [], lsFont = styleFont st }
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
    let st = (elementDeref node) `cascadeStyle` parentStyle
    let styling = st `styleDiff` parentStyle
    modify $ \lt -> lt{ ltStyle=st, ltStyling=styling, ltElement = elementRefID node }
    result <- action
    modify $ \lt -> lt{ ltStyle=parentStyle, ltStyling=parentStyling, ltElement = parentNode }
    return result

layoutElement :: CanMeasureText m => ElementRef -> LayoutOver m ()
layoutElement elt = do
    let st = elementOwnStyle elt
        content = elementContent (elementDeref elt)
        display = st `cssValue` CSSDisplay
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
        font <- gets (styleFont . ltStyle)
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
        let mbResSize = (\(ImageResource wh _) -> wh) <$> M.lookup href resources
        case mbSize <|> mbResSize of
          Just wh -> do
            let baseline = imgBaseline wh elt
            layoutInlineBox wh baseline elt (BoxInline $ ImageBox wh href) []
          _ -> return ()

      (Right _, CSS_Keyword "inline") -> do
        withStyle elt $ forM_ (elementChildren elt) layoutElement

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

      font <- gets (styleFont . ltStyle)
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
  let font = styleFont $ ltStyle lt
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
  when (not (null ws)) $ do
    layoutInlineClose

  boxes <- gets (lsBoxes . ltLS)
  if null boxes then do
    preserveNewine <- gets (stylePreservesNewlines . ltStyle)
    when preserveNewine $ do
      -- advance Y by a line height:
      font <- gets (styleFont . ltStyle)
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
renderDOM Page{ pageBoxes = Nothing } =
  return $ warning "renderDOM: page is not layed out yet" ()
renderDOM page@Page{ pageBoxes = Just body } = do
  let minY = pageScroll page
  let (texture, renderer) = let win = pageWindow page in (vadoTexture win, vadoRenderer win)

  let (stpush, _) = boxStyling body
  let backgroundColor = case M.lookup CSSBackgroundColor stpush of
        Just (CSS_RGB r g b) -> Canvas.rgb r g b
        _ -> Canvas.rgb 255 255 255

  replaced <- Canvas.withCanvas texture $ do
    Canvas.background backgroundColor
    withStyling body (0, 0, noStyle) $ \st ->
      renderTree (pageDocument page) (minY, minY + vadoViewHeight page) (0, 0, st) body
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
            return $ warning ("renderDOM: no resource for <img src=" ++ show (T.unpack href) ++ ">") ()
      other ->
        return $ warning ("renderDOM: unexpected replaced element: " ++ show other) ()

  SDL.present renderer

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
      forM_ (M.toList diff) $ \(prop, val) ->
        case (prop, val) of
          (CSSFont, CSS_Font _) ->
            Canvas.textFont $ styleFont st
          (CSSColor, CSS_RGB r g b) ->
            Canvas.stroke $ Canvas.rgb r g b
          (CSSBackgroundColor, _) ->
            return ()
          other -> return $ warning ("stpush property ignored: " ++ show other) ()

    applyBackgroundColor st (CSS_RGB r g b) = do
      Canvas.stroke $ Canvas.rgb r g b
      Canvas.fill $ Canvas.rgb r g b
      Canvas.rect $ Canvas.D x y w h
      case st `cssValueMaybe` CSSColor of
        Just (CSS_RGB r' g' b') -> Canvas.stroke $ Canvas.rgb r' g' b'
        Just other -> return $ warning ("unknown stroke color: " ++ show other) ()
        Nothing -> return $ warning "no stroke color set, weird" ()
    applyBackgroundColor _ other =
      return $ warning ("Unknown background color: " ++ show other) ()



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
  | CSSTextDecorationLine
  deriving (Show, Eq, Ord, Enum)

cssPropertyNames :: M.Map CSSProperty Text
cssPropertyNames = M.fromList
  [ (CSSBackgroundColor,    "background-color")
  , (CSSColor,              "color")
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
  , (CSSWhiteSpace,         CSS_Keyword "normal")
  , (CSSTextAlign,          CSS_Keyword "left")
  , (CSSTextDecorationLine, CSS_Keyword "none")
  , (CSSFont,               CSS_Font noCSSFont )
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
  | CSS_Pt Double
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
  showdbg (CSS_RGB r g b) = concat ["rgb(", show r, ", ", show g, ", ", show b, ")"]
  showdbg other = "TODO:showdbg(" ++ show other ++ ")"


data CSSFontValue = CSSFontValue
  { cssfontStyle :: Maybe CSSValue
  , cssfontSize :: Maybe CSSValue
  , cssfontWeight :: Maybe CSSValue
  , cssfontFamily :: Maybe CSSValue
  } deriving (Eq)

noCSSFont :: CSSFontValue
noCSSFont = CSSFontValue
  { cssfontStyle = Nothing
  , cssfontSize = Nothing
  , cssfontWeight = Nothing
  , cssfontFamily = Nothing
  }

instance Show CSSFontValue where
  show CSSFontValue{..} =
    L.intercalate ":" $ map (maybe "-" show) [cssfontFamily, cssfontStyle, cssfontSize, cssfontWeight]

mergeCSSFontValues :: CSSFontValue -> CSSFontValue -> CSSFontValue
mergeCSSFontValues font1 font2 = CSSFontValue
    { cssfontStyle = cssfontStyle font1 <|> cssfontStyle font2
    , cssfontSize = cssfontSize font1 <|> cssfontSize font2
    , cssfontWeight = cssfontWeight font1 <|> cssfontWeight font2
    , cssfontFamily = cssfontFamily font1 <|> cssfontFamily font2
    }

-- | Given a node and a parent style, compute cascaded node style
cascadeStyle :: Element -> Style -> Style
cascadeStyle elt parentStyle = Style
    { styleInherit = attrStyle `merge` htmlStyle `merge` styleInherit parentStyle
    , styleOwn = attrOwn `M.union` htmlOwn
    }
  where
    Style{ styleInherit = attrStyle, styleOwn = attrOwn} = elementStyleAttr elt
    Style{ styleInherit = htmlStyle, styleOwn = htmlOwn} = elementStyleHTML elt
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
    { styleInherit = styleInherit newprops `M.union` styleInherit base
    , styleOwn = styleOwn newprops `M.union` styleOwn base
    }

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
css :: [(Text, Text)] -> Style
css properties = Style
    { styleOwn = M.fromListWith mergePropVal own
    , styleInherit = M.fromListWith mergePropVal inherit
    }
  where
    mergePropVal (CSS_Font new) (CSS_Font old) = CSS_Font (mergeCSSFontValues new old)
    mergePropVal new _old = new

    (own, inherit) = partitionEithers $ map desugarValues $ concatMap readProperty properties

    readProperty :: (Text, Text) -> [Either (CSSOwnProperty, CSSValue) (CSSProperty, CSSValue)]
    readProperty (name, textval) =
      case cssReadValue textval of
        Left _ ->
          warning (concat ["Unknown value of a property: ", T.unpack name, "=", T.unpack textval]) []
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
                      let mkFont font = [Right (CSSFont, CSS_Font font)]
                      in case name of
                        "font-weight" -> mkFont $ noCSSFont{ cssfontWeight = Just val }
                        "font-style" -> mkFont $ noCSSFont{ cssfontStyle = Just val }
                        "font-size" -> mkFont $ noCSSFont{ cssfontSize = Just val }
                        "font-family" ->  mkFont $ noCSSFont{ cssfontFamily = Just val }
                        _ -> if "-" `T.isPrefixOf` name then []
                             else warning ("Unknown property: " ++ T.unpack name ++ "=" ++ show val) []

    desugarValues (Right (prop, CSS_Keyword color)) | prop `elem` [CSSColor, CSSBackgroundColor] = Right (prop, color')
      where
        Right color' = cssReadValue $ fromMaybe color $ M.lookup color cssColorAliases  -- yes, it's always Right:
    desugarValues other = other

    cssShorthands = M.fromList
      [ ("text-decoration", expandTextDecoration)
      ]

    expandTextDecoration = \case
      CSS_Keyword kw | kw `elem` ["none", "underline", "overline", "line-through"] ->
        [Right (CSSTextDecorationLine, CSS_Keyword kw)]
      CSS_List vals ->
        concatMap expandTextDecoration vals
      other -> warning ("Unknown value for text-decoration=" ++ show other) []

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


-- | TODO: cursors bitmap collection
-- https://developer.mozilla.org/en-US/docs/Web/CSS/cursor

--------------------------------------------------------------------------------
-- | Default CSS values and getters

-- TODO: sensible choice depending on the OS
defaultFontFace :: String
defaultFontFace = "serif"

defaultFontFaceSans :: String
defaultFontFaceSans = "sans"

defaultFontFaceMono :: String
defaultFontFaceMono = "monospace"

defaultFontSize :: Double
defaultFontSize = 18

styleFontSize :: Style -> Double
styleFontSize st =
  let CSS_Font font = st `cssValue` CSSFont
  in case cssfontSize font of
    Just (CSS_Num size) -> size
    Just (CSS_Px size) -> size
    Just (CSS_Em em) -> defaultFontSize * em
    Just (CSS_Pt pt) -> 1.3333 * pt
    Just other -> warning ("styleFontSize: not computed: " ++ show other) defaultFontSize
    Nothing -> defaultFontSize

styleFont :: Style -> Canvas.Font
styleFont st = Canvas.Font face size weight italic
  where
    CSS_Font font = st `cssValue` CSSFont
    face =
      case cssfontFamily font of
        Just (CSS_String name) -> T.unpack name
        Just (CSS_Keyword name) -> T.unpack name
        Just other -> error $ "styleFont: unknown " ++ show other
        Nothing -> defaultFontFace
    size = styleFontSize st
    weight = (cssfontWeight font == Just (CSS_Keyword "bold"))
    italic = (cssfontStyle font == Just (CSS_Keyword "italic"))

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
bodyStyle = css
  [ ("background-color","white")
  , ("color",           "black")
  , ("font-family",     T.pack $ show defaultFontFace)   -- TODO: default
  , ("font-size",       T.pack $ show defaultFontSize ++ "px")
  , ("font-style",      "normal")   -- TODO: make this default
  , ("font-weight",     "normal")
  , ("margin",          "8px")
  , ("white-space",     "normal")
  ]

-- interactive UI elements must stand out from the surrounding elements,
-- unless explicitly overriden.
uiStyle :: Style
uiStyle = css
  [ ("background-color", "#f8f8f8")
  , ("color", "black")
  , ("font-family", T.pack $ show defaultFontFaceSans)
  , ("font-size", T.pack $ show defaultFontSize ++ "px")
  , ("font-style", "normal")
  , ("font-weight", "normal")
  , ("text-decoration", "none")
  , ("white-space", "pre")
  ]

textinputStyle :: Style
textinputStyle = css
  [ ("border", "rgb(218,218,218) inset 2px")
  , ("cursor", "text")
  , ("display", "inline")    -- "inline-block", actually
  , ("width", "32em")
  ] `overriding` uiStyle

buttonStyle :: Style
buttonStyle = css
  [ ("background-color", "#f8f8f8")
  , ("border", "rgb(218,218,218) outset 2px")
  , ("display", "inline")   -- "inline-block", actually
  , ("padding", "0 0.5em")
  , ("text-align", "center")
  ] `overriding` uiStyle

-- | Default stylings for standard HTML elements.
builtinHTMLStyles :: HM.HashMap Text Style
builtinHTMLStyles = HM.fromList $
     [ ("h1",       css [fontsize (2.00 * defaultFontSize), fontweight_bold])
     , ("h2",       css [fontsize (1.50 * defaultFontSize), fontweight_bold])
     , ("h3",       css [fontsize (1.17 * defaultFontSize), fontweight_bold])
     , ("h4",       css [fontsize (1.00 * defaultFontSize), fontweight_bold])
     , ("h5",       css [fontsize (0.83 * defaultFontSize), fontweight_bold])
     , ("h6",       css [fontsize (0.75 * defaultFontSize), fontweight_bold])
     ] ++
     [ ("a",        css [color "#00e", display_inline, ("text-decoration", "underline")])
     , ("abbr",     inline)
     , ("acronym",  inline)
     , ("b",        css [fontweight_bold, display_inline])
     , ("bdo",      inline)
     , ("big",      css [("font-size", "117%"), display_inline])
     , ("blockquote", css [("margin", "40px 15px")])
     , ("body",     bodyStyle)
     , ("button",   buttonStyle)
     , ("cite",     css [fontstyle_italic, display_inline])
     , ("code",     css [fontfamily defaultFontFaceMono, display_inline])
     , ("dfn",      inline)
     , ("em",       css [fontstyle_italic, display_inline])
     , ("i",        css [fontstyle_italic, display_inline])
     , ("img",      inline)
     , ("input",    textinputStyle)   -- TODO: different types
     , ("kbd",      inline)
     , ("label",    inline)
     , ("li",       css [("display", "list-item")])
     , ("map",      inline)
     , ("mark",     css [("background-color", "yellow"), display_inline])
     , ("object",   inline)
     , ("pre",      css [("white-space", "pre"), ("margin", "13px 0"), fontfamily defaultFontFaceMono])
     , ("q",        inline)
     , ("samp",     inline)
     , ("script",   inline)
     , ("select",   inline)
     , ("small",    css [("font-size", "83%"), display_inline])
     , ("span",     inline)
     , ("strike",   css [("text-decoration", "line-through"), display_inline])
     , ("strong",   css [fontweight_bold, display_inline])
     , ("sub",      inline)
     , ("sup",      inline)
     , ("td",       inline)   -- TODO: proper tables
     , ("th",       inline)   -- TODO: proper tables
     , ("time",     inline)
     , ("textarea", inline)
     , ("tt",       inline)
     , ("u",        css [("text-decoration", "underline"), display_inline])
     , ("var",      inline)
     ] ++
     [ ("script",   nodisplay)
     , ("style",    nodisplay)
     , ("svg",      nodisplay)  -- TODO
     ]
  where
    inline = css [display_inline]
    nodisplay = css [("display", "none")]
    display_inline = ("display", "inline")
    fontsize sz = ("font-size", T.pack (show sz ++ "px"))
    fontstyle_italic = ("font-style", "italic")
    fontweight_bold = ("font-weight", "bold")
    fontfamily fam = ("font-family", T.pack $ show fam)
    color c = ("color", c)

builtinInputStyles :: HM.HashMap (Text, Text) Style
builtinInputStyles = HM.fromList
  [ (("input", "button"),   buttonStyle)
  , (("input", "password"), textinputStyle)
  , (("input", "reset"),    buttonStyle)
  , (("input", "submit"),   buttonStyle)
  , (("input", "text"),     textinputStyle)
  ]

builtinHTMLStyleFor :: Text -> M.Map Text Text -> Maybe Style
builtinHTMLStyleFor tag attrs = do
  case M.lookup "type" attrs of
    Just typeA ->
      case HM.lookup (tag, typeA) builtinInputStyles of
        Just val -> Just val
        Nothing -> HM.lookup tag builtinHTMLStyles
    Nothing -> HM.lookup tag builtinHTMLStyles

-- | Built-in vado:pages

vadoPage :: Document -> Page -> Page
vadoPage doc page = (emptyPage $ pageWindow page)
  { pageDocument = doc
  , pageUrl = nullURI
  }


vadoHome :: Document
vadoHome =
  MId.runIdentity $ fromEmptyDocument $ do
    htmlDOMFromXML $ xmlHtml body
    Just nid <- getElementById "vado"
    addEventListener nid "keyup" url_onKeyReleased
  where
    body =
      xmlNode' "body" [("style", "text-align: center; white-space: pre")]
        [ xmlNode "h1" [ xmlText "\n\n\nVado"]
        , xmlNode "hr" []
        , xmlNode' "form" [("action", "vado:go"), ("method", "POST")]
          [ xmlNode' "input" inputAttrs []
          , xmlNode "br" []
          , xmlNode' "input" [("type", "submit"), ("value", "I go!")] []
          ]
        ]
    inputAttrs = [("type", "text"), ("name", "url"), ("id", "vado"), ("autofocus", "")]

    -- TODO: use a "change" event
    url_onKeyReleased :: EventHandler
    url_onKeyReleased event nid page = do
      let KeyboardEvent e = SDL.eventPayload event
      let keysym = SDL.keyboardEventKeysym e
      case SDL.keysymKeycode keysym of
        SDL.KeycodeReturn -> do
          node <- inPageDocument page $ getElement nid
          let Left (InputTextContent href) = elementContent node
          layoutPage (vadoPage vadoWait page) >>= renderDOM
          fetchURL (T.unpack href) page >>= layoutPage
        _ -> return page


vadoError :: Text -> Document
vadoError err =
  MId.runIdentity $ fromEmptyDocument $ htmlDOMFromXML $
    xmlHtml $ xmlNode "body"
      [ xmlNode' "h1" h1style
         [ xmlText "\noops"]
      , xmlNode "pre" [ xmlText err ]
      ]
  where
    h1style = [("style",  "text-align: center; color: red; white-space: pre; font-size: 72px")]

vadoWait :: Document
vadoWait = MId.runIdentity $ fromEmptyDocument $ htmlDOMFromXML $ xmlHtml body
  where
    body = xmlNode' "body" [("style", "text-align: center")]
          [ xmlNode' "h1" [("style",  "white-space: pre; font-size: 48px")]
              [ xmlText "\n\nloading..."]
          ]
