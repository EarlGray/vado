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
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.String
import           Data.Text ( Text)
import qualified Data.Text as T
import           Linear.V2 (V2(..))
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Client.Internal
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Header as HTTP
import           Network.URI
import qualified SDL as SDL
import qualified SDL.Cairo as Cairo
import qualified SDL.Cairo.Canvas as Canvas
import qualified SDL.Image as Image
import           SDL.Event as SDL
import           SDL.Vect
import           System.Environment
import qualified Text.HTML.DOM as DOM
import qualified Text.XML as XML

--------------------------------------------------------------------------------
-- Types

-- | Some content, either a block or inline element, or some text.
data Content
  = ElementContent !Text !Events !Style ![Content]
  | TextContent !Text
  | ImageContent !Text !(Maybe (V2 Double)) !(Maybe SDL.Surface) -- !(Maybe Text)
  | NewlineContent
  | LineContent
  deriving (Show)

instance Show SDL.Surface where
  show _ = "<image>"
instance Show Events where
  show _ = "<events>"

-- | Style for an element. Inheritable values are Maybes.
data Style = Style
  { styleMargin :: !Double
  , stylePadding :: !Double
  , styleDisplay :: !Display
  , styleWidth :: !(Maybe Double)
  , styleBackgroundColor :: !(Maybe Canvas.Color)
  , styleColor :: !(Maybe Canvas.Color)
  , styleFontWeight :: !(Maybe FontWeight)
  , styleFontSize :: !(Maybe Double)
  , styleLineHeight :: !(Maybe Double)
  , styleFontStyle :: !(Maybe FontStyle)
  , styleFontFamily :: !(Maybe String)
  } deriving (Show)

-- | Display style.
data Display = BlockDisplay | InlineDisplay
  deriving (Show, Eq)

-- | Font style.
data FontStyle = NormalStyle | ItalicStyle
  deriving (Show, Eq)

-- | Font weight.
data FontWeight = NormalWeight | BoldWeight
  deriving (Show, Eq)

-- | A box to be displayed.
data Box
  = TextBox !Events !TextBox
  | ImageBox !Canvas.Dim SDL.Surface    -- an image
  | LineBox !Canvas.Dim                 -- a horizontal line

instance Show Box where
  show (TextBox _ t) = "TextBox <events> " ++ show t
  show (ImageBox dim _) = concat ["ImageBox ", show dim, " <image>"]
  show (LineBox dim) = "LineBox " ++ show dim

-- | A set of events that an element may handle.
data Events = Events
  { eventsClick :: Maybe ((URI -> IO ()) -> IO () -> IO ())
    -- ^ A handler accepting two arguments: either a loadUrl or a "do
    -- nothing" action. This was thrown together at the last minute to
    -- support clicking links.
  }

-- | A box of text to be rendered on the screen at the given
-- coordinates with the given style.
data TextBox = Text
  { textXY :: !(V2 Double)
  , textWH :: !(V2 Double)
  , textColor :: !Canvas.Color
  , textFont :: !String
  , textWeight :: !FontWeight
  , textStyle :: !FontStyle
  , textSize :: !Double
  , textText :: !Text
  } deriving (Show)

-- | A text size measurer.
data MeasureEnv = MeasureEnv
  { measureScale :: Double
  , measureMaxWidth :: Double
  , measureMaxHeight :: Double
  }

newtype Measuring m a =
  Measuring {runMeasuring :: RWST MeasureEnv [Box] LS m a}
  deriving (Monad, Functor, Applicative, MonadTrans)

type CanvasMeasuring a = Measuring Canvas.Canvas a

measureInlineBox :: Double -> Double -> CanvasMeasuring Canvas.Dim
measureInlineBox dx dy = Measuring $ do
  ls <- RWS.get
  let ls' = ls { lsX = lsX ls + dx, lsLineHeight = max (lsLineHeight ls) dy }
  RWS.put ls'
  return $ Canvas.D (lsX ls) (lsY ls) dx dy

measureBlockBox :: Double -> Double -> CanvasMeasuring Canvas.Dim
measureBlockBox dx dy = do
  measureLineBreak
  dim <- measureInlineBox dx dy
  measureLineBreak
  return dim

measureLineBreak :: CanvasMeasuring ()
measureLineBreak = Measuring $ do
  ls <- RWS.get
  let dy = lsLineHeight ls
  let ls' = ls { lsX = 0, lsY = lsY ls + dy, lsLineHeight = 0 }
  RWS.put ls'

measureOverLine :: Double -> CanvasMeasuring Bool
measureOverLine width = Measuring $ do
  maxwidth <- RWS.asks measureMaxWidth
  x <- RWS.gets lsX
  return ((x + width) > maxwidth)

measureTell :: Box -> CanvasMeasuring ()
measureTell box = Measuring $ RWS.tell [box]


-- | Line-state. The state of rendering line-broken text.
data LS = LS
  { lsX :: Double
  , lsY :: Double
  , lsLineHeight :: Double
    -- ^ The line height gets increased every time we render a font
    -- larger than the last rendered thing.
  } deriving (Show)

-- | State for the event handler.
data EV = EV
  { evRequest :: Request
  , evContent :: Content
  , evBoxes :: [Box]
  , evScrollY :: Double
  , evTexture :: SDL.Texture
  , evRenderer :: SDL.Renderer
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
  url:_ <- getArgs -- Get the URL.
  request0 <- makeRequest url
  content0 <- getContent request0
  -- Initialize SDL and create a window.
  setEnv "SDL_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR" "0"
  SDL.initialize [SDL.InitVideo, SDL.InitTimer, SDL.InitEvents]
  window <-
    SDL.createWindow
      (T.concat ["Vado - ", T.pack url])
      SDL.defaultWindow
      { SDL.windowHighDPI = True
      , SDL.windowResizable = True
      , SDL.windowInitialSize = defaultWindowSize
      }
  -- Setup Cairo rendering on the window.
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  Just (SDL.Rectangle _ viewport@(V2 w h)) <-
    SDL.get (SDL.rendererViewport renderer)
  texture0 <- Cairo.createCairoTexture renderer viewport
  -- Render the page immediately.
  let ev0 = EV request0 content0 [] 0 texture0 renderer
  let scale@(_, scaley) =
        let (V2 w0 h0) = defaultWindowSize
        in (fromIntegral w / w0, fromIntegral h / h0)
  boxes0 <- rerender scaley ev0
  -- Setup an event loop to handle events or quit. Re-render on each event.
  -- This set of arguments would be better collapsed into a record.
  eloop scale ev0 {evBoxes = boxes0}

-- | Event loop.
eloop :: (Double, Double) -> EV -> IO ()
eloop scale@(scalex, scaley) ev = do
  event <- waitEvent
  case eventPayload event of
    QuitEvent -> return ()
    WindowClosedEvent {} -> return ()
    MouseButtonEvent e ->
      case mouseButtonEventMotion e of
        Released -> do
          case find
                 (overlaps
                    (let P (V2 w h) = mouseButtonEventPos e
                     in P
                          (V2
                             (fromIntegral w * scalex)
                             (fromIntegral h * scaley))) .
                  snd)
                 (reverse (mapMaybe getClickEvent (evBoxes ev))) of
            Just (handler, _) ->
              let loadUrl uri = do
                    request' <- setUriRelative (evRequest ev) uri
                    putStrLn ("Downloading: " ++ show request')
                    content' <- getContent request'
                    let scrollY' = 0
                    boxes' <-
                      rerender
                        scaley
                        ev {evContent = content', evScrollY = scrollY'}
                    eloop
                      scale
                      ev
                      { evScrollY = scrollY'
                      , evBoxes = boxes'
                      , evContent = content'
                      , evRequest = request'
                      }
                  continue = eloop scale ev
              in handler loadUrl continue
            _ -> eloop scale ev
        _ -> eloop scale ev
    MouseWheelEvent e -> do
      let ev' =
            ev
            { evScrollY =
                min
                  0
                  (evScrollY ev +
                   (let V2 _ y = mouseWheelEventPos e
                    in fromIntegral y * scaley * 5))
            }
      boxes' <- rerender scaley ev'
      eloop scale ev' {evBoxes = boxes'}
    WindowResizedEvent e -> do
      texture' <-
        Cairo.createCairoTexture
          (evRenderer ev)
          (fmap fromIntegral (windowResizedEventSize e))
      let ev' = ev {evTexture = texture'}
      boxes' <- rerender scaley ev'
      eloop scale ev' {evBoxes = boxes'}
    _ -> do
      eloop scale ev

--------------------------------------------------------------------------------
-- Web request

-- | Make a web request and then parse the HTML, convert it to the
-- more normalized Content type.
getContent :: HTTP.Request -> IO Content
getContent request = do
  -- Make a blocking request to the URL.
  manager <- HTTP.newManager TLS.tlsManagerSettings
  response <- HTTP.httpLbs request manager
  -- Parse the response body as possibly malformed HTML and convert that to an XML tree.
  let doc = XML.documentRoot (DOM.parseLBS (HTTP.responseBody response))
      content = elementToContent doc
  fetchImages (manager, request) content

fetchImages :: (HTTP.Manager, HTTP.Request) -> Content -> IO Content
fetchImages http (ElementContent a b c elements) = do
  elements' <- mapM (fetchImages http) elements
  return (ElementContent a b c elements')
fetchImages (manager, req) (ImageContent src dim0 Nothing) = do
  let Just uri = parseURIReference (T.unpack src)
  req' <- setUriRelative req uri
  putStrLn ("Downloading: " ++ show req')
  resp <- HTTP.httpLbs req' manager
  let body = B.toStrict $ HTTP.responseBody resp
  case Image.format body of
    Just _ -> do
      img <- Image.decode body
      V2 dx dy <- SDL.surfaceDimensions img
      let dim = dim0 <|> Just (V2 (fromIntegral dx) (fromIntegral dy))
      return $ ImageContent src dim (Just img)
    _ ->
      return $ ImageContent src dim0 Nothing
fetchImages _ t = return t

makeRequest :: String -> IO Request
makeRequest url = do
  request0 <- HTTP.parseRequest (fromString url)
  let req = request0 { requestHeaders = [ (HTTP.hUserAgent, "github.com/chrisdone/vado") ] }
  putStrLn ("Downloading: " ++ show req)
  return req

--------------------------------------------------------------------------------
-- Mouse events

-- | Does the point overlap the rectangle of text? Text is rendered
-- above the y, not below it. So that explains the calculation below.
overlaps :: Point V2 Double -> Canvas.Dim -> Bool
overlaps (P (V2 x y)) (Canvas.D px py0 pw ph) =
  x >= px && y >= py && x <= px + pw && y <= py + ph
  where
    py = py0 - ph

-- | If an element has a click event, i.e. anchor elements, extract that.
getClickEvent :: Box -> Maybe (((URI -> IO ()) -> IO () -> IO ()), Canvas.Dim)
getClickEvent =
  \case
    TextBox events t -> do
      handler <- eventsClick events
      let V2 x y = textXY t
          V2 w h = textWH t
      pure (handler, Canvas.D x y w h)
    _ -> Nothing

--------------------------------------------------------------------------------
-- Converting XML tree to a normalized content tree

-- | Normalize an XML tree of elements and text, possibly with
-- attributes like style and event handlers.
xmlToContent :: XML.Node -> Maybe Content
xmlToContent = \case
    XML.NodeElement element -> do
      let tag = T.toLower (XML.nameLocalName (XML.elementName element))
      if tag == "img" then do
        src <- lookupXMLAttribute "src" element
        let w = decodeXMLAttribute "width" element
        let h = decodeXMLAttribute "height" element
        let dim = V2 <$> w <*> h
        Just $ ImageContent src dim Nothing
      else if tag == "hr" then do
        Just LineContent
      else if tag == "br" then
        Just NewlineContent
      else if tag `elem` ignoreElements
        then Nothing
        else Just (elementToContent element)
    XML.NodeContent t ->
      if T.null (T.strip t)
        then Nothing
        else Just (TextContent (T.unwords (T.words t)))
    _ -> Nothing
  where
    ignoreElements = ["head", "script", "style", "input"]

-- | Convert an element to some content.
elementToContent :: XML.Element -> Content
elementToContent element = ElementContent name (eventsFor name) style nodes
  where
    name = T.toLower (XML.nameLocalName (XML.elementName element))
    eventsFor "a" = defaultEvents { eventsClick = Just (clickLink element) }
    eventsFor _ = defaultEvents
    style = fromMaybe defaultStyle (HM.lookup name elementStyles)
    nodes = mapMaybe xmlToContent (XML.elementNodes element)

clickLink :: XML.Element -> (URI -> IO ()) -> IO () -> IO ()
clickLink element loadUrl continue = maybe continue loadUrl maybeURI
  where
    maybeURI = (parseURIReference . T.unpack) =<< maybeAttr
    maybeAttr = M.lookup "href" $ XML.elementAttributes element

--------------------------------------------------------------------------------
-- Laying out content to a list of absolutely-positioned boxes

-- | Convert an element to boxes.
blockToBoxes :: Bool -> Events -> Style -> [Content] -> CanvasMeasuring ()
blockToBoxes isInline parentEvents parentStyle nodes = do
  maxwidth <- Measuring $ RWS.asks measureMaxWidth
  maxheight <- Measuring $ RWS.asks measureMaxHeight
  y <- Measuring $ RWS.gets lsY
  when (y <= maxheight) $ do
    forM_ nodes $ \case
        ImageContent _ (Just (V2 dx dy)) (Just img) -> do
          dim <- measureInlineBox dx dy
          measureTell $ ImageBox dim img
        ImageContent _ _ _ ->
          return ()
        LineContent -> do
          dim <- measureBlockBox maxwidth defaultFontSize
          measureTell $ LineBox dim
        NewlineContent -> do
          measureLineBreak
        TextContent t -> do
          textToBoxes parentEvents parentStyle t
        ElementContent _ events style subnodes ->
          let isInline' = isInline || (styleDisplay style == InlineDisplay)
              style' = mergeStyles parentStyle style
          in blockToBoxes isInline' events style' subnodes
    unless isInline $ measureLineBreak

-- | Layout text word-by-word with line-breaking.
textToBoxes :: Events -> Style -> Text -> CanvasMeasuring ()
textToBoxes events style text = do
  scale <- Measuring $ RWS.asks measureScale
  let font = fromMaybe defaultFontFace (styleFontFamily style)
  let fontstyle = fromMaybe defaultFontStyle (styleFontStyle style)
  let fontsize = scale * fromMaybe defaultFontSize (styleFontSize style)
  let weight = fromMaybe defaultWeight (styleFontWeight style)
  let space = fontsize / 2
  let lineheight = fontsize * fromMaybe defaultLineHeight (styleLineHeight style)

  lift $ Canvas.textFont (Canvas.Font font fontsize (weight == BoldWeight) (fontstyle == ItalicStyle))
  extents <- lift Canvas.fontExtents

  forM_ (T.words text) $ \word -> do
    wh@(V2 width _) <- lift $ Canvas.textSize (T.unpack word)
    overline <- measureOverLine width
    when overline $ measureLineBreak
    Canvas.D x y _ _ <- measureInlineBox (width + space) lineheight
    let t = Text { textXY = V2 x (y + Canvas.fontExtentsHeight extents)
           , textWH = wh
           , textText = word
           , textColor = fromMaybe defaultColor (styleColor style)
           , textSize = scale * fromMaybe defaultFontSize (styleFontSize style)
           , textFont = font
           , textWeight = weight
           , textStyle = fontstyle
           }
    measureTell $ TextBox events t


--------------------------------------------------------------------------------
-- SDL rendering to the canvas

-- | Re-render the canvas. The boxes are computed by blockToBoxes,
-- this function simply renders the boxes that it's told to render.
--
-- Later, it might calculate the height of the document, then create a
-- fresh texture which it could scroll simply by passing extra
-- arguments to copy of a region to copy offset by some Y.
rerender :: Double -> EV -> IO [Box]
rerender scale ev = do
  boxes <-
    Canvas.withCanvas (evTexture ev) $ do
      Canvas.background $ Canvas.rgb 255 255 255
      (V2 width height) <- Canvas.getCanvasSize
      (_, boxes) <-
        RWS.execRWST
          (runMeasuring
             (blockToBoxes False defaultEvents defaultStyle [evContent ev]))
          (MeasureEnv scale width height)
          LS { lsX = 0, lsY = evScrollY ev, lsLineHeight = 0 }
      forM_ boxes $ \box ->
         case box of
             LineBox (Canvas.D x y dx dy) -> do
               let from = V2 x (y + dy/2)
               let to = V2 (x + dx) (y + dy/2)
               Canvas.stroke defaultColor
               Canvas.line from to
             TextBox _ text -> do
               Canvas.stroke (textColor text)
               Canvas.textFont
                 (Canvas.Font
                    (textFont text)
                    (textSize text)
                    (textWeight text == BoldWeight)
                    (textStyle text == ItalicStyle))
               Canvas.textBaseline (T.unpack (textText text)) (textXY text)
             _ -> return ()
      pure boxes
  SDL.copy (evRenderer ev) (evTexture ev) Nothing Nothing
  forM_ boxes $ \case
    ImageBox (Canvas.D x y dx dy) img -> do
      texture <- SDL.createTextureFromSurface (evRenderer ev) img
      let pos = V2 (round x) (round y)
      let size = V2 (round  dx) (round dy)
      let dim = SDL.Rectangle (P pos) size
      SDL.copy (evRenderer ev) texture Nothing (Just dim)
    _ -> return ()
  SDL.present (evRenderer ev)
  pure boxes

--------------------------------------------------------------------------------
-- Styles

-- | Merge the inherited style and the element style.
mergeStyles :: Style -> Style -> Style
mergeStyles inherited element =
  Style
  { styleMargin = styleMargin element
  , stylePadding = stylePadding element
  , styleBackgroundColor =
      styleBackgroundColor element <|> styleBackgroundColor inherited
  , styleColor = styleColor element <|> styleColor inherited
  , styleFontFamily = styleFontFamily element <|> styleFontFamily inherited
  , styleFontWeight = styleFontWeight element <|> styleFontWeight inherited
  , styleDisplay = styleDisplay element
  , styleFontSize = styleFontSize element <|> styleFontSize inherited
  , styleFontStyle = styleFontStyle element <|> styleFontStyle inherited
  , styleWidth = styleWidth element <|> styleWidth inherited
  , styleLineHeight = styleLineHeight element <|> styleLineHeight inherited
  }

-- | Default style.
defaultStyle :: Style
defaultStyle =
  Style
  { styleMargin = 0
  , stylePadding = 0
  , styleBackgroundColor = Nothing
  , styleColor = Nothing
  , styleFontWeight = Nothing
  , styleDisplay = BlockDisplay
  , styleFontFamily = Nothing
  , styleFontSize = Nothing
  , styleWidth = Nothing
  , styleLineHeight = Nothing
  , styleFontStyle = Nothing
  }

-- | Default stylings for standard HTML elements.
elementStyles :: HM.HashMap Text Style
elementStyles =
  HM.fromList
    ([ ( T.pack ("h" ++ show n)
       , defaultStyle
         {styleFontSize = Just size, styleFontWeight = Just BoldWeight})
     | (n :: Int, size :: Double) <- zip [1 .. 6] [40, 35, 30, 25, 20, 18]
     ] ++
     [ ("a",        inline {styleColor = Just (Canvas.blue 255) })
     , ("abbr",     inline)
     , ("acronym",  inline)
     , ("b",        inline)
     , ("bdo",      inline)
     , ("big",      inline {styleFontSize = Just (1.2 * defaultFontSize)})
     , ("button",   inline)
     , ("cite",     inline)
     , ("code",     inline {styleFontFamily = Just "monospace"})
     , ("dfn",      inline)
     , ("em",       inline {styleFontStyle = Just ItalicStyle})
     , ("i",        inline {styleFontStyle = Just ItalicStyle})
     , ("img",      inline)
     , ("input",    inline)
     , ("kbd",      inline)
     , ("label",    inline)
     , ("map",      inline)
     , ("object",   inline)
     , ("q",        inline)
     , ("samp",     inline)
     , ("script",   inline)
     , ("select",   inline)
     , ("small",    inline {styleFontSize = Just (0.8 * defaultFontSize)})
     , ("span",     inline)
     , ("strong",   inline {styleFontWeight = Just BoldWeight})
     , ("sub",      inline)
     , ("sup",      inline)
     , ("time",     inline)
     , ("textarea", inline)
     , ("tt",       inline)
     , ("var",      inline)
     ])
  where
    inline = defaultStyle {styleDisplay = InlineDisplay}

-- | Default text color.
defaultColor :: Canvas.Color
defaultColor = Canvas.rgb 0 0 0

-- | Default font-size.
defaultFontSize :: Double
defaultFontSize = 15

defaultWeight :: FontWeight
defaultWeight = NormalWeight

defaultFontStyle :: FontStyle
defaultFontStyle = NormalStyle

defaultLineHeight :: Double
defaultLineHeight = 1.5

defaultWindowSize :: Num n => V2 n
defaultWindowSize = V2 800 600

defaultFontFace :: String
defaultFontFace = "Arial"

defaultEvents :: Events
defaultEvents = Events Nothing

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
