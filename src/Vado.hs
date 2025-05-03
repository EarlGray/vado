{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{- HLINT ignore "Eta reduce" -}   -- more is less
{- HLINT ignore "Use fmap" -}     -- plain Haskell
{- HLINT ignore "Use second" -}   -- plain Haskell

module Vado where

import           Control.Applicative
import qualified Control.Exception as Err
import           Control.Monad
import           Control.Monad.RWS.Strict as RWS
import           Control.Monad.State (StateT (..))
import qualified Control.Monad.State as St
import           Control.Monad.Except (ExceptT)
import qualified Control.Monad.Except as Exc
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Bc
import qualified Data.Either as Ei
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S

-- import           System.CPUTime (getCPUTime)
import qualified Data.StateVar as SV
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Foreign.C.Types (CInt)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.URI as URIh
import           Network.URI as URI
import qualified SDL
import qualified SDL.Cairo as Cairo
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Canvas as Canvas
import           SDL.Event as SDL
import qualified SDL.Image as Image
import           SDL.Vect
import qualified System.Environment as Env
import           System.Exit (exitSuccess)

import           Vado.CSS
import           Vado.Document
import           Vado.Layout
import qualified Vado.Resource as Resource
import           Vado.Types

--------------------------------------------------------------------------------
-- Navigator, Window, DOM

-- | The navigator is a "browser" abstraction.
-- |   - stores settings and the collection of windows.
-- |   - does network requests and caching.
data Page = Page
  { pageState :: PageState
  , pageDocument :: Document
  , pageEvents :: IM.IntMap {-ElementID-} Events
  , pageBoxes :: Maybe BoxTree
  --, pageCSSRules :: CSSRules
  -- ^ Rendering tree

  , pageUrl :: URI
  , pageHistory :: ([URI], [URI])   -- back, forward
  , pageDebugNetwork :: Bool

  , pageResMan :: Resource.Manager
  , pageWindow :: VadoWindow
  }

-- | Set of states in the page state machine:
data PageState
  = PageConnecting      -- page at uri: awaiting headers or connection error
  | PageStreaming       -- streaming page and a set of resources
  | PageReady           -- DOM and resources are loaded.
  deriving (Eq, Show)

newtype BrowserT m a = BrowserT { runBrowserT :: StateT Page m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Page, MonadTrans)
type Browser a = BrowserT IO a

modifyPageWith :: (Page -> Browser ()) -> (Page -> IO (Maybe Page)) -> Browser ()
modifyPageWith ifChanged action = do
  page0 <- get
  mbPage <- liftIO $ action page0
  whenJust mbPage ifChanged

modifyPage :: (Page -> IO (Maybe Page)) -> Browser ()
modifyPage = modifyPageWith put

changePage :: (Page -> IO (Maybe Page)) -> Browser ()
changePage = modifyPageWith (\page -> renderPage page >> put page)

inPageDocument :: Monad m => Page -> DocumentT m a -> m a
inPageDocument page f = inDocument (pageDocument page) f

runPageDocument :: Monad m => Page -> DocumentT m () -> m Page
runPageDocument page f = do
  doc' <- runDocument (pageDocument page) f
  return page{ pageDocument = doc' }

inBrowserDocument :: Monad m => DocumentT m a -> BrowserT m a
inBrowserDocument f = do
  doc <- gets pageDocument
  lift $ inDocument doc f

withBrowserDocument :: Monad m => DocumentT m a -> BrowserT m a
withBrowserDocument f = do
  doc <- gets pageDocument
  (result, doc') <- lift $ St.runStateT f doc
  modify $ \page -> page{ pageDocument = doc' }
  return result

logDebug :: String -> Browser ()
logDebug msg = do
  debug <- gets pageDebugNetwork
  when debug $ logInfo msg

-- | Entry point for layout procedure
layoutPage :: Page -> IO Page
layoutPage page@Page {..}
  | documentBody pageDocument == noElement =
      return $ warning "layoutPage: no body" page
layoutPage page = do
  let doc = pageDocument page
  let ctx = LayoutCtx {ltResources = documentResources doc}
  let VadoWindow {windowViewport = V2 w _, windowTexture = texture} = pageWindow page
  let params = LayoutParams {ltWidth = w}
  let body = elementRef doc (documentBody doc)
  (boxes, _ctx) <- Cairo.withCairoTexture' texture $ Canvas.runCanvas $ elementToBoxes ctx params noStyle body
  return page {pageBoxes = Just boxes}

renderPage :: (MonadIO m) => Page -> m ()
renderPage Page {pageBoxes = Nothing} =
  logWarn "renderPage: page is not layed out yet"
renderPage page@Page {pageBoxes = Just body, pageDocument = doc, pageWindow = win} = liftIO $ do
  let minY = pageScroll page
  let (texture, renderer) = (windowTexture win, windowRenderer win)

  let (stpush, _) = boxStyling body
  let backgroundColor = case M.lookup CSSBackgroundColor stpush of
        Just (CSS_RGB r g b) -> Canvas.rgb r g b
        _ -> Canvas.rgb 255 255 255

  replaced <- Cairo.withCairoTexture' texture $ Canvas.runCanvas $ do
    Canvas.background backgroundColor
    withStyling body (0, 0, noStyle) $ \st ->
      renderTree doc (minY, minY + vadoViewHeight page) (0, 0, st) body
  SDL.copy renderer texture Nothing Nothing

  forM_ replaced $ \(rect, content) -> do
    case content of
      ImageBox _ href ->
        case M.lookup href (documentResources doc) of
          Just (ImageResource _ imgtexture, _) -> do
            let cint x = fromIntegral (round x :: Int)
            let SDL.Rectangle (P (V2 x y)) (V2 dx dy) = rect
            let pos = P $ V2 (cint x) (cint (y - minY))
            let dim = V2 (cint dx) (cint dy)
            let rect' = SDL.Rectangle pos dim
            SDL.copy renderer imgtexture Nothing (Just rect')
          _
            | href `M.member` documentResourcesLoading doc ->
                return () -- Still loading
          _ ->
            logWarn $ "renderPage: no resource for <img src=" ++ show href ++ ">"
      other ->
        logWarn $ "renderPage: unexpected replaced element: " ++ show other

  SDL.present renderer


navigate :: Maybe Resource.Content -> URI -> Page -> IO Page
navigate mbSend uri page =
  if uriScheme uri == "vado:" then
    case uriPath uri of
      "home" -> do
        -- TODO: cancel all pending resource requests; clear resources
        page1 <- layoutPage $ builtinPage vadoHome page
        Just nid <- inPageDocument page1 $ getElementById "vado"
        return (addEventListener nid "keyup" vadoHome_onKeyReleased page1)
          { pageHistory = historyRewind (pageHistory page1) (pageUrl page1) uri
          }
      _ ->
        error $ "navigate: unknown address " ++ show uri
  else do
    let resman = pageResMan page
    let fullURI = uri `URI.relativeTo` documentLocation (pageDocument page)
    -- TODO: check if fullURI is canonicalized:
    when (pageDebugNetwork page) $ logInfo $ "navigatePage " ++ show fullURI
    case mbSend of
        Nothing -> Resource.requestGet resman fullURI ["text/html"]
        Just content -> Resource.requestPost resman fullURI content ["text/html"]
    return $ page
      { pageState = PageConnecting
      , pageUrl = fullURI
      , pageHistory = historyRewind (pageHistory page) (pageUrl page) fullURI
      }
  -- TODO: loading UI indicator, e.g. refresh button -> stop button

navigatePage :: URI -> Page -> IO Page
navigatePage = navigate Nothing

webSearch :: String -> URI
webSearch s = fromJust $ URI.parseAbsoluteURI $ "https://google.com/search?q=" ++ es
  where es = URI.escapeURIString URI.isAllowedInURI s

--------------------------------------------------------------------------------
-- History

historyRewind :: ([URI], [URI]) -> URI -> URI -> ([URI], [URI])
historyRewind (back, forward) old new =
  case (back, forward) of
    (prev:pback, _) | prev == new ->
      (pback, old:forward)
    (_, next:nforward) | next == new ->
      (old:back, nforward)
    _ ->
      (old:back, [])

printHistory :: ([URI], [URI]) -> URI -> IO ()
printHistory (back, forward) current = mapM_ putStrLn $
    ["History:"] ++ map indent (reverse back) ++ [">>" ++ show current] ++ map indent forward
  where
    indent url = ' ':' ':show url

--------------------------------------------------------------------------------
-- Main entry point and event handler

-- | Main entry point.
--
-- * Read in commandline argument with the URL.
-- * Make a request to the URL.
-- * Create an SDL window.
-- * Render the content.
--
vadoMain :: IO ()
vadoMain = do
  args <- Env.getArgs
  window <- vadoWindow
  resman <- Resource.runManager

  page0 <- layoutPage $ builtinPage vadoWait $ Page
    { pageState = PageReady
    , pageDocument = emptyDocument
    , pageEvents = IM.empty
    , pageBoxes = Nothing
    , pageResMan = resman
    , pageUrl = fromJust $ URI.parseURI "vado:home"
    , pageHistory = ([], [])
    , pageDebugNetwork = False
    , pageWindow = window
    }
  renderPage page0

  let address = fromMaybe "vado:home" $ mbHead args
  let url = fromMaybe (webSearch address) $ URI.parseURI address
  flip St.evalStateT page0 $ runBrowserT $ do
    changePage $ \page -> Just <$> navigatePage url page
    asyncEventLoop

--------------------------------------------------------------------------------
-- window rendering and events

data VadoWindow = VadoWindow
  { windowRenderer :: SDL.Renderer
  , windowTexture :: SDL.Texture
  , windowViewport :: V2 Double
  , windowScroll :: Height
  --, windowScale :: V2 Double
  }

pageScroll :: Page -> Height
pageScroll = windowScroll . pageWindow

pageViewport :: Page -> V2 Double
pageViewport = windowViewport . pageWindow

vadoViewHeight :: Page -> Double
vadoViewHeight page = h
  where V2 _ h = pageViewport page

defaultWindowSize :: Num n => V2 n
defaultWindowSize = V2 800 600

vadoScroll :: Height -> Page -> Page
vadoScroll dy page = page{ pageWindow = (pageWindow page){ windowScroll = scroll } }
  where
    V2 _ pageH = maybe 0 boxDim $ pageBoxes page
    scroll = max 0 $ min (pageScroll page + dy) (pageH - vadoViewHeight page)

-- | Create window and texture
vadoWindow :: IO VadoWindow
vadoWindow = do
  Env.setEnv "SDL_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR" "0"
  SDL.initialize [SDL.InitVideo, SDL.InitTimer, SDL.InitEvents]
  window <-
    SDL.createWindow
      "Vado"
      SDL.defaultWindow
      { SDL.windowHighDPI = False
      , SDL.windowResizable = True
      , SDL.windowInitialSize = defaultWindowSize
      }
  -- Setup Cairo rendering on the window.
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  rscale <- SV.get (SDL.rendererScale renderer)
  logInfo $ "SDL.rendererScale = " ++ show rscale

  logicalResolution <- SV.get (SDL.rendererLogicalSize renderer)
  logInfo $ "logical resolution: " ++ show logicalResolution

  Just (SDL.Rectangle _ viewport@(V2 vw vh)) <- SDL.get (SDL.rendererViewport renderer)
  logInfo $ "viewport: " ++ show viewport

  let scale = V2 (vw `doubleDiv` ww) (vh `doubleDiv` wh)
        where
          V2 ww wh = defaultWindowSize :: V2 Int
          doubleDiv x y = fromIntegral x / fromIntegral y :: Double
  logInfo $ "scale: " ++ show scale

  texture0 <- Cairo.createCairoTexture renderer viewport
  return $ VadoWindow
    { windowRenderer = renderer
    , windowViewport = fromIntegral <$> viewport
    , windowTexture = texture0
    , windowScroll = 0
    }

--------------------------------------------------------------------------------
-- Page event loop.

fps :: CInt
fps = 60

asyncEventLoop :: Browser ()
asyncEventLoop = forever $ do
  -- UI events
  mbUIEvent <- liftIO $ SDL.waitEventTimeout (1000 `div` fps)
  whenJust mbUIEvent $ \event ->
    handleUIEvent event

  -- ResourceManager events
  resman <- gets pageResMan
  resEvents <- liftIO $ Resource.drainEvents resman
  forM_ resEvents $ \(u, event) -> do
    uri <- gets pageUrl
    st <- gets pageState
    loading <- gets (documentResourcesLoading . pageDocument)
    case event of
      Resource.EventStreamChunk _ -> return ()
      _ -> logDebug $ "[" ++ show st ++ "] " ++ show (u, event)

    if u == uri then
      handlePageStreaming st event
    else if u `M.member` loading then
      handleResourceEvent st u event
    else
      logWarn $ "eventloop: unknown resource " ++ show u ++ " for event " ++ show event

  -- DOM-emitted events
  resReqs <- withBrowserDocument drainDocumentEvents
  forM_ resReqs $ \event ->
    handleDOMEvent event


handlePageStreaming :: PageState -> Resource.EventKind -> Browser ()
handlePageStreaming st event = do
  case (st, event) of
    (_, Resource.EventConnectionError exc) | st `elem` [PageConnecting, PageStreaming] ->
      changePage $ \page -> do
        when (pageDebugNetwork page) $ print exc
        let doc = vadoError $ T.pack $ show exc
        return $ Just $ builtinPage doc page

    (PageConnecting, Resource.EventStreamMetadata (req, _resp) _mbStream) ->
      changePage $ \page -> do
        when (pageDebugNetwork page) (print req) -- TODO: response too
        let finalURI = HTTP.getUri req
        return $ Just page
          { pageState = PageStreaming
          , pageDocument = emptyDocument{ documentLocation = finalURI }
          , pageEvents = IM.empty
          , pageBoxes = Nothing
          , pageWindow = (pageWindow page){ windowScroll = 0 }
          }

    (PageStreaming, Resource.EventStreamChunk (Resource.ChunkHtml update)) -> do
      -- grow the DOM tree:
      debug <- gets pageDebugNetwork
      when debug $
        logWarn $ show update
      withBrowserDocument $ htmlDOMFromEvents update

    (PageStreaming, Resource.EventStreamClose) ->
      changePage $ \page -> do
        Just <$> layoutPage page{ pageState = PageReady }

    _ ->
      logWarn $ "eventloop: invalid state " ++ show st ++ " for event " ++ show event
    {-
    (Resource.EventContentReady (_req, _resp) _content ->
      -- TODO: check content-type, wrap standalone resource in html
      -- TODO: construct the page, go to PageReady/PageWaitResources
      error $ "TODO: eventloop: non-streaming page"
    -}


handleResourceEvent :: PageState -> URI -> Resource.EventKind -> Browser ()
handleResourceEvent st uri event =
  case (st, event) of
    (_, Resource.EventContentReady (_req, resp) content) -> do
      wereLoading <- gets (documentResourcesLoading . pageDocument)
      let nodes = wereLoading M.! uri
      withBrowserDocument $ do
        loading <- gets (M.delete uri . documentResourcesLoading)
        when (null loading) $ logInfo "All resources are ready"
        modify $ \doc -> doc {documentResourcesLoading = loading}

      let contentType = Resource.httpResponseContentType resp
      logDebug $
        "OK: "
          ++ show uri
          ++ " ("
          ++ T.unpack contentType
          ++ ") loaded: "
          ++ show (HTTP.responseStatus resp)

      mbRes <- decodeResourceContent contentType content
      case mbRes of
        Left oops -> logWarn $ "ERROR: <" ++ show uri ++ ">: " ++ oops
        Right (CSSResource blocks) -> do
          logWarn $ "Parsed " ++ show (length blocks) ++ " CSS rules from " ++ show uri
          withBrowserDocument $ modify $ \doc ->
            doc {documentCSSRules = addCSSRules blocks $ documentCSSRules doc}

          -- re-match the rules in the document
          allNodes <- fmap IM.toList $ inBrowserDocument $ gets documentAllNodes
          rules <- inBrowserDocument $ gets documentCSSRules
          changed <- fmap concat $ forM allNodes $ \(nid, node) -> do
            eltref <- inBrowserDocument $ getElementRef nid
            let tagattrs = (elementTag node, elementAttrs node)
            let newchains = rulechainsForMatcher rules tagattrs $ domMatchSelector eltref

            let sheet0 = elementStylesheet node
            if newchains == stylesheetAuthorChain sheet0
              then return []
              else do
                let (impchain, regchain) = newchains
                let style = (styleFromRulechain rules impchain, styleFromRulechain rules regchain)
                let sheet = sheet0 {stylesheetAuthor = style}
                withBrowserDocument $
                  setElement nid $
                    node {elementStylesheet = sheet {stylesheetComputed = computedStyle sheet}}
                return [nid]

          when (nonEmpty changed) $ do
            logDebug $ "Relayout needed for nodes: " ++ show changed
            changePage $ \p -> Just <$> layoutPage p
        Right res@(ImageResource _ _) -> do
          withBrowserDocument $ modify $ \doc ->
            doc {documentResources = M.insert uri (res, nodes) (documentResources doc)}

          -- TODO: replace with re-layout requests when incremental layout is ready.
          -- Are there <img>s without specified width= and height= that need relayout?
          needsLayout <- inBrowserDocument $ do
            doc <- get
            let maybeWH nid =
                  let attrs = elementAttrs $ elementDeref $ elementRef doc nid
                      mbW = decodeAttr "width" attrs :: Maybe Double
                      mbH = decodeAttr "height" attrs :: Maybe Double
                   in liftA2 V2 mbW mbH
            return $ any (isNothing . maybeWH) $ S.toList nodes
          when needsLayout $ do
            logDebug $ "Relayout caused by " ++ show uri
            modifyPage $ \p -> Just <$> layoutPage p
          get >>= renderPage -- new image must be shown in any case
    (_, Resource.EventConnectionError exc) | st /= PageConnecting -> do
      logDebug $ "ERROR: " ++ show uri ++ " : " ++ show exc
      withBrowserDocument $ do
        loading <- gets (M.delete uri . documentResourcesLoading)
        when (null loading) $ logInfo "All resources are ready"
        modify $ \doc -> doc {documentResourcesLoading = loading}
    _ ->
      logWarn $ "eventloop: invalid state " ++ show st ++ " for event " ++ show event

-- TODO: move out of browser, pass the renderer only
decodeResourceContent :: Text -> Resource.Content -> Browser (Either String HTTPResource)
decodeResourceContent contentType = \case
  Resource.ContentBytes bs
    | T.isPrefixOf "image/" contentType -> do
        renderer <- gets (windowRenderer . pageWindow)
        liftIO $ Exc.runExceptT $ withImageSurface (contentType, bs) $ \bitmap -> do
          texture <- SDL.createTextureFromSurface renderer bitmap
          V2 w h <- SDL.surfaceDimensions bitmap
          let wh = V2 (fromIntegral w) (fromIntegral h)
          pure $ ImageResource wh texture
  Resource.ContentBytes bs | T.isPrefixOf "text/css" contentType -> do
    -- TODO: handle non-utf8 encodings properly
    let txt = Ei.fromRight (TE.decodeLatin1 bs) $ TE.decodeUtf8' bs
    let rules = cssParser txt
    return $ Right (CSSResource rules)
  _content ->
    return $ Left ("decodeResourceContent: unknown Content-Type=" ++ T.unpack contentType)

withImageSurface :: (Text, ByteString) -> (SDL.Surface -> IO a) -> ExceptT String IO a
withImageSurface (contentType, bs) action =
  if isJust (Image.format bs) then do
    result <- liftIO (Err.try (Image.decode bs) :: IO (Either Err.SomeException SDL.Surface))
    case result of
      Right sfc -> liftIO $ action sfc
      Left e -> Exc.liftEither $ Left $ "Image.decode error: " ++ show e
  else Exc.liftEither $ Left $ "decodeResourceContent: an unknown image format: " ++ T.unpack contentType

--------------------------------------------------------------------------------
-- DOM events, event handlers, event dispatch.

handleDOMEvent :: DocumentEvent -> Browser ()
handleDOMEvent (DocEmitEvent nid evt) = do
  modifyPage (\p -> Just <$> dispatchEvent (Left evt) nid p)
handleDOMEvent (DocResourceRequest nid resuri) = do
  resman <- gets pageResMan
  if URI.uriScheme resuri /= "blob:"
    then do
      logDebug $ "eventloop: @" ++ show nid ++ " requests " ++ show resuri
      withBrowserDocument $ do
        resources <- gets documentResources
        wereLoading <- gets documentResourcesLoading
        unless (resuri `M.member` wereLoading || resuri `M.member` resources) $
          liftIO $
            Resource.requestGet resman resuri []

        let multimapAdd v = M.alter (\vs -> Just (v `S.insert` fromMaybe S.empty vs))
        let nowLoading = multimapAdd nid resuri wereLoading
        modify $ \doc -> doc {documentResourcesLoading = nowLoading}
    else do
      -- assumption: the only source of blobs are <img src="data:...">
      node <- inBrowserDocument $ getElement nid
      case M.lookup "src" $ elementAttrs node of
        Just dataurl -> decodeResourceDataUrl dataurl
        _ -> logWarn $ "no src in @" ++ show nid
  where
    decodeResourceDataUrl href = do
      case Resource.decodeDataURL href of
        Left e ->
          logWarn $ "failed to decode data url in @" ++ show nid ++ ": " ++ e
        Right (conttype, content) -> do
          eiRes <- decodeResourceContent conttype content
          case eiRes of
            Left e ->
              logWarn ("failed to decode resource in src=data: of @"++show nid ++ ": "++ e)
            Right res ->
              withBrowserDocument $ modify $ \doc -> do
                let resources0 = documentResources doc
                let resources = M.insert resuri (res, S.singleton nid) resources0
                doc{ documentResources = resources }


data EventContext = EventContext
  { evctxPage :: Page
  , evctxStopPropagation :: Bool
  , evctxPreventDefault :: Bool
  }

type EventHandler = Either EventName SDL.Event -> ElementID -> StateT EventContext IO ()

-- | A set of events that an element may handle.
-- TODO: refactor to a map + an enum of known enum names
data Events = Events
  { eventsMouseReleased :: [EventHandler]
  , eventsKeyReleased :: [EventHandler]
  , eventsTextInput :: [EventHandler]
  , eventsOther :: M.Map EventName [EventHandler]
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
dispatchEvent :: Either EventName SDL.Event -> ElementID -> Page -> IO Page
dispatchEvent event nid0 page0 = do
    let ctx = EventContext
          { evctxPage=page0
          , evctxPreventDefault=False
          , evctxStopPropagation=False
          }
    -- TODO: the "capture" propagation
    evctxPage <$> St.execStateT (bubbleUp nid0) ctx
  where
    getNodeEvents :: ElementID -> Page -> Events
    getNodeEvents nid page = fromMaybe noEvents $ IM.lookup nid $ pageEvents page

    bubbleUp :: ElementID -> StateT EventContext IO ()
    bubbleUp nid | nid == noElement =
      return ()
    bubbleUp nid = do
      page <- gets evctxPage
      -- custom events:
      forM_ (listeners $ getNodeEvents nid page) $ \listener -> listener event nid

      -- builtin events:
      node <- inPageDocument page $ getElement nid
      preventDefault <- gets evctxPreventDefault
      unless preventDefault $
        whenJust (HM.lookup (elementTag node) builtinHTMLEvents) $ \events -> do
          forM_ (listeners events) $ \listener -> listener event nid

      stopProp <- gets evctxStopPropagation
      unless stopProp $ bubbleUp (elementParent node)

    listeners :: Events -> [EventHandler]
    listeners events = case event of
      Right evsdl -> case SDL.eventPayload evsdl of
        MouseButtonEvent _ -> eventsMouseReleased events
        KeyboardEvent _ -> eventsKeyReleased events
        TextInputEvent _ -> eventsTextInput events
        _ -> []
      Left evdom ->
        fromMaybe [] $ evdom `M.lookup` eventsOther events

addEventListener :: ElementID -> EventName -> EventHandler -> Page -> Page
addEventListener nid eventname handler page =
  let eventmap = pageEvents page
      events = fromMaybe noEvents $ IM.lookup nid eventmap
      events' = case eventname of
        "keyup" -> events{ eventsKeyReleased = handler : eventsKeyReleased events }
        "click" -> events{ eventsMouseReleased = handler : eventsMouseReleased events }
        "input" -> events{ eventsTextInput = handler : eventsTextInput events }
        _ ->
          let otherevts0 =  eventsOther events
              otherevts = M.alter (\vs -> Just (handler : fromMaybe [] vs)) eventname otherevts0
          in events{ eventsOther = otherevts }
  in page{ pageEvents = IM.insert nid events' eventmap }


--------------------------------------------------------------------------------
-- UI events

eventHandlerWithPage :: (Page -> IO Page) -> StateT EventContext IO ()
eventHandlerWithPage f = do
  page <- gets evctxPage
  page' <- liftIO $ f page
  modify $ \ctx -> ctx{ evctxPage = page' }


handleUIEvent :: SDL.Event -> Browser ()
handleUIEvent event =
    case SDL.eventPayload event of
      SDL.QuitEvent ->
        SDL.quit >> liftIO exitSuccess

      SDL.WindowResizedEvent e -> changePage $ \page -> do
        let size = windowResizedEventSize e
        let win = pageWindow page
        SDL.destroyTexture $ windowTexture win
        texture' <- Cairo.createCairoTexture (windowRenderer win) (fromIntegral <$> size)
        let win' = win{ windowTexture=texture', windowViewport=fromIntegral <$> size }
        -- TODO: optimize, don't do full layout again:
        Just <$> layoutPage page{ pageWindow=win' }

      SDL.KeyboardEvent e | SDL.keyboardEventKeyMotion e == Released ->
        changePage $ \page -> do
          -- print e
          let focused = documentFocus $ pageDocument page
          Just <$> dispatchEvent (Right event) focused page

      SDL.TextInputEvent _ -> changePage $ \page -> do
        let focused = documentFocus $ pageDocument page
        Just <$> dispatchEvent (Right event) focused page

      SDL.MouseWheelEvent e -> changePage $ \page -> do
        let V2 _ dy = mouseWheelEventPos e
        return $ Just $ vadoScroll (negate $ 10 * fromIntegral dy) page

      SDL.MouseButtonEvent e | SDL.mouseButtonEventMotion e == Released -> do
        let P (V2 xi yi) = mouseButtonEventPos e
        mbBoxes <- gets pageBoxes
        whenJust mbBoxes $ \boxes -> do
          scrollY <- gets pageScroll
          let pagepos = V2 (fromIntegral xi) (fromIntegral yi + scrollY)
          let stack = P pagepos `findInBox` boxes
          case boxNode <$> mbHead stack of
            Nothing -> logWarn "click event without a box"
            Just nid -> do
              node <- inBrowserDocument $ getElement nid
              when (isJust $ elementTabIndex node) $ do
                -- TODO: emit "blur" event for documentFocus
                withBrowserDocument $ modify $ \doc -> doc{ documentFocus = nid }
                logInfo $ "MouseButtonEvent: focused @" ++ show nid
                -- TODO: emit "focus" event for nid
              logInfo $ "MouseButtonEvent: dispatchEvent @" ++ show nid
              changePage $ \page -> Just <$> dispatchEvent (Right event) nid page

      SDL.KeyboardEvent _ -> return ()
      SDL.MouseMotionEvent _ -> return ()
      _ -> return () --liftIO $ print event

clickLink :: EventHandler
clickLink _event nid = do
  page <- gets evctxPage
  node <- inPageDocument page $ getElement nid
  whenJust ("href" `M.lookup` elementAttrs node) $ \href ->
    whenJust (URI.parseURIReference $ T.unpack href) $ \uri -> do
      page' <- liftIO $ navigatePage uri page
      put EventContext
        { evctxPage=page'
        , evctxStopPropagation=True
        , evctxPreventDefault=True
        }

-- Keyboard handlers helpers

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

isKeyCtrlShiftPressed :: SDL.KeyModifier -> Bool
isKeyCtrlShiftPressed mods =
  (SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods) &&
  isKeyCtrlPressed mods{ SDL.keyModifierLeftShift=False, SDL.keyModifierRightShift=False }

isKeyCtrlPressed :: SDL.KeyModifier -> Bool
isKeyCtrlPressed mods =
  (SDL.keyModifierLeftCtrl mods || SDL.keyModifierRightCtrl mods) &&
  (mods{ SDL.keyModifierLeftCtrl=False, SDL.keyModifierRightCtrl=False } == noKeyModifiers)

isKeyAltPressed :: SDL.KeyModifier -> Bool
isKeyAltPressed mods =
  (SDL.keyModifierLeftAlt mods || SDL.keyModifierRightAlt mods) &&
  (mods{ SDL.keyModifierLeftAlt=False, SDL.keyModifierRightAlt=False } == noKeyModifiers)


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
  , ("form", noEvents
      { eventsOther = M.fromList [("change", [form_onSubmit])]
      })
  ]

--- Text input events

inputModify :: (Text -> Text) -> Element -> Element
inputModify f node =
  case elementContent node of
    Left (InputTextContent txt) -> node{ elementContent = Left $ InputTextContent $ f txt }
    _ -> warning ("an input event not on a text input: " ++ show node) node

input_onKeyReleased :: EventHandler
input_onKeyReleased event nid =
  eventHandlerWithPage $ \page -> runPageDocument page $ do
    -- TODO: it's not always a text input
    let mbKeysym = case event of
          Right evsdl ->
            case SDL.eventPayload evsdl of
              KeyboardEvent e -> Just (SDL.keyboardEventKeysym e)
              _ -> Nothing
          _ -> Nothing
    --let Right evsdl = event
    --let KeyboardEvent e = SDL.eventPayload evsdl
    --let keysym = SDL.keyboardEventKeysym e
    whenJust mbKeysym $ \keysym ->
      case SDL.keysymKeycode keysym of
        SDL.KeycodeBackspace ->
          modifyElement nid $ inputModify (\case "" -> ""; t -> T.init t)
        SDL.KeycodeReturn ->
          documentEnqueue $ DocEmitEvent nid "change"
        -- TODO: Mac (GUI) keyboard layout support:
        SDL.KeycodeV | isKeyCtrlPressed (SDL.keysymModifier keysym) -> do
          clipboard <- lift SDL.getClipboardText
          modifyElement nid $ inputModify (const clipboard)
        _ -> return ()
        --  lift $ putStrLn $ "input_onKeyReleased $ " ++ show (SDL.keysymKeycode keysym)
    documentRedraw nid

input_onTextInput :: EventHandler
input_onTextInput event nid =
  eventHandlerWithPage $ \page ->
    runPageDocument page $
      forM_ event $ \evsdl -> do
        case SDL.eventPayload evsdl of
          TextInputEvent e -> do
            let input = SDL.textInputEventText e
            -- lift $ putStrLn $ "input_onTextInput: " ++ T.unpack input
            modifyElement nid $ inputModify (`T.append` input)
            documentRedraw nid
          _ -> pure ()

{- HLINT ignore "Use camelCase" -}
body_onKeyReleased :: EventHandler
body_onKeyReleased (Right (SDL.Event _ (SDL.KeyboardEvent e))) _nid = do
  let modifiers = SDL.keysymModifier $ SDL.keyboardEventKeysym e
  page <- gets evctxPage
  -- putStrLn $ "body_onKeyReleased: " ++ show e
  page' <- liftIO $ case SDL.keysymKeycode $ SDL.keyboardEventKeysym e of
    SDL.KeycodeE | isKeyCtrlShiftPressed modifiers -> do
      let debug = not $ pageDebugNetwork page
      putStrLn $ "Setting pageDebugNetwork to " ++ show debug
      return page {pageDebugNetwork = debug}
    SDL.KeycodeEnd
      | modifiers == noKeyModifiers ->
          let pageH = maybe 0 boxHeight $ pageBoxes page
            in return $ vadoScroll pageH page
    SDL.KeycodeH | isKeyCtrlShiftPressed modifiers -> do
      printHistory (pageHistory page) (pageUrl page) -- show History
      return page
    SDL.KeycodeHome
      | isKeyAltPressed modifiers ->
          navigatePage (fromJust $ URI.parseURI "vado:home") page
    SDL.KeycodeHome
      | modifiers == noKeyModifiers ->
          return $ vadoScroll (negate $ pageScroll page) page
    SDL.KeycodeI | isKeyCtrlShiftPressed modifiers -> do
      putStrLn (showdbg $ pageDocument page) -- Inspect the DOM tree:
      return page
    SDL.KeycodeK
      | isKeyCtrlShiftPressed modifiers ->
          -- Inspect rendering boxes:
          print (pageBoxes page) >> return page
    SDL.KeycodeLeft | isKeyAltPressed modifiers ->
      case pageHistory page of
        (prevurl : _, _) -> navigatePage prevurl page
        _ -> return page
    SDL.KeycodePageDown -> do
      return $ vadoScroll (vadoViewHeight page) page
    SDL.KeycodePageUp -> do
      return $ vadoScroll (negate $ vadoViewHeight page) page
    SDL.KeycodeQ
      | isKeyCtrlPressed modifiers ->
          SDL.quit >> liftIO exitSuccess >> return page
    SDL.KeycodeR
      | isKeyCtrlPressed modifiers ->
          navigatePage (pageUrl page) page
    SDL.KeycodeRight | isKeyAltPressed modifiers ->
      case pageHistory page of
        (_, nexturl : _) -> navigatePage nexturl page
        _ -> return page
    _ ->
      return page
  modify $ \ctx -> ctx {evctxPage = page'}

body_onKeyReleased event _nid =
  logWarn $ "onKeyReleased: wrong event " ++ show event

form_onSubmit :: EventHandler
form_onSubmit event formID = do
  logWarn $ "form_onSubmit: @" ++ show formID ++ " event=" ++ show event
  page <- gets evctxPage

  -- gather the parameters:
  formref <- inPageDocument page $ getElementRef formID
  let controls = querySelectorsAllRef formControlSelectors formref
  let values = mapMaybe (nodeFormValue . elementDeref) controls

  let attrs = elementAttrs $ elementDeref formref
  let formAction = fromMaybe undefined $ M.lookup "action" attrs -- TODO: default to docuri
  case URI.parseURIReference $ T.unpack formAction of
    Nothing ->
      logWarn $ "form_onSubmit: cannot parse action=" ++ T.unpack formAction
    Just formURI -> do
      page' <- liftIO $ case maybe "get" T.toLower $ M.lookup "method" attrs of
        "get" -> do
          let values' = map (\(k, v) -> (k, listToMaybe v)) values
          let bsQuery = URIh.renderQuery True $ URIh.queryTextToQuery values'
          let queryURI = formURI {uriQuery = Bc.unpack bsQuery}
          logWarn $ "form_onSubmit: method=GET action=" ++ show queryURI
          navigatePage queryURI page
        "post" -> do
          -- TODO: check enctype
          let fields = M.fromListWith mappend values
          logWarn $ "form_onSubmit: method=POST action=" ++ show formURI ++ " form=" ++ show fields
          liftIO $ navigate (Just $ Resource.ContentForm fields) formURI page
        other ->
          return $ warning ("form_onSubmit: invalid method=" ++ T.unpack other) page

      put
        EventContext
          { evctxPage = page',
            evctxStopPropagation = True,
            evctxPreventDefault = True
          }
  where
    formControlSelectors = [SelTag "input", SelTag "select", SelTag "textarea"]

    nodeFormValue :: Element -> Maybe (Text, [Text])
    nodeFormValue node
      | elementTag node == "input" =
          let attrs = elementAttrs node
           in case M.lookup "name" attrs <|> M.lookup "id" attrs of -- TODO: check if can use "id"
                Nothing -> warning ("Form control without name" ++ show node) Nothing
                Just name ->
                  case M.lookup "type" attrs of
                    Just ty
                      | ty `elem` ["hidden", "submit"] ->
                          Just (name, maybeToList $ M.lookup "value" attrs)
                    _ ->
                      case elementContent node of
                        Left (InputTextContent value) -> Just (name, [value])
                        other -> warning ("Unexpected content in <input>: " ++ show other) $ Just (name, [])
    nodeFormValue node =
      warning ("form_onSubmit: cannot extract value from " ++ show node) Nothing

--------------------------------------------------------------------------------
-- vado: builtin pages

builtinPage :: Document -> Page -> Page
builtinPage doc page =
  page
    { pageState = PageReady,
      pageDocument = doc,
      pageBoxes = Nothing,
      pageEvents = IM.empty,
      pageWindow = (pageWindow page) {windowScroll = 0}
    }

vadoHome_onKeyReleased :: EventHandler
vadoHome_onKeyReleased (Right (Event _ (KeyboardEvent e))) nid = do
  let keysym = SDL.keyboardEventKeysym e
  case SDL.keysymKeycode keysym of
    SDL.KeycodeReturn -> do
      -- get the address
      page <- gets evctxPage
      node <- inPageDocument page $ getElement nid
      liftIO $ print node
      let address =
            case elementContent node of
              Left (InputTextContent addr) -> addr
              content -> error $ "vadoHome_onKeyReleased: wrong node content " ++ show content
      -- set up the waiting page
      liftIO $ do
        page' <- layoutPage $ builtinPage vadoWait page
        renderPage page'
      -- decide where to go
      let mbHref = URI.parseURI $ T.unpack address
      let href = fromMaybe (webSearch $ T.unpack address) mbHref
      page' <- liftIO $ navigatePage href page
      put
        EventContext
          { evctxPage = page',
            evctxStopPropagation = True,
            evctxPreventDefault = True
          }
    _ ->
      return ()
vadoHome_onKeyReleased event _nid =
  logWarn $ "vddo:home onKeyReleased: wrong event " ++ show event
