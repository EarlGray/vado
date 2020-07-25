{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Vado where

import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Identity as MId
import           Control.Monad.RWS.Strict as RWS
import           Control.Monad.State (StateT(..))
import qualified Control.Monad.State as St
import qualified Data.ByteString.Char8 as Bc
import qualified Data.Char as C
import qualified Data.Either as Ei
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import           Data.Maybe
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Foreign.C.Types (CInt)
import           Linear.V2 (V2(..))
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.URI as URIh
import           Network.URI as URI
import qualified SDL
import qualified SDL.Cairo as Cairo
import qualified SDL.Cairo.Canvas as Canvas
import           SDL.Event as SDL
import qualified SDL.Image as Image
import           SDL.Vect
--import           System.CPUTime (getCPUTime)
import qualified System.Environment as Env
import           System.Exit (exitSuccess)
import           Text.Read (readMaybe)
import qualified Text.XML as XML
import qualified Data.XML.Types as XMLTy

import           Vado.Types
import qualified Vado.Resource as Resource
import           Vado.CSS


-- Meaningful names:

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = forM_

mbHead :: [a] -> Maybe a
mbHead = listToMaybe

mbReadText :: Read a => Text -> Maybe a
mbReadText = readMaybe . T.unpack

intmapAppend :: a -> IM.IntMap a -> IM.IntMap a
intmapAppend val im | IM.null im = IM.fromList [(0, val)]
intmapAppend val im = let (i, _) = IM.findMax im in IM.insert (i+1) val im

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

withPage :: (Page -> IO a) -> Browser a
withPage action = get >>= (liftIO . action)

modifyPage :: (Page -> IO (Maybe Page)) -> Browser ()
modifyPage action = do
  page0 <- get
  mbPage <- liftIO $ action page0
  whenJust mbPage put

changePage :: (Page -> IO (Maybe Page)) -> Browser ()
changePage action = do
  page0 <- get
  mbPage <- liftIO $ action page0
  whenJust mbPage $ \page -> do
    page' <- case pageBoxes page of
      Nothing -> liftIO $ layoutPage page
      _ -> return page
    liftIO $ renderDOM page'
    put page'

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

runBrowserDocument :: Monad m => DocumentT m () -> BrowserT m ()
runBrowserDocument f = BrowserT $ do
  doc <- gets pageDocument
  doc' <- lift $ runDocument doc f
  modify $ \page -> page{ pageDocument = doc' }

withBrowserDocument :: Monad m => DocumentT m a -> BrowserT m a
withBrowserDocument f = do
  doc <- gets pageDocument
  (result, doc') <- lift $ St.runStateT f doc
  modify $ \page -> page{ pageDocument = doc' }
  return result

logDebug :: String -> Browser ()
logDebug msg = do
  debug <- gets pageDebugNetwork
  when debug $ liftIO $ putStrLn msg

logWarn :: MonadIO m => String -> m ()
logWarn msg = liftIO $ putStrLn msg

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

  , documentLocation :: URI
  , documentXMLType :: Maybe XML.Doctype
  , documentResources :: DOMResourceMap
  , documentResourcesLoading :: M.Map URI (S.Set ElementID)   -- to remember what's in flight
  , documentEvents :: [DocumentEvent]   -- to queue requests from inside DocumentT
  , documentCSSRules :: CSSRules

  , documentHead :: ElementID
  , documentBody :: ElementID
  , documentFocus :: ElementID      -- the keyboard focus
  , documentImages :: IM.IntMap ElementID
  , documentTitle :: Text
  -- , documentContentType :: ContentType
  -- , documentMeta :: M.Map Text Text
  -- , documentElementsByClass :: M.Map Text [ElementID]
  -- , documentElementById :: M.Map Text ElementID
  } deriving (Show)

emptyDocument :: Document
emptyDocument = Document
  { documentAllNodes = IM.fromList [(htmlElement, documentElement)]
  , documentNewID = htmlElement + 1
  , documentBuildState = [htmlElement]
  , documentLocation = nullURI
  , documentXMLType = Nothing
  , documentResources = M.empty
  , documentResourcesLoading = M.empty
  , documentEvents = []
  , documentCSSRules = emptyCSSRules
  , documentHead = noElement
  , documentBody = noElement
  , documentTitle = ""
  , documentFocus = noElement
  , documentImages = IM.empty
  }

documentElement :: Element
documentElement = Element
  { elementParent = noElement
  , elementSiblings = ElementSiblings htmlElement htmlElement
  , elementContent = Right noElement
  , elementTag = ""
  , elementAttrs = M.empty
  , elementStylesheet = emptyStylesheet
  }

instance HasDebugView Document where
  showdbg doc = unlines $
      [ "documentHead = @" ++ show (documentHead doc)
      , "documentBody = @" ++ show (documentBody doc)
      , "documentTitle = " ++ T.unpack (documentTitle doc)
      , "documentNewID = @" ++ show (documentNewID doc)
      , "documentFocus = @" ++ show (documentFocus doc)
      , "documentCSSRules: " ++ show (documentCSSRules doc)
      , "documentResources = " ++ show (documentResources doc)
      ] ++ shownode (elementRef doc htmlElement)
    where
      shownode :: ElementRef -> [String]
      shownode elt =
        let nid = elementRefID elt
            node = elementDeref elt
            tag = elementTag node
            attrs = elementAttrs node
            st = elementStylesheet node
            hdr = ["@"++show nid++":"++T.unpack tag++" "++show (M.toList attrs)++
                   " style="++showdbg (stylesheetComputed st)]
        in case elementContent node of
          Right _ ->
            hdr ++ indent (L.concatMap shownode (elemrefChildren elt))
          Left content ->
            hdr ++ indent [show content]

      indent :: [String] -> [String]
      indent = map (\s -> ' ':' ':s)

type DocumentT m a = StateT Document m a

docWarn :: Monad m => String -> DocumentT m ()
docWarn msg = modify $ \doc -> warning msg doc

-- | Run a computation in DocumentT, return the result, discard document changes.
inDocument :: Monad m => Document -> DocumentT m a -> m a
inDocument doc f = St.evalStateT f doc

-- | Run an action on DocumentT, return a changed document.
runDocument :: Monad m => Document -> DocumentT m () -> m Document
runDocument doc f = St.execStateT f doc

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

fromEmptyDocument :: Monad m => DocumentT m () -> m Document
fromEmptyDocument f = runDocument emptyDocument f

domtreeAppendChild :: Monad m =>
                      ElementID -> Maybe (Text, M.Map Text Text) -> DOMContent
                      -> DocumentT m ElementID
domtreeAppendChild pid mbXML content = do
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
    _ -> do
      docWarn $ "not an insertable element: " ++ show parent
      return noElement

domtreeStartElement :: Monad m
                    => ElementID -> Element
                    -> DocumentT m ElementID
domtreeStartElement pid node0 = do
  nid <- takeNewID
  let node = node0{ elementParent = pid, elementContent = Right noElement }

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

  return nid

domtreeEndElement :: Monad m => DocumentT m ElementID
domtreeEndElement = do
  opened <- gets documentBuildState
  modify $ \doc -> doc{ documentBuildState = tail opened }
  return $ fromMaybe noElement $ mbHead opened


-- | Traversing document DOM tree
--domtreeTraverse :: (Traversable t, Applicative f) => (ElementRef -> f b) -> Document -> f (t b)
--domtreeTraverse = undefined

-- | ElementRef is useful for traversing the DOM without Document in context.
-- ElementRef should not live longer that the current version of the IntMap storage.
type ElementRef = (IM.IntMap Element, ElementID)

elementRef :: Document -> ElementID -> ElementRef
elementRef doc nid = (documentAllNodes doc, nid)

elementDeref :: ElementRef -> Element
elementDeref (nodes, nid) = nodes IM.! nid

elementRefID :: ElementRef -> ElementID
elementRefID (_, nid) = nid

elemrefAncestors :: ElementRef -> [ElementRef]
elemrefAncestors (_nodes, nid) | nid == noElement = []
elemrefAncestors (nodes, nid0) = go $ elementParent (nodes IM.! nid0)
  where
    go nid | nid == noElement = []
    go nid = (nodes, nid) : go (elementParent (nodes IM.! nid))

elemrefPrevious :: ElementRef -> Maybe ElementRef
elemrefPrevious (nodes, nid) =
  let node = nodes IM.! nid
      pid = elementParent node
      pnode = nodes IM.! pid
      fid = Ei.fromRight noElement $ elementContent pnode
      previd = prevSibling $ elementSiblings node
  in if nid == fid || nid == previd then Nothing else Just (nodes, previd)

{-
elemrefNext :: ElementRef -> Maybe ElementRef
elemrefNext (nodes, nid) =
  let node = nodes IM.! nid
      pid = elementParent node
      pnode = nodes IM.! pid
      fid = Ei.fromRight noElement $ elementContent pnode
      nextid = nextSibling $ elementSiblings node
  in if nid ==  || nid == previd then Nothing else Just (nodes, previd)
-}

elemrefChildren :: ElementRef -> [ElementRef]
elemrefChildren (nodes, nid) = maybe [] (nodeChildren . elementContent) $ IM.lookup nid nodes
  where
    nodeChildren (Right fid) | fid /= noElement = go fid fid
    nodeChildren _ = []
    go fid cid =
      let cnode = nodes IM.! cid
          next = nextSibling $ elementSiblings cnode
      in (nodes, cid) : if next == fid then [] else go fid next

elemrefDescendants :: ElementRef -> [ElementRef]
elemrefDescendants eltref =
  concatMap (\elt -> elt : elemrefDescendants elt) $ elemrefChildren eltref

querySelectorsAllRef :: [Selector] -> ElementRef -> [ElementRef]
querySelectorsAllRef sels parent = do
    mapMaybe checkNode $ elemrefDescendants parent
  where
    checkNode noderef =
      if any (isJust . domMatchSelector noderef) sels
      then Just noderef
      else Nothing

-- | This is a DOM node
type ElementID = Int

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

  , elementContent :: Either DOMContent ElementID
  -- ^ DOM content or the first child (may be noElement)
  , elementTag :: !Text
  , elementAttrs :: M.Map Text Text

  , elementStylesheet :: Stylesheet
  }
  deriving (Show)

emptyElement :: Element
emptyElement = Element
  { elementParent = noElement
  , elementSiblings = ElementSiblings{ prevSibling = noElement, nextSibling = noElement }
  , elementContent = Right noElement
  , elementTag = ""
  , elementAttrs = M.empty
  , elementStylesheet = emptyStylesheet
  }

elementAttrWords :: Text -> Element -> [Text]
elementAttrWords attr node =
  fromMaybe [] $ fmap T.words $ M.lookup attr (elementAttrs node)

elementTagAttrs :: Element -> TagAttrs
elementTagAttrs node = (elementTag node, elementAttrs node)

-- | Content of a leaf DOM node,
-- i.e. what is to be drawn in this node.
-- either a block or inline element, e.g. some text.
data DOMContent
  = TextContent !Text
  -- ^ A text node
  | NewlineContent
  -- ^ Force newline here, usually corresponds to <br>
  | ImageContent URI (Maybe (V2 Double))
  -- ^           ^href      ^ given dimensions
  | HorizLineContent
  -- ^ <hr>
  | InputTextContent !Text
  -- ^ <input type="text">
  deriving (Eq)

instance Show DOMContent where
  show (TextContent txt) = T.unpack $ T.concat ["TextContent \"", txt, "\""]
  show NewlineContent = "NewlineContent"
  show HorizLineContent = "HorizLineContent"
  show (ImageContent href wh) = "ImageContent " ++ show href ++ " (" ++ show wh ++ ")"
  show (InputTextContent t) = "InputTextContent \"" ++ T.unpack t ++ "\""


-- Map from a URI to the resource and the list of elements that use this resource
type DOMResourceMap = M.Map URI (HTTPResource, S.Set ElementID)

data HTTPResource
  = ImageResource !(V2 Double) SDL.Texture
  | CSSResource [CSSBlock]

instance Show HTTPResource where
  show (ImageResource dim _) = "ImageResource " ++ show dim
  show (CSSResource blocks) = "CSSResource " ++ show blocks

--------------------------------------------------------------------------------
-- | Normalize an XML tree of elements and text, possibly with
-- attributes like style and event handlers.

htmlDOMFromXML :: Monad m => XML.Node -> DocumentT m ()
htmlDOMFromXML = \case
  XML.NodeElement el -> do
    let attrs = M.mapKeys xmlTextName (XML.elementAttributes el)
    domStartHTMLElement (tagName el, attrs)
    forM_ (XML.elementNodes el) $ \child ->
      htmlDOMFromXML child
    domEndHTMLElement (tagName el)

  XML.NodeContent txt ->
    domAppendHTMLContent Nothing (TextContent txt)

  _ -> return ()

htmlDOMFromEvents :: Monad m => XMLTy.Event -> DocumentT m ()
htmlDOMFromEvents = \case
    XMLTy.EventBeginDoctype doctype mbExtID ->
      modify $ \doc -> doc{ documentXMLType = Just $ XMLTy.Doctype doctype mbExtID }
    XMLTy.EventContent content -> do
      domAppendHTMLContent Nothing (TextContent $ xmlContent content)
    XMLTy.EventBeginElement name attrs0 ->
      let tag = xmlTextName name
          attrs = M.fromList $ map xmlAttr attrs0
      in domStartHTMLElement (tag, attrs)
    XMLTy.EventEndElement name ->
      domEndHTMLElement (xmlTextName name)
    XMLTy.EventBeginDocument -> return ()
    XMLTy.EventEndDocument -> return ()    -- TODO: close all open tags
    XMLTy.EventEndDoctype -> return ()
    XMLTy.EventInstruction _ -> return ()
    XMLTy.EventComment _ -> return ()
    other -> docWarn $ "htmlDOMFromEvents: " ++ show other
  where
    xmlContent (XMLTy.ContentText t) = t
    xmlContent (XMLTy.ContentEntity t) = t

    xmlAttr (k, []) = (xmlTextName k, "")
    xmlAttr (k, vs) = (xmlTextName k, xmlContent $ last vs)

domStartHTMLElement :: Monad m => TagAttrs -> DocumentT m ()
domStartHTMLElement (tag, attrs) = do
  -- DOM fixing:
  mbPid <- mbHead <$> gets documentBuildState
  mbParent <- mapM getElement mbPid
  case (tag, fmap elementTag mbParent) of
    ("li", Just "li") -> domEndHTMLElement "li"
    (_, Just "p") | tag `elem` htmlAutoclosePBefore -> domEndHTMLElement "p"
    _ -> return ()

  opened <- gets documentBuildState     -- do not move before the fixing
  let pid = if null opened then noElement else head opened

  -- insert
  let node = emptyElement { elementTag = tag, elementAttrs = attrs }
  nid <- domtreeStartElement pid node
  modify $ \doc -> doc{ documentBuildState = (nid:opened) }

  domParseHTMLAttributes nid

domAppendHTMLContent :: Monad m => Maybe TagAttrs -> DOMContent -> DocumentT m ()
domAppendHTMLContent mbTagAttrs content = do
  opened <- gets documentBuildState
  when (L.null opened) $ error "domAppendHTMLContent: no opened elements"
  let pid = head opened
  nid <- domtreeAppendChild pid mbTagAttrs content
  domParseHTMLAttributes nid

domEndHTMLElement :: Monad m => Text -> DocumentT m ()
domEndHTMLElement name = do
  opened <- gets documentBuildState
  mbNode <- mapM getElement $ mbHead opened
  whenJust (liftA2 (,) (mbHead opened) mbNode) $ \(nid, node) -> do
    let attrs = elementAttrs node

    -- hr, br and such are not containers, change their content if needed:
    let fixupContent nid' content = modifyElement nid' $ \node' ->
          if elementContent node' == Right noElement
          then node' { elementContent = Left content }
          else error $ "fixupContent: expected an empty container in " ++ show node'

    case elementTag node of
      "title" -> do
        let Right fid = elementContent node
        tnode <- getElement fid
        case elementContent tnode of
          Left (TextContent title) | nextSibling (elementSiblings tnode) == fid ->
            modify $ \doc -> doc{ documentTitle = title }
          other ->
            docWarn ("domEndHTMLElement @"++show nid++
                       ": expected text in <title>, found " ++ show other)
      "hr" -> fixupContent nid HorizLineContent
      "br" -> fixupContent nid NewlineContent

      "input" ->
        let value = fromMaybe "" $ M.lookup "value" attrs
        in fixupContent nid $ case M.lookup "type" attrs  of
            Nothing -> InputTextContent value
            Just "text" -> InputTextContent value
            Just "password" -> InputTextContent value    -- TODO: a password input
            Just "button" -> TextContent value
            Just "checkbox" -> TextContent $ maybe "\x2610" (\_ -> "\x2611") $ M.lookup "checked" attrs
            Just "submit" -> TextContent $ fromMaybe "submit" $ M.lookup "value" attrs
            Just "hidden" -> TextContent ""
            inpty -> warning ("Unknown input type: " ++ show inpty) $ TextContent ""

      "style" -> do
        let Right fid = elementContent node
        if fid == noElement then return ()  -- empty style
        else do
          tnode <- getElement fid
          case elementContent tnode of
            Left (TextContent styletext) -> do
              let rules = cssParser styletext
              modify $ \doc ->
                warning ("Parsed "++show (length rules)++" CSS rules from @"++show nid) $
                  doc{ documentCSSRules = addCSSRules rules $ documentCSSRules doc }
              -- TODO: make "@import" requests.
              -- TODO: re-layout
            other ->
              docWarn $ "domEndHTMLElement @"++show nid++
                        ": expected text in <style>, found " ++ show other

      "link" | Just "stylesheet" == M.lookup "rel" attrs ->
        case fromMaybe "text/css" $ M.lookup "type" attrs of
          "text/css" ->
            case M.lookup "href" attrs of
              Nothing -> docWarn $ "no href in <link rel=stylesheet>" ++ show node
              Just href -> do
                case URI.parseURIReference (T.unpack href) of
                  Nothing -> docWarn $ "failed to parse link href=" ++ T.unpack href
                  Just u -> do
                    docuri <- gets documentLocation
                    let resuri = u `URI.relativeTo` docuri
                    documentEnqueue $ DocResourceRequest nid resuri
          _ -> docWarn $ "unknown type in " ++ show node

      "img" -> do
        -- TODO: fix separate TextContent/ImageContent for displaying `alt`
        let alt = TextContent $ fromMaybe "<img>" $ M.lookup "alt" attrs
        let wh = liftA2 V2 (decodeAttr "width" attrs) (decodeAttr "height" attrs)
        docuri <- gets documentLocation
        fixupContent nid $ case M.lookup "src" attrs of
          Just href | "data:" `T.isPrefixOf` href ->
            -- A hack: pass "blob:data@12" to domResourceFetch
            -- and let handlePageStreaming in `Browser` to actually decode it.
            let Just bloburi = URI.parseURI ("blob:data@" ++ show nid)
            in ImageContent bloburi wh
          Just href ->
            case URI.parseURIReference (T.unpack href) of
              Just u -> let resuri = u `URI.relativeTo` docuri in ImageContent resuri wh
              Nothing -> warning ("failed to parse img src=" ++ T.unpack href) alt
          _ -> alt

        modify $ \doc -> doc{ documentImages = intmapAppend nid (documentImages doc) }

        node' <- getElement nid
        case elementContent node' of
          Left (ImageContent resuri _) -> domResourceFetch nid resuri
          _ -> return ()

      _ -> return ()

  case (name, elementTag <$> mbNode) of
    ("li", Just tag) | tag `elem` ["ul", "ol"] -> return ()
    ("p", Just tag) | tag /= "p" -> return ()
    (_, Just tag) | tag /= name ->
      error $ "domEndHTMLElement: "++ show tag ++" /= " ++ show name++", opened=" ++ show opened
    _ ->
      void domtreeEndElement

domParseHTMLAttributes :: Monad m => ElementID -> DocumentT m ()
domParseHTMLAttributes nid = do
  node <- getElement nid
  let tag = elementTag node
  let attrs = elementAttrs node
  -- This can't be moved to domEndHTMLElement: children of <body> might need to know about it:
  case elementTag node of
    "head" -> modify $ \doc ->
        if documentHead doc == noElement
        then doc{ documentHead = nid }
        else warning "domParseHTMLAttributes: more than one <head>" doc
    "body" -> modify $ \doc ->
        if documentBody doc == noElement
        then doc{ documentBody = nid, documentFocus = nid }
        else warning "domParseHTMLAttributes: more than one <body>" doc
    _ -> return ()
  -- TODO: parse class set
  -- TODO: parse id and add globally
  let htmlStyle = fromMaybe noStyle $ builtinHTMLStyleFor tag attrs

  let mbStyleAttr = (\t -> mconcat ["*{", t, "}"]) <$> M.lookup "style" attrs
  let attrStyle = case cssParser <$> mbStyleAttr of
        Nothing -> (noStyle, noStyle)
        Just [] ->
          let msg ="domParseHTMLAttributes@"++show nid++": failed to parse: "++show mbStyleAttr
          in warning msg (noStyle, noStyle)
        Just blocks ->
          let styleFromBlocks = maybe noStyle blockStyle . listToMaybe
              (impblocks, regblocks) = L.partition blockImportant blocks
          in (styleFromBlocks impblocks, styleFromBlocks regblocks)

  rules <- gets documentCSSRules
  eltref <- getElementRef nid
  let (impchain, regchain) = rulechainsForMatcher rules (tag, attrs) $ domMatchSelector eltref
  let cssStyle = (styleFromRulechain rules impchain, styleFromRulechain rules regchain)
  let sheet = Stylesheet
        { stylesheetUserBrowser = (noStyle, htmlStyle)   -- TODO: OriginImportantUser
        , stylesheetAttr = attrStyle
        , stylesheetAuthor = cssStyle
        , stylesheetAuthorChain = (impchain, regchain)
        , stylesheetComputed = undefined
        }
  setElement nid $ node
    { elementStylesheet = sheet{ stylesheetComputed = computedStyle sheet }
    }

  whenJust (M.lookup "autofocus" attrs) $ \_ ->
    modify $ \doc -> doc{ documentFocus = nid }

-- | Having an effect inside DocumentT m (): just queue them for the wrapper to handle.
data DocumentEvent
  = DocResourceRequest ElementID URI
  | DocEmitEvent ElementID EventName
  deriving (Show)

drainDocumentEvents :: Monad m => DocumentT m [DocumentEvent]
drainDocumentEvents = do
  resReqs <- gets (L.reverse . documentEvents)
  modify $ \doc -> doc{ documentEvents = [] }
  return resReqs

documentEnqueue :: Monad m => DocumentEvent -> DocumentT m ()
documentEnqueue event = modify $ \doc ->
  doc{ documentEvents = event : documentEvents doc }

domResourceFetch :: Monad m => ElementID -> URI -> DocumentT m ()
domResourceFetch nid href = do
  docuri <- gets documentLocation
  documentEnqueue $ DocResourceRequest nid (href `URI.relativeTo` docuri)

-- | Request a redraw for a node
documentRedraw :: Monad m => ElementID -> DocumentT m ()
documentRedraw nid = docWarn $ "TODO: documentRedraw @"++show nid

-- HTML 5 misc

htmlAutoclosePBefore :: [TagName]
htmlAutoclosePBefore =
 [ "address", "article", "aside", "blockquote", "details", "div", "dl"
 , "fieldset", "figure", "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6"
 , "header", "hgroup", "hr", "main", "menu", "nav", "ol"
 , "p", "pre", "section", "table", "ul" ]

-- | if element is focusable, return `Just tabindex` (the attribute value) or `Just 0`.
-- If not focusable, Nothing.
elementTabIndex :: Element -> Maybe Int
elementTabIndex node =
  let tag = elementTag node
      attrs = elementAttrs node
  in case ("disabled" `M.member` attrs, "tabindex" `M.lookup` attrs) of
    (_, mbIdx) | tag == "a" ->
      maybe (Just 0) mbReadText mbIdx
    (False, mbIdx) | tag `elem` ["area", "button", "input", "textarea", "select"] ->
      maybe (Just 0) mbReadText mbIdx
    (_, Just idx) ->
      mbReadText idx
    _ ->
      Nothing

--------------------------------------------------------------------------------
-- XML Utilities

decodeAttr :: Read a => Text -> (M.Map Text Text) -> Maybe a
decodeAttr attr attrs = mbReadText =<< M.lookup attr attrs

tagName :: XML.Element -> Text
tagName = xmlTextName . XML.elementName

xmlTextName :: XML.Name -> Text
xmlTextName name =
  let n = XML.nameLocalName name
  in T.toLower $ case XML.nameNamespace name of
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

{-
xmlEvents :: XML.Document -> [XML.Event]
xmlEvents doc = [XML.EventBeginDocument] ++ prologue ++ root ++ epilogue ++ [XML.EventEndDocument]
  where
    emitMisc (XML.MiscInstruction instr) =
      XML.EventInstruction instr
    emitDoctype XML.Doctype{XML.doctypeName=name, XML.doctypeID=mbExtID} =
      [XML.EventBeginDoctype name mbExtID, XML.EventEndDoctype]
    emitElement (XML.Element el) = undefined -- [XML.EventBeginElement bbbb

    emitNode = \case
      XML.NodeElement el -> emitElement el
      XML.NodeInstruction instr -> [XML.EventInstruction instr]
      XML.NodeContent content -> [XML.Content content]
      XML.NodeComment comment -> [XML.EventComment comment]

    prologue =
      let pro = XML.documentPrologue doc
      in map emitMisc (XML.prologueBefore pro)
          ++ maybe [] emitDoctype (XML.prologueDoctype)
          ++ map emitMisc (XML.prologueAfter pro)
    rootelem = XML.documentRoot doc
    root = emitNode (XML.NodeElement rootelem)
    epilogue = map emitMisc $ XML.documentEpilogue doc
-}

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

  page0 <- layoutPage $ vadoPage vadoWait $ Page
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
  renderDOM page0

  let address = fromMaybe "vado:home" $ listToMaybe args
  let url = fromMaybe (webSearch address) $ URI.parseURI address
  flip St.evalStateT page0 $ runBrowserT $ do
    changePage $ \page -> Just <$> navigatePage url page
    asyncEventLoop

webSearch :: String -> URI
webSearch s = fromJust $ URI.parseAbsoluteURI $ "https://google.com/search?q=" ++ es
  where es = URI.escapeURIString URI.isAllowedInURI s

navigate :: Maybe Resource.Content -> URI -> Page -> IO Page
navigate mbSend uri page =
  if uriScheme uri == "vado:" then
    case uriPath uri of
      "home" -> do
        -- TODO: cancel all pending resource requests; clear resources
        page1 <- layoutPage $ vadoPage vadoHome page
        Just nid <- inPageDocument page1 $ getElementById "vado"
        return (addEventListener nid "keyup" vadoHome_onKeyReleased page1)
          { pageHistory = historyRewind (pageHistory page1) (pageUrl page1) uri
          }
      _ ->
        error $ "navigate: unknown address " ++ show uri
  else do
    let resman = pageResMan page
    let fullURI = uri `URI.relativeTo` (documentLocation $ pageDocument page)
    -- TODO: check if fullURI is canonicalized:
    liftIO $ do
      when (pageDebugNetwork page) $ putStrLn $ "navigatePage " ++ show fullURI
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

-- | Page event loop.
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
      --Resource.EventStreamChunk _ -> return ()
      _ -> logDebug $ "[" ++ show st ++ "] " ++ show (u, event)

    if u == uri then
      handlePageStreaming st event
    else if u `M.member` loading then
      handleResourceEvent st u event
    else
      logWarn $ "eventloop: unknown resource " ++ show u ++ " for event " ++ show event

  -- DOM-emitted events
  resReqs <- withBrowserDocument $ drainDocumentEvents
  forM_ resReqs $ handleDOMEvent

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
        let win' = win{ windowTexture=texture', windowViewport=(fromIntegral <$> size) }
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
                runBrowserDocument $ modify $ \doc -> doc{ documentFocus = nid }
                liftIO $ putStrLn $ "MouseButtonEvent: focused @" ++ show nid
                -- TODO: emit "focus" event for nid
              liftIO $ putStrLn $ "MouseButtonEvent: dispatchEvent @" ++ show nid
              changePage $ \page -> Just <$> dispatchEvent (Right event) nid page

      SDL.KeyboardEvent _ -> return ()
      SDL.MouseMotionEvent _ -> return ()
      _ -> return () --liftIO $ print event

handlePageStreaming :: PageState -> Resource.EventKind -> Browser ()
handlePageStreaming st event = do
  case (st, event) of
    (_, Resource.EventConnectionError exc) | st `elem` [PageConnecting, PageStreaming] ->
      changePage $ \page -> do
        when (pageDebugNetwork page) $ print exc
        let doc = vadoError $ T.pack $ show exc
        return $ Just $ vadoPage doc page

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
      runBrowserDocument $ htmlDOMFromEvents update

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

handleDOMEvent :: DocumentEvent -> Browser ()
handleDOMEvent (DocResourceRequest nid resuri) = do
    resman <- gets pageResMan
    if URI.uriScheme resuri /= "blob:" then do
      logDebug $ "eventloop: @" ++ show nid ++ " requests " ++ show resuri
      runBrowserDocument $ documentMakeRequest resman
    else do
      -- assumption: the only source of blobs are <img src="data:...">
      node <- inBrowserDocument $ getElement nid
      case M.lookup "src" $ elementAttrs node of
        Just dataurl -> decodeResourceDataUrl dataurl
        _ -> logWarn $ "no src in @" ++ show nid
  where
    documentMakeRequest :: Resource.Manager -> DocumentT IO ()
    documentMakeRequest resman = do
      resources <- gets documentResources
      wereLoading <- gets documentResourcesLoading
      unless (resuri `M.member` wereLoading || resuri `M.member` resources) $
        liftIO $ Resource.requestGet resman resuri []

      let multimapAdd v = M.alter (\vs -> Just (v `S.insert` fromMaybe S.empty vs))
      let nowLoading = multimapAdd nid resuri wereLoading
      modify $ \doc -> doc{ documentResourcesLoading = nowLoading }

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
              runBrowserDocument $ modify $ \doc -> do
                let resources0 = documentResources doc
                let resources = M.insert resuri (res, S.singleton nid) resources0
                doc{ documentResources = resources }

handleDOMEvent (DocEmitEvent nid evt) = do
  modifyPage (\p -> Just <$> dispatchEvent (Left evt) nid p)

--handleDOMEvent other = logWarn $ "TODO: handleDOMEvent " ++ show other

handleResourceEvent :: PageState -> URI -> Resource.EventKind -> Browser ()
handleResourceEvent st uri event =
  case (st, event) of
    (_, Resource.EventContentReady (_req, resp) content) -> do
      wereLoading <- gets (documentResourcesLoading . pageDocument)
      let nodes = wereLoading M.! uri
      runBrowserDocument $ do
        loading <- gets (M.delete uri . documentResourcesLoading)
        when (M.null loading) $ liftIO $ putStrLn $ "All resources are ready"
        modify $ \doc -> doc{ documentResourcesLoading = loading }

      let contentType = Resource.httpResponseContentType resp
      logDebug $ "OK: "++show uri++" ("++T.unpack contentType++") loaded: " ++
                 show (HTTP.responseStatus resp)

      mbRes <- decodeResourceContent contentType content
      case mbRes of
        Left oops -> logWarn $ "ERROR: <"++show uri++">: " ++ oops

        Right (CSSResource blocks) -> do
          logWarn $ "Parsed "++show (length blocks)++" CSS rules from "++show uri
          runBrowserDocument $ modify $ \doc ->
            doc{ documentCSSRules = addCSSRules blocks $ documentCSSRules doc }

          -- re-match the rules in the document
          allNodes <- fmap IM.toList $ inBrowserDocument $ gets documentAllNodes
          rules <- inBrowserDocument $ gets documentCSSRules
          changed <- fmap concat $ forM allNodes $ \(nid, node) -> do
              eltref <- inBrowserDocument $ getElementRef nid
              let tagattrs = (elementTag node, elementAttrs node)
              let newchains = rulechainsForMatcher rules tagattrs $ domMatchSelector eltref

              let sheet0 = elementStylesheet node
              if newchains == stylesheetAuthorChain sheet0 then return []
              else do
                let (impchain, regchain) = newchains
                let style = (styleFromRulechain rules impchain, styleFromRulechain rules regchain)
                let sheet = sheet0{ stylesheetAuthor = style }
                runBrowserDocument $ setElement nid $
                  node{ elementStylesheet = sheet{ stylesheetComputed = computedStyle sheet } }
                return [nid]

          when (not $ L.null changed) $ do
            logDebug $ "Relayout needed for nodes: " ++ show changed
            changePage $ \p -> Just <$> layoutPage p

        Right res@(ImageResource _ _) -> do
          runBrowserDocument $ modify $ \doc ->
            doc{ documentResources = M.insert uri (res, nodes) (documentResources doc) }

          -- TODO: replace with re-layout requests when incremental layout is ready.
          -- Are there <img>s without specified width= and height= that need relayout?
          needsLayout <- inBrowserDocument $ do
            doc <- get
            let maybeWH nid =
                  let attrs = elementAttrs $ elementDeref $ elementRef doc nid
                      mbW = decodeAttr "width" attrs :: Maybe Double
                      mbH = decodeAttr "height" attrs :: Maybe Double
                  in liftA2 V2 mbW mbH
            return $ any isNothing $ map maybeWH $ S.toList nodes
          if needsLayout then do
            logDebug $ "Relayout caused by " ++ show uri
            changePage $ \p -> Just <$> layoutPage p
          else withPage renderDOM

    (_, Resource.EventConnectionError exc) | st /= PageConnecting -> do
      logDebug $ "ERROR: "++show uri++" : " ++ show exc
      runBrowserDocument $ do
        loading <- gets (M.delete uri . documentResourcesLoading)
        when (M.null loading) $ liftIO $ putStrLn $ "All resources are ready"
        modify $ \doc -> doc{ documentResourcesLoading = loading }

    _ ->
      logWarn $ "eventloop: invalid state " ++ show st ++ " for event " ++ show event

decodeResourceContent :: Text -> Resource.Content -> Browser (Either String HTTPResource)
decodeResourceContent contentType = \case
  Resource.ContentBytes bs | T.isPrefixOf "image/" contentType ->
    if isJust (Image.format bs) then do
      renderer <- gets (windowRenderer . pageWindow)
      (texture, wh) <- liftIO $ do
        bitmap <- Image.decode bs
        texture <- SDL.createTextureFromSurface renderer bitmap
        V2 w h <- SDL.surfaceDimensions bitmap
        return (texture, V2 (fromIntegral w) (fromIntegral h))
      return $ Right (ImageResource wh texture)
    else
      return $ Left ("decodeResourceContent: an unknown image format " ++ T.unpack contentType)

  Resource.ContentBytes bs | T.isPrefixOf "text/css" contentType -> do
    -- TODO: handle non-utf8 encodings properly
    let txt = Ei.fromRight (TE.decodeLatin1 bs) $ TE.decodeUtf8' bs
    let rules = cssParser txt
    return $ Right (CSSResource rules)

  _content ->
    return $ Left ("decodeResourceContent: unknown Content-Type=" ++ T.unpack contentType)

--------------------------------------------------------------------------------
-- UI: rendering and events

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
      { SDL.windowHighDPI = True
      , SDL.windowResizable = True
      , SDL.windowInitialSize = defaultWindowSize
      }
  -- Setup Cairo rendering on the window.
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  Just (SDL.Rectangle _ viewport@(V2 w h)) <-
    SDL.get (SDL.rendererViewport renderer)
  texture0 <- Cairo.createCairoTexture renderer viewport
  return $ VadoWindow
    { windowRenderer = renderer
    , windowViewport = V2 (fromIntegral w) (fromIntegral h)
    , windowTexture = texture0
    , windowScroll = 0
    }

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
-- Events, event handlers, event dispatch.

type EventName = Text

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

eventHandlerWithPage :: (Page -> IO Page) -> StateT EventContext IO ()
eventHandlerWithPage f = do
  page <- gets evctxPage
  page' <- liftIO $ f page
  modify $ \ctx -> ctx{ evctxPage = page' }


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

--
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

--- Text input events

modify_input :: (Text -> Text) -> Element -> Element
modify_input f node =
  case elementContent node of
    Left (InputTextContent txt) -> node{ elementContent = Left $ InputTextContent $ f txt }
    _ -> warning ("an input event not on a text input: " ++ show node) node

input_onKeyReleased :: EventHandler
input_onKeyReleased event nid =
  eventHandlerWithPage $ \page -> runPageDocument page $ do
    -- TODO: it's not always a text input
    let Right evsdl = event
    let KeyboardEvent e = SDL.eventPayload evsdl
    let keysym = SDL.keyboardEventKeysym e
    case SDL.keysymKeycode keysym of
      SDL.KeycodeBackspace ->
        modifyElement nid $ modify_input (\case "" -> ""; t -> T.init t)
      SDL.KeycodeReturn ->
        documentEnqueue $ DocEmitEvent nid "change"
      -- TODO: Mac (GUI) keyboard layout support:
      SDL.KeycodeV | isKeyCtrlPressed (SDL.keysymModifier keysym) -> do
        clipboard <- lift $ SDL.getClipboardText
        modifyElement nid $ modify_input (const clipboard)
      _ -> return ()
      --  lift $ putStrLn $ "input_onKeyReleased $ " ++ show (SDL.keysymKeycode keysym)
    documentRedraw nid

input_onTextInput :: EventHandler
input_onTextInput event nid = do
  eventHandlerWithPage $ \page ->
    runPageDocument page $ do
      let Right evsdl = event
      let TextInputEvent e = SDL.eventPayload evsdl
      let input = SDL.textInputEventText e
      -- lift $ putStrLn $ "input_onTextInput: " ++ T.unpack input
      modifyElement nid $ modify_input (`T.append` input)
      documentRedraw nid

body_onKeyReleased :: EventHandler
body_onKeyReleased event _nid = do
  page <- gets evctxPage
  let Right evsdl = event
  let SDL.KeyboardEvent e = SDL.eventPayload evsdl
  let modifiers = SDL.keysymModifier $ SDL.keyboardEventKeysym e
  --putStrLn $ "body_onKeyReleased: " ++ show e
  page' <- liftIO $ case SDL.keysymKeycode $ SDL.keyboardEventKeysym e of
    SDL.KeycodeE | isKeyCtrlShiftPressed modifiers -> do
      let debug = not $ pageDebugNetwork page
      putStrLn $ "Setting pageDebugNetwork to " ++ show debug
      return page{ pageDebugNetwork = debug }
    SDL.KeycodeEnd ->
      let pageH = maybe 0 boxHeight $ pageBoxes page
      in return $ vadoScroll pageH page
    SDL.KeycodeH | isKeyCtrlShiftPressed modifiers -> do
      printHistory (pageHistory page) (pageUrl page) -- show History
      return page
    SDL.KeycodeHome | isKeyAltPressed modifiers ->
      navigatePage (fromJust $ URI.parseURI "vado:home") page
    SDL.KeycodeHome ->
      return $ vadoScroll (negate $ pageScroll page) page
    SDL.KeycodeI | isKeyCtrlShiftPressed modifiers -> do
      putStrLn (showdbg $ pageDocument page)   -- Inspect the DOM tree:
      return page
    SDL.KeycodeK | isKeyCtrlShiftPressed modifiers ->
      -- Inspect rendering boxes:
      print (pageBoxes page) >> return page
    SDL.KeycodeLeft | isKeyAltPressed modifiers ->
      case pageHistory page of
        (prevurl:_, _) -> navigatePage prevurl page
        _ -> return page
    SDL.KeycodePageDown -> do
      return $ vadoScroll (vadoViewHeight page) page
    SDL.KeycodePageUp -> do
      return $ vadoScroll (negate $ vadoViewHeight page) page
    SDL.KeycodeQ | isKeyCtrlPressed modifiers ->
      SDL.quit >> liftIO exitSuccess >> return page
    SDL.KeycodeR | isKeyCtrlPressed modifiers ->
      navigatePage (pageUrl page) page
    SDL.KeycodeRight | isKeyAltPressed modifiers ->
      case pageHistory page of
        (_, nexturl:_) -> navigatePage nexturl page
        _ -> return page
    _ ->
      return page
  modify $ \ctx -> ctx{ evctxPage = page' }

form_onSubmit :: EventHandler
form_onSubmit event formID = do
    logWarn $ "form_onSubmit: @"++show formID++" event="++show event
    page <- gets evctxPage

    -- gather the parameters:
    formref <- inPageDocument page $ getElementRef formID
    let controls = querySelectorsAllRef formControlSelectors formref
    let values = mapMaybe (nodeFormValue . elementDeref) controls

    let attrs = elementAttrs $ elementDeref formref
    let formAction = fromMaybe undefined $ M.lookup "action" attrs -- TODO: default to docuri
    case URI.parseURIReference $ T.unpack formAction of
      Nothing ->
        logWarn $ "form_onSubmit: cannot parse action="++T.unpack formAction
      Just formURI -> do
        page' <- liftIO $ case maybe "get" T.toLower $ M.lookup "method" attrs of
          "get" -> do
            let values' = map (\(k, v) -> (k, listToMaybe v)) values
            let bsQuery = URIh.renderQuery True $ URIh.queryTextToQuery values'
            let queryURI = formURI{ uriQuery = Bc.unpack bsQuery }
            logWarn $ "form_onSubmit: method=GET action="++show queryURI
            navigatePage queryURI page
          "post" -> do
            -- TODO: check enctype
            let fields = M.fromListWith mappend values
            logWarn $ "form_onSubmit: method=POST action="++show formURI++" form="++show fields
            liftIO $ navigate (Just $ Resource.ContentForm fields) formURI page
          other ->
            return $ warning ("form_onSubmit: invalid method="++T.unpack other) page

        put EventContext
          { evctxPage = page'
          , evctxStopPropagation = True
          , evctxPreventDefault = True
          }
  where
    formControlSelectors = [SelTag "input", SelTag "select", SelTag "textarea"]

    nodeFormValue :: Element -> Maybe (Text, [Text])
    nodeFormValue node | elementTag node == "input" =
      let attrs = elementAttrs node
      in case M.lookup "name" attrs <|> M.lookup "id" attrs of    -- TODO: check if can use "id"
          Nothing -> warning ("Form control without name"++show node) Nothing
          Just name ->
            case M.lookup "type" attrs of
              Just ty | ty `elem` ["hidden", "submit"] ->
                Just (name, maybeToList $ M.lookup "value" attrs)
              _ ->
                case elementContent node of
                  Left (InputTextContent value) -> Just (name, [value])
                  other -> warning ("Unexpected content in <input>: "++ show other) $ Just (name, [])
    nodeFormValue node =
      warning ("form_onSubmit: cannot extract value from "++show node) Nothing


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

-- | Entry point for layout procedure
layoutPage :: Page -> IO Page
layoutPage page@Page{..} | documentBody pageDocument == noElement =
  return $ warning ("layoutPage: no body") page
layoutPage page = do
    let doc = pageDocument page
    let ctx = LayoutCtx { ltResources = documentResources doc }
    let VadoWindow { windowViewport = V2 w _, windowTexture = texture } = pageWindow page
    let params = LayoutParams { ltWidth = w }
    let body = elementRef doc (documentBody doc)
    (boxes, _ctx) <- Canvas.withCanvas texture $ elementToBoxes ctx params noStyle body
    return page{ pageBoxes=Just boxes }


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
    let st = (elementStylesheet $ elementDeref node) `cascadeStyle` parentStyle
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

--------------------------------------------------------------------------------
-- Rendering engine

renderDOM :: Page -> IO ()
renderDOM Page{ pageBoxes = Nothing } =
  logWarn "renderDOM: page is not layed out yet"
renderDOM page@Page{ pageBoxes = Just body, pageDocument = doc, pageWindow = win } = do
  let minY = pageScroll page
  let (texture, renderer) = (windowTexture win, windowRenderer win)

  let (stpush, _) = boxStyling body
  let backgroundColor = case M.lookup CSSBackgroundColor stpush of
        Just (CSS_RGB r g b) -> Canvas.rgb r g b
        _ -> Canvas.rgb 255 255 255

  replaced <- Canvas.withCanvas texture $ do
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

          _ | href `M.member` documentResourcesLoading doc ->
            return ()     -- Still loading
          _ ->
            logWarn $ "renderDOM: no resource for <img src=" ++ show href ++ ">"
      other ->
        logWarn $ "renderDOM: unexpected replaced element: " ++ show other

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
      when (any (`M.member` diff) [CSSFontFamily, CSSFontSize, CSSFontStyle, CSSFontWeight]) $
        Canvas.textFont $ styleFont st
      forM_ (M.toList diff) $ \case
        (CSSColor, CSS_RGB r g b) ->
          Canvas.stroke $ Canvas.rgb r g b
        (CSSBackgroundColor, _) ->
          return ()
        other ->
          return $ warning ("stpush property ignored: " ++ show other) ()

    applyBackgroundColor st (CSS_RGB r g b) = do
      Canvas.stroke $ Canvas.rgb r g b
      Canvas.fill $ Canvas.rgb r g b
      Canvas.rect $ Canvas.D x y w h
      case st `cssValueMaybe` CSSColor of
        Just (CSS_RGB r' g' b') -> Canvas.stroke $ Canvas.rgb r' g' b'
        Just other -> logWarn ("unknown stroke color: " ++ show other)
        Nothing -> logWarn "no stroke color set, weird"
    applyBackgroundColor _ other =
      logWarn ("Unknown background color: " ++ show other)



--------------------------------------------------------------------------------
-- DOM and CSS

-- | domMatchSelector checks if an element matches a selector.
-- If yes, returns Just Specificity, else Nothing
domMatchSelector :: ElementRef -> Selector -> Maybe Specificity
domMatchSelector elt sel =
  let node = elementDeref elt
      attrs = elementAttrs node
  in case sel of
    SelAny -> Just (0, 0, 0)
    SelTag t | t == elementTag node -> Just (0, 0, 1)
    SelClass c | c `L.elem` elementAttrWords "class" node -> Just (0, 1, 0)

    SelHasAttr a | a `M.member` elementAttrs node -> Just (0, 1, 0)
    SelAttrEq a s | Just s == M.lookup a attrs -> Just (0, 1, 0)
    SelAttrWord a w | w `L.elem` elementAttrWords a node -> Just (0, 1, 0)
    SelAttrPre a p | Just p == M.lookup a attrs -> Just (0, 1, 0)
    SelAttrPre a p | maybe False (T.isPrefixOf (p <> "-")) (M.lookup a attrs) -> Just (0, 1, 0)
    SelId id_ | Just id_ == M.lookup "id" attrs -> Just (1, 0, 0)

    SelSiblings s1 s2 ->
      let match1 = domMatchSelector elt s1
          match2 = elemrefPrevious elt >>= \predelt -> domMatchSelector predelt s2
      in liftA2 addSpecty match1 match2
    SelChild s1 s2 ->
      let match1 = domMatchSelector elt s1
          match2 = mbHead (elemrefAncestors elt) >>= \parent -> domMatchSelector parent s2
      in liftA2 addSpecty match1 match2
    SelDescends s1 s2 ->
      let match1 = domMatchSelector elt s1
          match2 = listToMaybe $ mapMaybe (\el -> domMatchSelector el s2) $ elemrefAncestors elt
      in liftA2 addSpecty match1 match2

    SelAnd sels ->
      let specties = map (domMatchSelector elt) sels
      in if all isJust specties
        then Just $ foldl addSpecty (0, 0, 0) $ catMaybes specties
        else Nothing

    SelPseudoElem _ -> Nothing  -- TODO
    SelPseudo _ -> Nothing    -- TODO
    _ -> Nothing
  where
    addSpecty (a1, b1, c1) (a2, b2, c2) = (a1+a2, b1+b2, c1+c2)


--------------------------------------------------------------------------------
-- | Default CSS values and getters

-- TODO: sensible choice depending on the OS
defaultFontFace :: String
defaultFontFace = "Noto Serif"

defaultFontFaceSans :: String
defaultFontFaceSans = "Noto Sans"

defaultFontFaceMono :: String
defaultFontFaceMono = "Noto Mono"

defaultFontSize :: Double
defaultFontSize = 18

styleFont :: Style -> Canvas.Font
styleFont st = Canvas.Font face size weight italic
  where
    face =
      case st `cssValueMaybe` CSSFontFamily of
        Just (CSS_String name) -> T.unpack name
        Just (CSS_Keyword name) -> T.unpack name
        Just other -> error $ "styleFont: unknown font-family=" ++ show other
        Nothing -> defaultFontFace
    size =
      case st `cssValueMaybe` CSSFontSize of
        Just (CSS_Px px) -> px
        --Just other -> warning ("styleFont: unknown font-size=" ++ show other) defaultFontSize
        _ -> defaultFontSize
    weight = (st `cssValueMaybe` CSSFontWeight == Just (CSS_Keyword "bold"))
    italic = (st `cssValueMaybe` CSSFontStyle == Just (CSS_Keyword "italic"))

stylePreservesNewlines :: Style -> Bool
stylePreservesNewlines st =
  case st `cssValueMaybe` CSSWhiteSpace of
    Just (CSS_Keyword "pre") -> True
    _ -> False

--------------------------------------------------------------------------------
-- | Built-in styles

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


uiStyle :: Style
uiStyle = css
  [ ("background-color", "#f8f8f8")
  , ("color", "black")
  , ("text-decoration", "none")
  , ("white-space", "pre")
  ] `overriding` uiFont

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
     [ ("h1",       css [display_block, ("font-size", "2.00em"), fontweight_bold])-- TODO: use rem?
     , ("h2",       css [display_block, ("font-size", "1.50em"), fontweight_bold])
     , ("h3",       css [display_block, ("font-size", "1.17em"), fontweight_bold])
     , ("h4",       css [display_block, ("font-size", "1.00em"), fontweight_bold])
     , ("h5",       css [display_block, ("font-size", "0.83em"), fontweight_bold])
     , ("h6",       css [display_block, ("font-size", "0.75em"), fontweight_bold])
     ] ++
     [ ("a",        css [color "#00e", display_inline, ("text-decoration", "underline")])
     , ("abbr",     inline)
     , ("acronym",  inline)
     , ("address",  css [display_block])
     , ("article",  css [display_block])
     , ("aside",    css [display_block])
     , ("b",        css [fontweight_bold, display_inline])
     , ("bdo",      inline)
     , ("big",      css [("font-size", "117%"), display_inline])
     , ("blockquote", css [display_block, ("margin", "40px 15px")])
     , ("body",     bodyStyle)
     , ("button",   buttonStyle)
     , ("center",   css [("text-align", "center")])
     , ("cite",     css [fontstyle_italic, display_inline])
     , ("code",     css [fontfamily defaultFontFaceMono, display_inline])
     , ("dd",       css [display_block])
     , ("detauls",  css [display_block])
     , ("dfn",      inline)
     , ("dialog",   css [display_block])
     , ("div",      css [display_block])
     , ("dt",       css [display_block])
     , ("em",       css [fontstyle_italic, display_inline])
     , ("fieldset", css [display_block])
     , ("figcaption", css [display_block])
     , ("figure",   css [display_block])
     , ("footer",   css [display_block])
     , ("form",     css [display_block])
     , ("header",   css [display_block])
     , ("hgroup",   css [display_block])
     , ("hr",       css [display_block])
     , ("i",        css [fontstyle_italic, display_inline])
     , ("img",      inline)
     , ("input",    textinputStyle)   -- TODO: different types
     , ("kbd",      inline)
     , ("label",    inline)
     , ("li",       css [("display", "list-item")])
     , ("main",     css [display_block])
     , ("map",      inline)
     , ("mark",     css [("background-color", "yellow"), display_inline])
     , ("nav",      css [display_block])
     , ("object",   inline)
     , ("ol",       css [display_block])
     , ("p",        css [display_block])
     , ("pre",      css [display_block, ("white-space", "pre"), ("margin", "13px 0"), fontfamily defaultFontFaceMono])
     , ("q",        inline)
     , ("samp",     inline)
     , ("script",   inline)
     , ("section",  css [display_block])
     , ("select",   inline)
     , ("small",    css [("font-size", "83%"), display_inline])
     , ("span",     inline)
     , ("strike",   css [("text-decoration", "line-through"), display_inline])
     , ("strong",   css [fontweight_bold, display_inline])
     , ("sub",      inline)
     , ("sup",      inline)
     , ("table",    css [display_block])
     , ("td",       inline)   -- TODO: proper tables
     , ("th",       inline)   -- TODO: proper tables
     , ("time",     inline)
     , ("textarea", inline)
     , ("tr",       css [display_block])     -- TODO: proper tables
     , ("tt",       inline)
     , ("u",        css [("text-decoration", "underline"), display_inline])
     , ("ul",       css [display_block])
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
    display_block = ("display", "block")
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
vadoPage doc page = page
  { pageState = PageReady
  , pageDocument = doc
  , pageBoxes = Nothing
  , pageEvents = IM.empty
  , pageWindow = (pageWindow page){ windowScroll = 0 }
  }

vadoHome :: Document
vadoHome =
  MId.runIdentity $ fromEmptyDocument $ do
    modify $ \doc -> doc{ documentLocation = fromJust $ URI.parseURI "vado:home" }
    htmlDOMFromXML $ xmlHtml body
  where
    body =
      xmlNode' "body" [("style", "text-align: center; white-space: pre")]
        [ xmlNode "h1" [ xmlText "\n\n\nVado"]
        , xmlNode "hr" []
        , xmlNode' "notform" [("action", "vado:go"), ("method", "POST"), ("id", "the-form")]
          [ xmlNode' "input" inputAttrs []
          , xmlNode "br" []
          , xmlNode' "input" [("type", "submit"), ("value", "I go!")] []
          ]
        ]
    inputAttrs =
        [ ("type", "text"), ("name", "url"), ("id", "vado")
        , ("class", "test1 test2"), ("autofocus", "")
        ]

-- TODO: use a "change" event
vadoHome_onKeyReleased :: EventHandler
vadoHome_onKeyReleased event nid = do
  let Right evsdl = event
  let KeyboardEvent e = SDL.eventPayload evsdl
  let keysym = SDL.keyboardEventKeysym e
  case SDL.keysymKeycode keysym of
    SDL.KeycodeReturn -> do
      -- get the address
      page <- gets evctxPage
      node <- inPageDocument page $ getElement nid
      liftIO $ print node
      let Left (InputTextContent address) = elementContent node
      -- set up the waiting page
      liftIO (layoutPage (vadoPage vadoWait page) >>= renderDOM)
      -- decide where to go
      let mbHref =  URI.parseURI $ T.unpack address
      let href = fromMaybe (webSearch $ T.unpack address) mbHref
      page' <- liftIO $ navigatePage href page
      put EventContext
        { evctxPage = page'
        , evctxStopPropagation = True
        , evctxPreventDefault = True
        }
    _ ->
      return ()


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

