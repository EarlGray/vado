{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Vado.Document where

import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Identity as MId
import           Control.Monad.State (StateT(..), gets, modify)
import qualified Control.Monad.State as St
import qualified Data.Either as Ei
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.XML.Types as XMLTy
import           Linear.V2 (V2(..))
import           Network.URI as URI
import qualified Text.XML as XML

import qualified SDL

import           Vado.Types
import           Vado.CSS


intmapAppend :: a -> IM.IntMap a -> IM.IntMap a
intmapAppend val im | IM.null im = IM.fromList [(0, val)]
intmapAppend val im = let (i, _) = IM.findMax im in IM.insert (i+1) val im


-- | This is a webpage
-- |   - contains the DOM tree
-- |   - contains the CSS rule storage
-- |   - contains the rendering cache(s)
data Document = Document
  { documentAllNodes :: IM.IntMap Element
  , documentNewID :: ElementID
  , documentBuildState :: [ElementID]     -- current open elements during adding
  --, documentSettings :: DocumentSettings  -- builtin styles, fonts, etc.

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
  { documentAllNodes = IM.fromList [(htmlElement, documentShadowRoot)]
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

documentShadowRoot :: Element
documentShadowRoot = Element
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

{-
data DocumentSettings = DocumentSettings
  { docBrowserStyle :: CSSRules
  --, docInitialFontSize :: Double,
  --, docInitialFontFaceSans :: Text
  --, docInitialFontFaceSerif :: Text
  --, docInitialFontFaceMono :: Text
  --, docScaleX :: DoubleX
  --, docScaleY :: DoubleY
  }
-}

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



-- | Having an effect inside DocumentT m (): just queue them for the wrapper to handle.
type EventName = Text

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
