{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Eta reduce" -}
-- more is less
{- HLINT ignore "Use fmap" -}
-- plain Haskell
{- HLINT ignore "Use second" -}
-- plain Haskell

module Vado.Resource where

import           Control.Applicative hiding (empty)
import qualified Control.Exception as Exc   -- TODO: safe-exceptions?
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Resource (runResourceT)

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as Bc
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.URI as URI

import qualified Data.Attoparsec.Text as Atto

import           Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.TMChan as Ch

import qualified Control.Concurrent as CC
import           Control.Concurrent.STM (atomically)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import           Network.HTTP.Types.URI (urlDecode)
import qualified Web.FormUrlEncoded as Form

import qualified Data.XML.Types as XML
import qualified Text.HTML.DOM as HTML


import           Vado.Types
import Data.Functor (($>))

-- | Names
type Chan a = Ch.TMChan a

send :: MonadIO m => Chan a -> a -> m ()
send chan message = liftIO $ atomically $ Ch.writeTMChan chan message

-- | Utils
httpHeader :: [HTTP.Header] -> HTTP.HeaderName -> Maybe Text
httpHeader headers which = T.toLower . T.decodeLatin1 <$> L.lookup which headers

httpResponseContentType :: HTTP.Response HTTP.BodyReader -> Text
httpResponseContentType resp =
  fromMaybe "application/octet-stream" $ httpHeader (HTTP.responseHeaders resp) "content-type"

mimeTypeForm :: Bc.ByteString
mimeTypeForm = T.encodeUtf8 "application/x-www-form-urlencoded"

-- | Resource Manager: public interface
-- | It exposes non-blocking operations that put requests into a queue.
-- | Results are obtained from Resource.waitEvent or Resource.drainEvents.

data Manager = Manager
  { manReqChan :: Chan Request
  , manEventChan :: Chan Event
  }

runManager :: IO Manager
runManager = do
  inch <- atomically Ch.newTMChan
  outch <- atomically Ch.newTMChan
  httpman <- HTTP.newManager TLS.tlsManagerSettings
  _ <- CC.forkIO $ do
    eventloop inch outch (ManagerState M.empty httpman)
       `Exc.catch` \(e :: Exc.SomeException) -> putStrLn $ "ResourceManager failed: " ++ show e
  return Manager{ manReqChan=inch, manEventChan=outch }

quitManager :: Manager -> IO ()
quitManager Manager{..} = manReqChan `send` Quit

-- | Resource Manager requests:
requestGet :: Manager -> URI -> [Text] -> IO ()
requestGet Manager{..} uri mimetypes =
  manReqChan `send` Get uri mimetypes

requestPost :: Manager -> URI -> Content -> [Text] -> IO ()
requestPost Manager{..} uri content mimetypes = manReqChan `send` Post uri content mimetypes

requestCancel :: Manager -> URI -> IO ()
requestCancel Manager{..} uri = manReqChan `send` Cancel uri

-- | Resource Manager events
type Event = (URI, EventKind)
data EventKind
  = EventConnectionError Exc.SomeException
  -- ^ could not connect
  | EventStreamMetadata (HTTP.Request, HTTP.Response HTTP.BodyReader) (Maybe Text)
  -- ^              final request and response;     maybe mime-type for streaming
  | EventStreamChunk Chunk
  -- ^ next chunk of a streamed resource
  | EventStreamClose
  -- ^ no more chunks of a streamed resource
  | EventContentReady (HTTP.Request, HTTP.Response HTTP.BodyReader) Content
  -- ^ content of the resource for non-streaming responses

data Chunk
  = ChunkHtml XML.Event
  | ChunkText Text
  | ChunkBytes Bs.ByteString
  deriving (Eq, Show)

data Content
  = ContentBytes Bs.ByteString
  | ContentXML XML.Document
  | ContentForm (M.Map Text [Text])
  deriving (Eq)

instance Show EventKind where
  show (EventConnectionError exc) = "EventConnectionError " ++ show exc
  show (EventStreamMetadata (req, resp) mbStreamType) = unlines $
      ["EventStreamMetadata " ++ maybe "" T.unpack mbStreamType
      , "> " ++ show (HTTP.method req) ++ " " ++ show (HTTP.path req) ++ " " ++ show ver
      ]
      ++ map (\(name, value) -> "> " ++ show name ++ ": " ++ show value) reqh
      ++ ["< " ++ show (HTTP.statusCode status) ++ " " ++ show (HTTP.statusMessage status)]
      ++ map (\(name, value) -> "< " ++ show name ++ ": " ++ show value) resph
    where
      ver = HTTP.requestVersion req
      reqh = HTTP.requestHeaders req
      status = HTTP.responseStatus resp
      resph = HTTP.responseHeaders resp
  show EventStreamClose = "EventStreamClose"
  show (EventStreamChunk chunk) = "EventStreamChunk " ++ show chunk
  show (EventContentReady (req, resp) content) = unlines $
      ["EventContentReady " ++ show content
      , "> " ++ show (HTTP.method req) ++ " " ++ show (HTTP.path req) ++ " " ++ show ver
      ]
      ++ map (\(name, value) -> "> " ++ show name ++ ": " ++ show value) reqh
      ++ ["< " ++ show (HTTP.statusCode status) ++ " " ++ show (HTTP.statusMessage status)]
      ++ map (\(name, value) -> "< " ++ show name ++ ": " ++ show value) resph
    where
      ver = HTTP.requestVersion req
      reqh = HTTP.requestHeaders req
      status = HTTP.responseStatus resp
      resph = HTTP.responseHeaders resp

instance Show Content where
  show = \case
    ContentXML _ -> "Resource.ContentXML "
    ContentBytes bs -> "Resource.ContentBytes <" ++ show (Bs.length bs) ++ " bytes>"
    ContentForm fields -> "Resource.ContentForm " ++ show fields

waitEvent :: Manager -> IO Event
waitEvent resman = do
  event <- atomically $ Ch.readTMChan $ manEventChan resman
  case event of
    Just e -> return e
    Nothing -> waitEvent resman

drainEvents :: Manager -> IO [Event]
drainEvents resman = go
  where
    evch = manEventChan resman
    go = do
      empty <- atomically $ Ch.isEmptyTMChan evch
      if empty then return []
      else do
        Just ev <- atomically $ Ch.readTMChan evch
        (ev:) <$> go

-- | Resource Manager: internal channel interface

data Request
  = Get URI [Text]
  | Post URI Content [Text]
  -- ^ request uri, possibly setting up a streaming parser in mimetypes
  | Cancel URI
  | Done URI        -- only for workers
  | Quit
  deriving (Eq, Show)


data ManagerState = ManagerState
  { manThreadByURI :: M.Map URI CC.ThreadId
  , manHTTPManager :: HTTP.Manager
  }

eventloop :: Chan Request -> Chan Event -> ManagerState -> IO ()
eventloop inch outch state = do
  Just req <- atomically $ Ch.readTMChan inch
  case req of
    Quit ->
      CC.myThreadId >>= CC.killThread
    Get uri mimetypes -> do
      tid <- CC.forkIO $ runStreaming mimetypes inch outch (manHTTPManager state) uri Nothing
      eventloop inch outch $ state{ manThreadByURI = M.insert uri tid $ manThreadByURI state }
    Post uri content mimetypes -> do
      tid <- CC.forkIO $ runStreaming mimetypes inch outch (manHTTPManager state) uri (Just content)
      eventloop inch outch $ state{ manThreadByURI = M.insert uri tid $ manThreadByURI state }
    Done uri -> do
      eventloop inch outch $ state{ manThreadByURI = M.delete uri $ manThreadByURI state }
    Cancel uri -> do
      forM_ (M.lookup uri $ manThreadByURI state) $ \tid ->
        CC.killThread tid
      eventloop inch outch $ state{ manThreadByURI = M.delete uri $ manThreadByURI state }

runStreaming :: [Text] -> Chan Request -> Chan Event
             -> HTTP.Manager -> URI -> Maybe Content
             -> IO ()
runStreaming mimetypes inch outch httpman uri mbContent = do
    doHttp httpman uri mbContent maybeStreamBody
        `Exc.catch` \(e :: Exc.SomeException) -> outch `send` (uri, EventConnectionError e)
    inch `send` Done uri
  where
    maybeStreamBody :: BodyHandler
    maybeStreamBody (req, resp) = do
      let headers = HTTP.responseHeaders resp
      let contentType = fromMaybe "application/octet-stream" $ httpHeader headers "content-type"
      if "text/html" `T.isPrefixOf` contentType && "text/html" `L.elem` mimetypes
      then runResourceT $ do
        liftIO $ outch `send` (uri, EventStreamMetadata (req, resp) (Just "text/html"))
        let body = HTTP.responseBody resp
        let consumeHttp _ = runResourceT $ do
              chunk <- liftIO $ HTTP.brRead body
              return $ if Bs.null chunk then Nothing else Just (chunk, ())
        runConduit $ CL.unfoldM consumeHttp ()
                  .| HTML.eventConduit
                  .| CL.map (\chunk -> (uri, EventStreamChunk (ChunkHtml chunk)))
                  .| Ch.sinkTMChan outch
        liftIO $ outch `send` (uri, EventStreamClose)
      else do
        bs <- Bs.concat <$> HTTP.brConsume (HTTP.responseBody resp)
        outch `send` (uri, EventContentReady (req, resp) (ContentBytes bs))

type BodyHandler = (HTTP.Request, HTTP.Response HTTP.BodyReader) -> IO ()

doHttp :: HTTP.Manager -> URI -> Maybe Content -> BodyHandler -> IO ()
doHttp httpman uri mbContent onBody = do
  req0 <- HTTP.requestFromURI uri
  let req1 = req0
        { HTTP.requestHeaders = [ (HTTP.hUserAgent, "Vado Browser") ]
        }
  let req2 = case mbContent of
        Just (ContentForm fields) ->
          let lbs = Form.urlEncodeAsForm fields
          in req1
            { HTTP.method = "POST"
            , HTTP.requestHeaders =
               (HTTP.hContentType, mimeTypeForm) :
               HTTP.requestHeaders req1
            , HTTP.requestBody = HTTP.RequestBodyLBS lbs
            }
        Just other -> warning ("TODO: doHTTP POST for " ++ show other) req1
        Nothing -> req1

  HTTP.withResponseHistory req2 httpman $ \respHistory -> do
    let req = HTTP.hrFinalRequest respHistory
    let resp = HTTP.hrFinalResponse respHistory
    onBody (req, resp)

-- | data: urls helpers
decodeDataURL :: Text -> Either String (Text, Content)
decodeDataURL datauri =
  (\(conttype, bs) -> (conttype, ContentBytes bs)) <$> Atto.parseOnly attoDataUrl datauri

attoDataUrl :: Atto.Parser (Text, Bs.ByteString)
attoDataUrl = do
  _ <- Atto.string "data:"
  (mty, msub, _mparams) <- Atto.option ("text", "plain", []) attoMimeType
  -- TODO: use charset from _mparams?
  isBase64 <- Atto.option False (Atto.string ";base64" $> True)
  _ <- Atto.char ','
  bytes <- Bc.pack . T.unpack <$> Atto.takeText
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
    token = T.pack <$> Atto.many1 (Atto.satisfy (\c -> Atto.notInClass tsspecials c && not (C.isSpace c)))
    qstr = fail "TODO: quoted strings as mime/type;parameter='value'"
    parameter = do
      attr <- token
      _ <- Atto.char '='
      value <- token <|> qstr
      return (T.toLower attr, value)

-- | Quick test: e.g. `test "http://localhost:8000/acid0.html"`
test :: Text -> IO ()
test address = do
  resman <- runManager
  let uri = fromJust $ URI.parseAbsoluteURI $ T.unpack address
  requestGet resman uri ["text/html", "text"]
  runResourceT $ do
    let sink (u, r) = do
          case r of
            EventStreamChunk (ChunkHtml (XML.EventBeginElement name attrs)) | name == "img" -> do
              let href = case head $ fromJust $ L.lookup "src" attrs of
                    XML.ContentText h -> h
                    content -> error $ "wrong content " ++ show content
              putStrLn $ "@@@ image: src=" ++ show href
              let rel = fromJust $ URI.parseURIReference $ T.unpack href
              let imguri = rel `URI.relativeTo` u
              requestGet resman imguri []
            _ -> return ()
          print (u, r)
    runConduit (Ch.sourceTMChan (manEventChan resman)  .| CL.mapM_ (liftIO . sink))

testPost :: Text -> IO ()
testPost address = do
  resman <- runManager
  let uri = fromJust $ URI.parseAbsoluteURI $ T.unpack address
  requestPost resman uri (ContentForm $ M.fromList [("hello", ["world", "світ"])]) []
  evt <- waitEvent resman
  print evt
