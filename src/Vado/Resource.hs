{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vado.Resource where

import qualified Control.Exception as Exc   -- TODO: safe-exceptions?
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (runResourceT)

import qualified Data.ByteString as Bs
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.URI as URI


import           Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.TMChan as Ch

import qualified Control.Concurrent as CC
import           Control.Concurrent.STM (atomically)

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP

import qualified Data.XML.Types as XML
import qualified Text.HTML.DOM as HTML


type Chan a = Ch.TMChan a

send :: Chan a -> a -> IO ()
send chan message = atomically $ Ch.writeTMChan chan message


data Request
  = Get URI
  | Stream URI
  | Cancel URI
  | Done URI        -- only for workers
  | Quit
  deriving (Eq, Show)

data Response
  = ResError URI Exc.SomeException                      -- could not connect
  | ResMetadata URI HTTP.Request (HTTP.Response HTTP.BodyReader)  -- final request and response
  | ResChunk URI Chunk          -- next chunk of a streamed resource
  | ResClose URI                -- no more chunks of a streamed resource
  | ResReady URI Bs.ByteString  -- body of the resource

instance Show Response where
  show (ResError uri exc) = "ResError " ++ show uri ++ " " ++ show exc
  show (ResMetadata uri req resp) = unlines $
      ["ResMetadata " ++ show uri ++ " "
      , "> " ++ show (HTTP.method req) ++ " " ++ show (HTTP.path req) ++ " " ++ show ver
      ]
      ++ (map (\(name, value) -> "> " ++ show name ++ ": " ++ show value) reqh)
      ++ ["< " ++ show (HTTP.statusCode status) ++ " " ++ show (HTTP.statusMessage status)]
      ++ (map (\(name, value) -> "< " ++ show name ++ ": " ++ show value) resph)
    where
      ver = HTTP.requestVersion req
      reqh = HTTP.requestHeaders req
      status = HTTP.responseStatus resp
      resph = HTTP.responseHeaders resp
  show (ResChunk uri chunk) = "ResChunk " ++ show uri ++ " " ++ show chunk
  show (ResClose uri) = "ResClose " ++ show uri
  show (ResReady uri bs) = "ResReady " ++ show uri ++ " <" ++ show (Bs.length bs) ++ " bytes>"

data Chunk
  = ChunkHtml XML.Event
  | ChunkBytes Bs.ByteString
  deriving (Eq, Show)


runManager :: IO (Chan Request, Chan Response)
runManager = do
  inch <- atomically $ Ch.newTMChan
  outch <- atomically $ Ch.newTMChan
  httpman <- HTTP.newManager TLS.tlsManagerSettings
  _ <- CC.forkIO $ eventloop inch outch $ ManagerState M.empty httpman
  return (inch, outch)


data ManagerState = ManagerState
  { manThreadByURI :: M.Map URI CC.ThreadId
  , manHTTPManager :: HTTP.Manager
  }

eventloop :: Chan Request -> Chan Response -> ManagerState -> IO ()
eventloop inch outch state = do
  Just req <- atomically $ Ch.readTMChan inch
  case req of
    Quit ->
      CC.myThreadId >>= CC.killThread
    Get uri -> do
      tid <- CC.forkIO $ runHttp bodyAsBytestring inch outch (manHTTPManager state) uri
      eventloop inch outch $ state{ manThreadByURI = M.insert uri tid $ manThreadByURI state }
    Stream uri -> do
      tid <- CC.forkIO $ runStreaming inch outch (manHTTPManager state) uri
      eventloop inch outch $ state{ manThreadByURI = M.insert uri tid $ manThreadByURI state }
    Done uri -> do
      outch `send` ResClose uri
      eventloop inch outch $ state{ manThreadByURI = M.delete uri $ manThreadByURI state }
    Cancel uri -> do
      forM_ (M.lookup uri $ manThreadByURI state) $ \tid ->
        CC.killThread tid
      eventloop inch outch $ state{ manThreadByURI = M.delete uri $ manThreadByURI state }

runStreaming :: Chan Request -> Chan Response -> HTTP.Manager -> URI -> IO ()
runStreaming = runHttp maybeStreamBody
  where
    findHeader :: [HTTP.Header] -> HTTP.HeaderName -> Maybe Text
    findHeader headers which = (T.toLower . T.decodeLatin1) <$> L.lookup which headers

    maybeStreamBody outch uri resp = do
      let headers = HTTP.responseHeaders resp
      let contentType = fromMaybe "application/octet-stream" $ findHeader headers "content-type"
      if "text/html" `T.isPrefixOf` contentType
      then runResourceT $ do
        let body = HTTP.responseBody resp
        let consumeHttp _ = runResourceT $ do
              chunk <- liftIO $ HTTP.brRead body
              return $ if Bs.null chunk then Nothing else Just (chunk, ())
        runConduit $ CL.unfoldM consumeHttp ()
                  .| HTML.eventConduit
                  .| CL.map (ResChunk uri . ChunkHtml)
                  .| Ch.sinkTMChan outch
      else
        bodyAsBytestring outch uri resp

type BodyHandler = Chan Response -> URI -> HTTP.Response HTTP.BodyReader -> IO ()

runHttp :: BodyHandler -> Chan Request -> Chan Response -> HTTP.Manager -> URI -> IO ()
runHttp onBody inch outch httpman uri = do
    doHttp outch httpman uri onBody
        `Exc.catch` \(e :: Exc.SomeException) -> outch `send` ResError uri e
    inch `send` Done uri

doHttp :: Chan Response -> HTTP.Manager -> URI -> BodyHandler -> IO ()
doHttp outch httpman uri onBody = do
  req0 <- HTTP.parseRequest $ show uri
  let req = req0 { HTTP.requestHeaders = [ (HTTP.hUserAgent, "Vado Browser") ] }

  HTTP.withResponseHistory req httpman $ \respHistory -> do
    let req' = HTTP.hrFinalRequest respHistory
    let resp = HTTP.hrFinalResponse respHistory
    outch `send` ResMetadata uri req' resp
    onBody outch uri resp

bodyAsBytestring :: BodyHandler
bodyAsBytestring outch uri resp = do
  bs <- Bs.concat <$> HTTP.brConsume (HTTP.responseBody resp)
  outch `send` ResReady uri bs

-- | Quick test: e.g. `test "http://localhost:8000/acid0.html"`
test :: Text -> IO ()
test address = do
  (reqch, respch) <- runManager
  let Just uri = URI.parseAbsoluteURI $ T.unpack address
  reqch `send` Stream uri
  runResourceT $ do
    let sink r = do
          case r of
            ResChunk _ (ChunkHtml (XML.EventBeginElement name attrs)) | name == "img" -> do
              let Just ((XML.ContentText href):_) = L.lookup "src" attrs
              putStrLn $ "@@@ image: src=" ++ show href
              let Just rel = URI.parseURIReference $ T.unpack href
              let imguri = rel `URI.relativeTo` uri
              reqch `send` Get imguri
            _ -> return ()
          print r
    runConduit (Ch.sourceTMChan respch  .| CL.mapM_ (liftIO . sink))
