{- HLINT ignore "Eta reduce" -}

module Vado.Types where

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as M
import           Data.Maybe (listToMaybe)
import           Debug.Trace as Trace
import           Text.Read (readMaybe)

import           Data.Text (Text)
import qualified Data.Text as T

-- | This is a omnipresent module for the project.

type TagName = Text
type TagAttrs = (Text, M.Map Text Text)

warning :: String -> a -> a
warning msg = Trace.trace msg

logWarn :: MonadIO m => String -> m ()
logWarn msg = liftIO $ putStrLn msg

logInfo :: MonadIO m => String -> m ()
logInfo = logWarn


-- Meaningful names:

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = forM_

mbHead :: [a] -> Maybe a
mbHead = listToMaybe

mbReadText :: Read a => Text -> Maybe a
mbReadText = readMaybe . T.unpack

nonEmpty :: Foldable t => t a -> Bool
nonEmpty = not . null

class HasDebugView a where
  showdbg :: a -> String
