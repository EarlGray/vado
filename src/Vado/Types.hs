module Vado.Types where

import qualified Data.Map as M
import           Debug.Trace as Trace

import           Data.Text (Text)

type TagName = Text
type TagAttrs = (Text, M.Map Text Text)

warning :: String -> a -> a
warning msg = Trace.trace msg


class HasDebugView a where
  showdbg :: a -> String
