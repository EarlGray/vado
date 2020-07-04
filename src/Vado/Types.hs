module Vado.Types where

import Debug.Trace as Trace

warning :: String -> a -> a
warning msg = Trace.trace msg


class HasDebugView a where
  showdbg :: a -> String
