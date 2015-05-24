{-# LANGUAGE JavaScriptFFI #-}
module Famous.Core.Node where

import GHCJS.Foreign
import GHCJS.Types

data Node_ a

type Node a = JSRef (Node_ a)


