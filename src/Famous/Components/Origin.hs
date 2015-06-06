{-# LANGUAGE JavaScriptFFI #-}
module Famous.Components.Origin where

import GHCJS.Types
import GHCJS.Foreign

import Famous.Core.Node
import Famous.Components.Position

data Origin_ a

type Origin a = Position (Origin_ a)

-- | Add a new Origin component to a node
foreign import javascript unsafe "new window.famous.components.Origin($1)"
  fms_addOrigin :: Node a -> IO (Origin ())

addOrigin = fms_addOrigin
