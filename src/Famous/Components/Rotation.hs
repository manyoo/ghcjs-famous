{-# LANGUAGE JavaScriptFFI #-}
module Famous.Components.Rotation where

import GHCJS.Foreign
import GHCJS.Types

import Famous.Core.Node
import Famous.Components.Position

data Rotation_ a

type Rotation a = Position (Rotation_ a)

-- | add a new Rotation component to a node
foreign import javascript unsafe "new famous.components.Rotation($1)"
  fms_addRotation :: Node a -> IO (Rotation ())

addRotation = fms_addRotation
