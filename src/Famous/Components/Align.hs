{-# LANGUAGE JavaScriptFFI #-}
module Famous.Components.Align where

import GHCJS.Foreign
import GHCJS.Types

import Famous.Core.Node
import Famous.Components.Position

data Align_ a

type Align a = Position (Align_ a)


-- | Add a new Align component to a node
foreign import javascript unsafe "new famous.components.Align($1)"
  fms_addAlign :: Node a -> IO (Align ())

addAlign = fms_addAlign

