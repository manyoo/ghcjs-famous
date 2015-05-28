{-# LANGUAGE JavaScriptFFI #-}
module Famous.Components.Scale where

import GHCJS.Foreign
import GHCJS.Types

import Famous.Core.Node
import Famous.Components.Position

data Scale_ a

type Scale a = Position (Scale_ a)

-- | Add a new Scale component to a node
foreign import javascript unsafe "new famous.components.Scale($1)"
  fms_addScale :: Node a -> IO (Scale ())

addScale = fms_addScale

