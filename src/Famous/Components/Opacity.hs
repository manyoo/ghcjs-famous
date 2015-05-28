{-# LANGUAGE JavaScriptFFI #-}
module Famous.Components.Opacity where

import GHCJS.Foreign
import GHCJS.Types

import Famous.Core.Node

data Opacity_

type Opacity = JSRef Opacity_


-- | Add a new Opacity component to a node
foreign import javascript unsafe "new famous.components.Opacity($1)"
  fms_addOpacity :: Node a -> IO Opacity

addOpacity = fms_addOpacity


-- | Get the current opacity for the component
foreign import javascript safe "($1).get()"
  fms_getOpacity :: Opacity -> Double

getOpacity = fms_getOpacity

-- | Stops Opacity transition
foreign import javascript unsafe "($1).halt()"
  fms_halt :: Opacity -> IO ()

halt = fms_halt

-- | Tells whether or not the opacity is in a transition
foreign import javascript safe "($1).isActive()"
  fms_isActive :: Opacity -> Bool

isActive = fms_isActive
