{-# LANGUAGE JavaScriptFFI #-}
module Famous.Components.Opacity where

import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal

import Famous.Core.Node
import Famous.Transitions.Curves

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

-- | set the opacity value with transition
foreign import javascript unsafe "($3).set($1, $2)"
  fms_setCurve :: Double -> JSRef Curve -> Opacity -> IO ()
foreign import javascript unsafe "($4).set($1, $2, $3)"
  fms_setCurveCb :: Double -> JSRef Curve -> JSFun (IO ()) -> Opacity -> IO ()

set :: Double -> Curve -> Maybe (IO ()) -> Opacity -> IO ()
set ov c Nothing op = do jsc <- toJSRef c
                         fms_setCurve ov jsc op
set ov c (Just f) op = do jsc <- toJSRef c
                          cb <- syncCallback AlwaysRetain True f
                          fms_setCurveCb ov jsc cb op

-- | Stops Opacity transition
foreign import javascript unsafe "($1).halt()"
  fms_halt :: Opacity -> IO ()

halt = fms_halt

-- | Tells whether or not the opacity is in a transition
foreign import javascript safe "($1).isActive()"
  fms_isActive :: Opacity -> Bool

isActive = fms_isActive
