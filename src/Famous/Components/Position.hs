{-# LANGUAGE JavaScriptFFI #-}
module Famous.Components.Position where

import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal

import Famous.Core.Node
import Famous.Transitions.Curves

data Position_ a

type Position a = JSRef (Position_ a)

-- | Add a new Position component to a node
foreign import javascript unsafe "new window.famous.components.Position($1)"
  fms_addPosition :: Node a -> IO (Position ())

addPosition = fms_addPosition

-- | get x translation
foreign import javascript safe "($1).getX()"
  fms_getX :: Position a -> Double

getX = fms_getX

-- | get y translation
foreign import javascript safe "($1).getY()"
  fms_getY :: Position a -> Double

getY = fms_getY

-- | get z translation
foreign import javascript safe "($1).getZ()"
  fms_getZ :: Position a -> Double

getZ = fms_getZ

-- | Whether or not the Position is currently changing
foreign import javascript safe "($1).isActive()"
  fms_isActive :: Position a -> Bool

isActive = fms_isActive

mkCallback = syncCallback AlwaysRetain True

-- | set the X position with parameters
foreign import javascript unsafe "($3).setX($1, $2)"
  fms_setXCurve :: Double -> JSRef Curve -> Position a -> IO ()
foreign import javascript unsafe "($4).setX($1, $2, $3)"
  fms_setXCurveCb :: Double -> JSRef Curve -> JSFun (IO ()) -> Position a -> IO ()

setX :: Double -> Curve -> Maybe (IO ()) -> Position a -> IO ()
setX x c Nothing p = do jsc <- toJSRef c
                        fms_setXCurve x jsc p
setX x c (Just f) p = do jsc <- toJSRef c
                         cb <- mkCallback f
                         fms_setXCurveCb x jsc cb p

-- | set the Y position with parameters
foreign import javascript unsafe "($3).setY($1, $2)"
  fms_setYCurve :: Double -> JSRef Curve -> Position a -> IO ()
foreign import javascript unsafe "($4).setY($1, $2, $3)"
  fms_setYCurveCb :: Double -> JSRef Curve -> JSFun (IO ()) -> Position a -> IO ()

setY :: Double -> Curve -> Maybe (IO ()) -> Position a -> IO ()
setY y c Nothing p = do jsc <- toJSRef c
                        fms_setYCurve y jsc p
setY y c (Just f) p = do jsc <- toJSRef c
                         cb <- mkCallback f
                         fms_setYCurveCb y jsc cb p

-- | set the Z position with parameters
foreign import javascript unsafe "($3).setZ($1, $2)"
  fms_setZCurve :: Double -> JSRef Curve -> Position a -> IO ()
foreign import javascript unsafe "($4).setZ($1, $2, $3)"
  fms_setZCurveCb :: Double -> JSRef Curve -> JSFun (IO ()) -> Position a -> IO ()

setZ :: Double -> Curve -> Maybe (IO ()) -> Position a -> IO ()
setZ z c Nothing p = do jsc <- toJSRef c
                        fms_setZCurve z jsc p
setZ z c (Just f) p = do jsc <- toJSRef c
                         cb <- mkCallback f
                         fms_setZCurveCb z jsc cb p


-- | set the x, y, z values
foreign import javascript unsafe "($5).set($1, $2, $3, $4)"
  fms_setCurve :: Double -> Double -> Double -> JSRef Curve -> Position a -> IO ()
foreign import javascript unsafe "($6).set($1, $2, $3, $4, $5)"
  fms_setCurveCb :: Double -> Double -> Double -> JSRef Curve -> JSFun (IO ()) -> Position a -> IO ()

set :: Double -> Double -> Double -> Curve -> Maybe (IO ()) -> Position a -> IO ()
set x y z c Nothing p = do jsc <- toJSRef c
                           fms_setCurve x y z jsc p
set x y z c (Just f) p = do jsc <- toJSRef c
                            cb <- mkCallback f
                            fms_setCurveCb x y z jsc cb p

-- | Stops transition of Position component
foreign import javascript unsafe "($1).halt()"
  fms_halt :: Position a -> IO ()

halt = fms_halt
