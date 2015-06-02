{-# LANGUAGE JavaScriptFFI #-}
module Famous.Components.Size where

import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal

import Famous.Core.Basic
import Famous.Core.Node
import Famous.Transitions.Curves

data Size_ a

type Size a = JSRef (Size_ a)

-- | Add a Size component to a node
foreign import javascript unsafe "new famous.components.Size($1)"
  fms_addSize :: Node a -> IO (Size ())

addSize = fms_addSize

-- | Helper function that grabs the activity of a certain type of size.
foreign import javascript safe "($2).isActive($1)"
  fms_isActive :: JSString -> Size a -> Bool

isActive :: ToJSString a => a -> Size b -> Bool
isActive t s = fms_isActive (toJSString t) s

-- | Set which mode each axis of Size will have its dimensions calculated by.
foreign import javascript unsafe "($4).setMode($1, $2, $3)"
  fms_setMode :: JSRef a -> JSRef a -> JSRef a -> Size b -> IO ()

setMode :: SizeMode -> SizeMode -> SizeMode -> Size a -> IO ()
setMode x y z s = do xRef <- sm2Ref x
                     yRef <- sm2Ref y
                     zRef <- sm2Ref z
                     fms_setMode xRef yRef zRef s
  where sm2Ref = toJSRef . sizeMode2Text

mkCallback = syncCallback AlwaysRetain True

-- | Applies Absolute size
foreign import javascript unsafe "($5).setAbsolute($1, $2, $3, $4)"
  fms_setAbsoluteCurve :: Double -> Double -> Double -> JSRef Curve -> Size a -> IO ()
foreign import javascript unsafe "($6).setAbsolute($1, $2, $3, $4)"
  fms_setAbsoluteCurveCb :: Double -> Double -> Double -> JSRef Curve -> JSFun (IO ()) -> Size a -> IO ()

setAbsolute :: Double -> Double -> Double -> Curve -> Maybe (IO ()) -> Size a -> IO ()
setAbsolute x y z c Nothing s = do jsc <- toJSRef c
                                   fms_setAbsoluteCurve x y z jsc s
setAbsolute x y z c (Just f) s = do jsc <- toJSRef c
                                    cb <- mkCallback f
                                    fms_setAbsoluteCurveCb x y z jsc cb s

-- | Applies Proporional size
foreign import javascript unsafe "($5).setProportional($1, $2, $3, $4)"
  fms_setProportionalCurve :: Double -> Double -> Double -> JSRef Curve -> Size a -> IO ()
foreign import javascript unsafe "($6).setProportional($1, $2, $3, $4)"
  fms_setProportionalCurveCb :: Double -> Double -> Double -> JSRef Curve -> JSFun (IO ()) -> Size a -> IO ()

setProportional :: Double -> Double -> Double -> Curve -> Maybe (IO ()) -> Size a -> IO ()
setProportional x y z c Nothing s = do jsc <- toJSRef c
                                       fms_setProportionalCurve x y z jsc s
setProportional x y z c (Just f) s = do jsc <- toJSRef c
                                        cb <- mkCallback f
                                        fms_setProportionalCurveCb x y z jsc cb s

-- | Applies Differential size
foreign import javascript unsafe "($5).setDifferential($1, $2, $3, $4)"
  fms_setDifferentialCurve :: Double -> Double -> Double -> JSRef Curve -> Size a -> IO ()
foreign import javascript unsafe "($6).setDifferential($1, $2, $3, $4)"
  fms_setDifferentialCurveCb :: Double -> Double -> Double -> JSRef Curve -> JSFun (IO ()) -> Size a -> IO ()

setDifferential :: Double -> Double -> Double -> Curve -> Maybe (IO ()) -> Size a -> IO ()
setDifferential x y z c Nothing s = do jsc <- toJSRef c
                                       fms_setDifferentialCurve x y z jsc s
setDifferential x y z c (Just f) s = do jsc <- toJSRef c
                                        cb <- mkCallback f
                                        fms_setDifferentialCurveCb x y z jsc cb s

-- | Halts all currently active size transitions.
foreign import javascript unsafe "($1).halt()"
  fms_halt :: Size a -> IO ()

halt = fms_halt

