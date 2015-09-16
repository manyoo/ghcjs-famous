{-# LANGUAGE OverloadedStrings #-}
module Famous.Transitions.Curves where

import qualified JavaScript.Object as Obj
import Data.JSString

import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal

data CurveType = CvLinear
               | CvEaseIn
               | CvEaseOut
               | CvEaseInOut
               | CvEaseOutBounce
               | CvSpring
               | CvInQuad
               | CvOutQuad
               | CvInOutQuad
               | CvInCubic
               | CvOutCubic
               | CvInOutCubic
               | CvInQuart
               | CvOutQuart
               | CvInOutQuart
               | CvInQuint
               | CvOutQuint
               | CvInOutQuint
               | CvInSine
               | CvOutSine
               | CvInOutSine
               | CvInExpo
               | CvOutExpo
               | CvInOutExpo
               | CvInCirc
               | CvOutCirc
               | CvInOutCirc
               | CvInElastic
               | CvOutElastic
               | CvInOutElastic
               | CvInBack
               | CvOutBack
               | CvInOutBack
               | CvInBounce
               | CvOutBounce
               | CvInOutBounce
               | CvFlat


curveType2JSString :: CurveType -> JSString
curveType2JSString CvLinear        = "linear"
curveType2JSString CvEaseIn        = "easeIn"
curveType2JSString CvEaseOut       = "easeOut"
curveType2JSString CvEaseInOut     = "easeInOut"
curveType2JSString CvEaseOutBounce = "easeOutBounce"
curveType2JSString CvSpring        = "spring"
curveType2JSString CvInQuad        = "inQuad"
curveType2JSString CvOutQuad       = "outQuad"
curveType2JSString CvInOutQuad     = "inOutQuad"
curveType2JSString CvInCubic       = "inCubic"
curveType2JSString CvOutCubic      = "outCubic"
curveType2JSString CvInOutCubic    = "inOutCubic"
curveType2JSString CvInQuart       = "inQuart"
curveType2JSString CvOutQuart      = "outQuart"
curveType2JSString CvInOutQuart    = "inOutQuart"
curveType2JSString CvInQuint       = "inQuint"
curveType2JSString CvOutQuint      = "outQuint"
curveType2JSString CvInOutQuint    = "inOutQuint"
curveType2JSString CvInSine        = "inSine"
curveType2JSString CvOutSine       = "outSine"
curveType2JSString CvInOutSine     = "inOutSine"
curveType2JSString CvInExpo        = "inExpo"
curveType2JSString CvOutExpo       = "outExpo"
curveType2JSString CvInOutExpo     = "inOutExpo"
curveType2JSString CvInCirc        = "inCirc"
curveType2JSString CvOutCirc       = "outCirc"
curveType2JSString CvInOutCirc     = "inOutCirc"
curveType2JSString CvInElastic     = "inElastic"
curveType2JSString CvOutElastic    = "outElastic"
curveType2JSString CvInOutElastic  = "inOutElastic"
curveType2JSString CvInBack        = "inBack"
curveType2JSString CvOutBack       = "outBack"
curveType2JSString CvInOutBack     = "inOutBack"
curveType2JSString CvInBounce      = "inBounce"
curveType2JSString CvOutBounce     = "outBounce"
curveType2JSString CvInOutBounce   = "inOutBounce"
curveType2JSString CvFlat          = "flat"


data Curve = Curve {
  duration  :: Int,
  curveType :: CurveType
  }


instance ToJSRef Curve where
  toJSRef c = do
    o <- Obj.create

    let dk = "duration"
        ck = "curve"

    d <- toJSRef $ duration c
    Obj.setProp dk d o

    let ct = jsref $ curveType2JSString $ curveType c
    Obj.setProp ck ct o

    return $ jsref o


-- | default curve, used to create new curves
-- example:  mkCurve { duration = 2000, curveType = CvIntBounce }
mkCurve = Curve 1000 CvLinear
