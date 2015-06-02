{-# LANGUAGE OverloadedStrings #-}
module Famous.Transitions.Curves where

import Data.Text

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


curveType2Text :: CurveType -> Text
curveType2Text CvLinear        = "linear"
curveType2Text CvEaseIn        = "easeIn"
curveType2Text CvEaseOut       = "easeOut"
curveType2Text CvEaseInOut     = "easeInOut"
curveType2Text CvEaseOutBounce = "easeOutBounce"
curveType2Text CvSpring        = "spring"
curveType2Text CvInQuad        = "inQuad"
curveType2Text CvOutQuad       = "outQuad"
curveType2Text CvInOutQuad     = "inOutQuad"
curveType2Text CvInCubic       = "inCubic"
curveType2Text CvOutCubic      = "outCubic"
curveType2Text CvInOutCubic    = "inOutCubic"
curveType2Text CvInQuart       = "inQuart"
curveType2Text CvOutQuart      = "outQuart"
curveType2Text CvInOutQuart    = "inOutQuart"
curveType2Text CvInQuint       = "inQuint"
curveType2Text CvOutQuint      = "outQuint"
curveType2Text CvInOutQuint    = "inOutQuint"
curveType2Text CvInSine        = "inSine"
curveType2Text CvOutSine       = "outSine"
curveType2Text CvInOutSine     = "inOutSine"
curveType2Text CvInExpo        = "inExpo"
curveType2Text CvOutExpo       = "outExpo"
curveType2Text CvInOutExpo     = "inOutExpo"
curveType2Text CvInCirc        = "inCirc"
curveType2Text CvOutCirc       = "outCirc"
curveType2Text CvInOutCirc     = "inOutCirc"
curveType2Text CvInElastic     = "inElastic"
curveType2Text CvOutElastic    = "outElastic"
curveType2Text CvInOutElastic  = "inOutElastic"
curveType2Text CvInBack        = "inBack"
curveType2Text CvOutBack       = "outBack"
curveType2Text CvInOutBack     = "inOutBack"
curveType2Text CvInBounce      = "inBounce"
curveType2Text CvOutBounce     = "outBounce"
curveType2Text CvInOutBounce   = "inOutBounce"
curveType2Text CvFlat          = "flat"


data Curve = Curve {
  duration  :: Int,
  curveType :: CurveType
  }


instance ToJSRef Curve where
  toJSRef c = do
    o <- newObj
    
    let dk = toJSString ("duration" :: Text)
        ck = toJSString ("curve" :: Text)
        
    d <- toJSRef $ duration c
    setProp dk d o
    
    ct <- toJSRef $ curveType2Text $ curveType c
    setProp ck ct o
    
    return o


-- | default curve, used to create new curves
-- example:  mkCurve { duration = 2000, curveType = CvIntBounce }
mkCurve = Curve 1000 CvLinear
