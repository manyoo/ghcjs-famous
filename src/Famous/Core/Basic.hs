{-# LANGUAGE JavaScriptFFI, TypeSynonymInstances, OverloadedStrings #-}
module Famous.Core.Basic where

import GHCJS.Foreign
import GHCJS.Marshal
import qualified Data.Map as Map
import Control.Monad (forM_)
import Data.Text

type Options k v = Map.Map k v


instance (ToJSString k, ToJSRef v) => ToJSRef (Options k v) where
  toJSRef m = do
    o <- newObj
    forM_ (Map.toList m) $ \(k, v) -> do v' <- toJSRef v
                                         setProp (toJSString k) v' o
    return o


-- | Size Mode
data SizeMode = SMAbsolute
              | SMRelative
              | SMRender


sizeMode2Text :: SizeMode -> Text
sizeMode2Text SMAbsolute = "absolute"
sizeMode2Text SMRelative = "relative"
sizeMode2Text SMRender   = "render"
