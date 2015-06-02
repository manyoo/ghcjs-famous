{-# LANGUAGE JavaScriptFFI, TypeSynonymInstances #-}
module Famous.Core.Basic where

import GHCJS.Foreign
import GHCJS.Marshal
import qualified Data.Map as Map
import Control.Monad (forM_)


type Options k v = Map.Map k v


instance (ToJSString k, ToJSRef v) => ToJSRef (Options k v) where
  toJSRef m = do
    o <- newObj
    forM_ (Map.toList m) $ \(k, v) -> do v' <- toJSRef v
                                         setProp (toJSString k) v' o
    return o


