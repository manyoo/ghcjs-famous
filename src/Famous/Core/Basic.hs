{-# LANGUAGE JavaScriptFFI, TypeSynonymInstances, OverloadedStrings, FlexibleInstances #-}
module Famous.Core.Basic where

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import qualified JavaScript.Object as Obj
import JavaScript.Object.Internal as Obj
import qualified Data.Map as Map
import Control.Monad (forM_)
import Data.Text

import Data.JSString

type FamoObj a = JSRef


type Options v = Map.Map JSString v


instance ToJSRef v => ToJSRef (Options v) where
  toJSRef m = do
    o <- Obj.create
    forM_ (Map.toList m) $ \(k, v) -> do v' <- toJSRef v
                                         Obj.setProp k v' o
    return $ jsref o


-- | Size Mode
data SizeMode = SMAbsolute
              | SMRelative
              | SMRender


sizeMode2Text :: SizeMode -> JSString
sizeMode2Text SMAbsolute = "absolute"
sizeMode2Text SMRelative = "relative"
sizeMode2Text SMRender   = "render"
