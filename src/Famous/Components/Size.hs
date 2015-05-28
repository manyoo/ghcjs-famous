{-# LANGUAGE JavaScriptFFI #-}
module Famous.Components.Size where

import GHCJS.Foreign
import GHCJS.Types

import Famous.Core.Node

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

