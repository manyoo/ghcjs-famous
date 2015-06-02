{-# LANGUAGE JavaScriptFFI #-}
module Famous.DOM.DOMElement where


import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal

import Data.Maybe

import Famous.Core.Basic
import Famous.Core.Node

data DOMElement_ a

type DOMElement a = JSRef (DOMElement_ a)

-- | create a new DOMElement
foreign import javascript unsafe "new famous.domRenderables.DOMElement($1, $2)"
  fms_newDomElement :: Node a -> JSRef b -> IO (DOMElement ())

newDomElement :: (ToJSString k, ToJSRef v) => Node a -> Options k v -> IO (DOMElement ())
newDomElement n opt = toJSRef opt >>= fms_newDomElement n

-- | set the id attribute of the DOMElement
foreign import javascript unsafe "($2).setId($1)"
  fms_setId :: JSString -> DOMElement a -> IO ()

setId :: (ToJSString s) => s -> DOMElement a -> IO ()
setId s d = fms_setId (toJSString s) d

-- | add a new class to the DOM element
foreign import javascript unsafe "($2).addClass($1)"
  fms_addClass :: JSString -> DOMElement a -> IO ()

addClass :: (ToJSString s) => s -> DOMElement a -> IO ()
addClass s d = fms_addClass (toJSString s) d

-- | remove a class from the DOMElement
foreign import javascript unsafe "($2).removeClass($1)"
  fms_removeClass :: JSString -> DOMElement a -> IO ()

removeClass :: (ToJSString s) => s -> DOMElement a -> IO ()
removeClass s d = fms_removeClass (toJSString s) d

-- | checks if the DOMElement has the passed in class
foreign import javascript safe "($2).hasClass($1)"
  fms_hasClass :: JSString -> DOMElement a -> IO Bool

hasClass :: (ToJSString s) => s -> DOMElement a -> IO Bool
hasClass s d = fms_hasClass (toJSString s) d

-- | set an attribute of the DOMElement
foreign import javascript unsafe "($3).setAttribute($1, $2)"
  fms_setAttribute :: JSString -> JSString -> DOMElement a -> IO ()

setAttribute :: (ToJSString n, ToJSString v) => n -> v -> DOMElement a -> IO ()
setAttribute name val d = fms_setAttribute (toJSString name) (toJSString val) d

-- | sets a CSS property
foreign import javascript unsafe "($3).setProperty($1, $2)"
  fms_setProperty :: JSString -> JSString -> DOMElement a -> IO ()

setProperty :: (ToJSString n, ToJSString v) => n -> v -> DOMElement a -> IO ()
setProperty name val d = fms_setProperty (toJSString name) (toJSString val) d

-- | set the content of the DOMElement. must be escaped.
foreign import javascript unsafe "($2).setContent($1)"
  fms_setContent :: JSString -> DOMElement a -> IO ()

setContent :: (ToJSString c) => c -> DOMElement a -> IO ()
setContent c d = fms_setContent (toJSString c) d

-- | subscribes to a DOMElement event
foreign import javascript unsafe "($3).on($1, $2)"
  fms_onEvent :: JSString -> JSFun (a -> IO ()) -> DOMElement b -> IO ()

onEvent :: (ToJSString s, FromJSRef a) => s -> (a -> IO ()) -> DOMElement b -> IO ()
onEvent eventType handler dom = do let f' x = fromJSRef x >>= handler . fromJust
                                   func <- syncCallback1 AlwaysRetain True f'
                                   fms_onEvent (toJSString eventType) func dom
