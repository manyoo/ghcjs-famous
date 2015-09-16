{-# LANGUAGE JavaScriptFFI #-}
module Famous.DOM.DOMElement where


import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Foreign.Callback

import Data.Maybe

import Famous.Core.Basic
import Famous.Core.Node

data DOMElement_ a

type DOMElement a = FamoObj (DOMElement_ a)

-- | create a new DOMElement
foreign import javascript unsafe "new window.famous.domRenderables.DOMElement($1, $2)"
  fms_newDomElement :: Node a -> JSRef -> IO (DOMElement ())

newDomElement :: ToJSRef v => Node a -> Options v -> IO (DOMElement ())
newDomElement n opt = toJSRef opt >>= fms_newDomElement n

-- | set the id attribute of the DOMElement
foreign import javascript unsafe "($2).setId($1)"
  fms_setId :: JSString -> DOMElement a -> IO ()

setId :: JSString -> DOMElement a -> IO ()
setId = fms_setId

-- | add a new class to the DOM element
foreign import javascript unsafe "($2).addClass($1)"
  fms_addClass :: JSString -> DOMElement a -> IO ()

addClass :: JSString -> DOMElement a -> IO ()
addClass = fms_addClass

-- | remove a class from the DOMElement
foreign import javascript unsafe "($2).removeClass($1)"
  fms_removeClass :: JSString -> DOMElement a -> IO ()

removeClass :: JSString -> DOMElement a -> IO ()
removeClass = fms_removeClass

-- | checks if the DOMElement has the passed in class
foreign import javascript safe "($2).hasClass($1)"
  fms_hasClass :: JSString -> DOMElement a -> IO Bool

hasClass :: JSString -> DOMElement a -> IO Bool
hasClass = fms_hasClass

-- | set an attribute of the DOMElement
foreign import javascript unsafe "($3).setAttribute($1, $2)"
  fms_setAttribute :: JSString -> JSString -> DOMElement a -> IO ()

setAttribute :: JSString -> JSString -> DOMElement a -> IO ()
setAttribute = fms_setAttribute

-- | sets a CSS property
foreign import javascript unsafe "($3).setProperty($1, $2)"
  fms_setProperty :: JSString -> JSString -> DOMElement a -> IO ()

setProperty :: JSString -> JSString -> DOMElement a -> IO ()
setProperty = fms_setProperty

-- | set the content of the DOMElement. must be escaped.
foreign import javascript unsafe "($2).setContent($1)"
  fms_setContent :: JSString -> DOMElement a -> IO ()

setContent :: JSString -> DOMElement a -> IO ()
setContent = fms_setContent

-- | subscribes to a DOMElement event
foreign import javascript unsafe "($3).on($1, $2)"
  fms_onEvent :: JSString -> Callback (JSRef -> IO ()) -> DOMElement b -> IO ()

onEvent :: FromJSRef a => JSString -> (a -> IO ()) -> DOMElement b -> IO ()
onEvent eventType handler dom = do let f' x = fromJSRef x >>= handler . fromJust
                                   func <- syncCallback1 ContinueAsync f'
                                   fms_onEvent eventType func dom
