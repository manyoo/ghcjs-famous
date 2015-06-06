{-# LANGUAGE JavaScriptFFI, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Famous.Components.GestureHandler where

import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal

import Data.Text

import Famous.Core.Node


data GestureHandler_ a

type GestureHandler a = JSRef (GestureHandler_ a)

data GestureEvent_
type GestureEvent = JSRef GestureEvent_

type GestureCallback = GestureEvent -> IO ()

mkCallback = syncCallback1 AlwaysRetain True

data GestureOption = GestureOption {
  event    :: Text,
  callback :: GestureCallback
  }

instance ToJSRef GestureOption where
  toJSRef (GestureOption e cb) = do
    o <- newObj
    evName <- toJSRef e
    setProp (toJSString ("event" :: Text)) evName o
    jsCb <- mkCallback cb
    setProp (toJSString ("callback" :: Text)) jsCb o
    return o

-- | Component to manage gesture events. Will track 'pinch', 'rotate', 'tap', and 'drag' events, on an as-requested basis.
foreign import javascript unsafe "$r = new window.famous.components.GestureHandler($1, $2)"
  fms_newGestureHandler :: Node a -> JSRef b -> IO (GestureHandler ())

newGestureHandler :: Node a -> [GestureOption] -> IO (GestureHandler ())
newGestureHandler n opts = do
  optObj <- toJSRef opts
  fms_newGestureHandler n optObj


-- | Register a callback to be invoked on an event.
foreign import javascript unsafe "($3).on($1, $2)"
  fms_on :: JSString -> JSFun (JSRef a -> IO ()) -> GestureHandler b -> IO ()

on :: Text -> GestureCallback -> GestureHandler a -> IO ()
on e cb g = do
  jsCb <- mkCallback cb
  fms_on (toJSString e) jsCb g
