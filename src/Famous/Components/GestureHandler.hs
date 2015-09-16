{-# LANGUAGE JavaScriptFFI, OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Famous.Components.GestureHandler where

import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback

import Data.JSString
import JavaScript.Object

import Famous.Core.Basic
import Famous.Core.Node


data GestureHandler_ a

type GestureHandler a = FamoObj (GestureHandler_ a)

data GestureEvent_
type GestureEvent = FamoObj GestureEvent_

type GestureCallback = GestureEvent -> IO ()

mkCallback = syncCallback1 ContinueAsync

data GestureOption = GestureOption {
  event    :: JSString,
  callback :: GestureCallback
  }

instance ToJSRef GestureOption where
  toJSRef (GestureOption e cb) = do
    o <- create
    evName <- toJSRef e
    setProp ("event" :: JSString) evName o
    jsCb <- mkCallback cb
    setProp ("callback" :: JSString) (jsref jsCb) o
    return $ jsref o

-- | Component to manage gesture events. Will track 'pinch', 'rotate', 'tap', and 'drag' events, on an as-requested basis.
foreign import javascript unsafe "$r = new window.famous.components.GestureHandler($1, $2)"
  fms_newGestureHandler :: Node a -> JSRef -> IO (GestureHandler ())

newGestureHandler :: Node a -> [GestureOption] -> IO (GestureHandler ())
newGestureHandler n opts = do
  optObj <- toJSRef opts
  fms_newGestureHandler n optObj


-- | Register a callback to be invoked on an event.
foreign import javascript unsafe "($3).on($1, $2)"
  fms_on :: JSString -> Callback (JSRef -> IO ()) -> GestureHandler b -> IO ()

on :: JSString -> GestureCallback -> GestureHandler a -> IO ()
on e cb g = do
  jsCb <- mkCallback cb
  fms_on e jsCb g
