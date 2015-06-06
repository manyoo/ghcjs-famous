{-# LANGUAGE JavaScriptFFI #-}
module Famous.Core.FamousEngine (
  FamousEngine,
  newEngine,
  famousEngine,
  engineInit,
  createScene
  ) where

import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal

import Famous.Core.Basic
import Famous.Core.Scene

-- just a data type for tagging
data Engine_

-- | FamousEngine type for the corresponding Javascript object
type FamousEngine = JSRef Engine_


-- | js API for creating new engine
foreign import javascript unsafe "new window.famous.core.FamousEngine()"
  fms_newEngine :: IO FamousEngine

newEngine = fms_newEngine

-- | js APi for getting the famousEngine
foreign import javascript safe "window.famous.core.FamousEngine"
  fms_famousEngine :: IO FamousEngine

famousEngine = fms_famousEngine

-- | An init script that initializes the FamousEngine with options or default parameters.
foreign import javascript unsafe "($1).init($2)"
  fms_engineInit :: FamousEngine -> JSRef opt -> IO ()

engineInit :: (ToJSString k, ToJSRef v) => FamousEngine -> Options k v -> IO ()
engineInit e opt = fms_engineInit e =<< toJSRef opt


-- | Creates a scene under which a scene graph could be built.
foreign import javascript unsafe "($1).createScene()"
  fms_createNewScene :: FamousEngine -> IO (Scene ())

foreign import javascript unsafe "($2).createScene($1)"
  fms_createScene :: JSString -> FamousEngine -> IO (Scene ())

createScene :: (ToJSString a) => Maybe a -> FamousEngine -> IO (Scene ())
createScene Nothing e = fms_createNewScene e
createScene (Just s) e = fms_createScene (toJSString s) e
