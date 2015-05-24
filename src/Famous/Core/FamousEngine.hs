{-# LANGUAGE JavaScriptFFI #-}
module Famous.Core.FamousEngine (
  FamousEngine, newEngine, engineInit
  ) where

import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal

import Famous.Core.Basic

-- just a data type for tagging
data Engine_

-- | FamousEngine type for the corresponding Javascript object
type FamousEngine = JSRef Engine_


-- | js API for creating new engine
foreign import javascript unsafe "new FamousEngine()"
  fms_newEngine :: IO FamousEngine

newEngine = fms_newEngine


-- | An init script that initializes the FamousEngine with options or default parameters.
foreign import javascript unsafe "($1).init($2)"
  fms_engineInit :: FamousEngine -> JSRef opt -> IO ()

engineInit :: (ToJSString k, ToJSRef v) => FamousEngine -> Options k v -> IO ()
engineInit e opt = fms_engineInit e =<< toJSRef opt


