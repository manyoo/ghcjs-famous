{-# LANGUAGE JavaScriptFFI #-}
module Famous.Components.MountPoint where

import GHCJS.Foreign
import GHCJS.Types

import Famous.Core.Node
import Famous.Components.Position

data MountPoint_ a

type MountPoint a = Position (MountPoint_ a)

-- | Add a MountPoint component to a node
foreign import javascript unsafe "new window.famous.components.MountPoint($1)"
  fms_addMountPoint :: Node a -> IO (MountPoint ())

addMountPoint = fms_addMountPoint
