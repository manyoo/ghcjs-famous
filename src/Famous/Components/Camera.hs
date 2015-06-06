{-# LANGUAGE JavaScriptFFI #-}
module Famous.Components.Camera where

import GHCJS.Foreign
import GHCJS.Types

import Famous.Core.Node

data Camera_ a

type Camera a = JSRef (Camera_ a)

-- | Camera is a component that is responsible for sending information
--   to the renderer about where the camera is in the scene. This allows
--   the user to set the type of projection, the focal depth, and other
--   properties to adjust the way the scenes are rendered.

foreign import javascript unsafe "new window.famous.components.Camera($1)"
  fms_addCamera :: Node a -> IO (Camera ())

addCamera = fms_addCamera

type CameraType = Double
type CameraDepth = Double
type CameraNear = Double
type CameraFar = Double

-- | Set the internals of the camera
foreign import javascript unsafe "($5).set($1, $2, $3, $4)"
  fms_cameraSet :: Double -> Double -> Double -> Double -> Camera a -> IO ()

cameraSet :: CameraType -> CameraDepth -> CameraNear -> CameraFar -> Camera a -> IO ()
cameraSet = fms_cameraSet

-- | set the camera depth for perspective projection
foreign import javascript unsafe "($2).setDepth($1)"
  fms_setDepth :: CameraDepth -> Camera a -> IO ()

setDepth = fms_setDepth

-- | set the camera's near and far frustum
foreign import javascript unsafe "($3).setFrustum($1, $2)"
  fms_setFrustum :: CameraNear -> CameraFar -> Camera a -> IO ()

setFrustum = fms_setFrustum

-- | Set the Camera to have orthographic projection
foreign import javascript unsafe "($1).setFlat()"
  fms_setFlat :: Camera a -> IO ()

setFlat = fms_setFlat
