{-# LANGUAGE JavaScriptFFI #-}
module Famous.Components.Position where

import GHCJS.Foreign
import GHCJS.Types

import Famous.Core.Node

data Position_ a

type Position a = JSRef (Position_ a)

-- | Add a new Position component to a node
foreign import javascript unsafe "new famous.components.Position($1)"
  fms_addPosition :: Node a -> IO (Position ())

addPosition = fms_addPosition

-- | get x translation
foreign import javascript safe "($1).getX()"
  fms_getX :: Position a -> Double

getX = fms_getX

-- | get y translation
foreign import javascript safe "($1).getY()"
  fms_getY :: Position a -> Double

getY = fms_getY

-- | get z translation
foreign import javascript safe "($1).getZ()"
  fms_getZ :: Position a -> Double

getZ = fms_getZ

-- | Whether or not the Position is currently changing
foreign import javascript safe "($1).isActive()"
  fms_isActive :: Position a -> Bool

isActive = fms_isActive

-- | Stops transition of Position component
foreign import javascript unsafe "($1).halt()"
  fms_halt :: Position a -> IO ()

halt = fms_halt
