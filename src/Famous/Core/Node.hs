{-# LANGUAGE JavaScriptFFI #-}
module Famous.Core.Node where

import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal

data Node_ a

type Node a = JSRef (Node_ a)


-- | APIs for a Node object

-- | show a node
foreign import javascript unsafe "($1).show()"
  fms_showNode :: Node a -> IO ()

showNode = fms_showNode

-- | hide a node
foreign import javascript unsafe "($1).hide()"
  fms_hideNode :: Node a -> IO ()

hideNode = fms_hideNode

-- | add a child node
foreign import javascript unsafe "($2).addChild($1)"
  fms_addChild :: Node a -> Node b -> IO ()

addChild = fms_addChild

-- | remove a child node
foreign import javascript unsafe "($2).removeChild($1)"
  fms_removeChild :: Node a -> Node b -> IO ()

removeChild = fms_removeChild


-- | set the align value of a node
foreign import javascript unsafe "($4).setAlign($1, $2, $3)"
  fms_setAlign :: Int -> Int -> Int -> Node a -> IO ()

setAlign = fms_setAlign

-- | Sets the mount point value of the node. Will call onMountPointChange on all of the node's components.
foreign import javascript unsafe "($4).setMountPoint($1, $2, $3)"
  fms_setMountPoint :: Int -> Int -> Int -> Node a -> IO ()

setMountPoint = fms_setMountPoint

-- | Sets the origin value of the node. Will call onOriginChange on all of the node's components.
foreign import javascript unsafe "($4).setOrigin($1, $2, $3)"
  fms_setOrigin :: Int -> Int -> Int -> Node a -> IO ()

setOrigin = fms_setOrigin

-- | Sets the position of the node. Will call onPositionChange on all of the node's components.
foreign import javascript unsafe "($4).setPosition($1, $2, $3)"
  fms_setPosition :: Int -> Int -> Int -> Node a -> IO ()

setPosition = fms_setPosition

-- | Sets the scale of the node. The default value is 1 in all dimensions.
-- The node's components will have onScaleChanged called on them.
foreign import javascript unsafe "($4).setScale($1, $2, $3)"
  fms_setScale :: Double -> Double -> Double -> Node a -> IO ()

setScale = fms_setScale

-- | Sets the value of the opacity of this node. All of the node's components will have
-- onOpacityChange called on them
foreign import javascript unsafe "($2).setOpacity($1)"
  fms_setOpacity :: Double -> Node a -> IO ()

setOpacity = fms_setOpacity

type Size = [Double]

-- | Returns the external size of the node
foreign import javascript safe "($1).getSize()"
  fms_getSize :: Node a -> JSRef b

getSize :: Node a -> IO (Maybe Size)
getSize = fromJSRef . fms_getSize


-- | Returns the current proportional size
foreign import javascript safe "($1).getProportationalSize()"
  fms_getProportionalSize :: Node a -> JSRef b

getProportionalSize :: Node a -> IO (Maybe Size)
getProportionalSize = fromJSRef . fms_getProportionalSize

-- | Returns the differential size of the node
foreign import javascript safe "($1).getDifferentialSize()"
  fms_getDifferentialSize :: Node a -> JSRef b

getDifferentialSize :: Node a -> IO (Maybe Size)
getDifferentialSize = fromJSRef . fms_getDifferentialSize

-- | Returns the absolute size of the node
foreign import javascript safe "($1).getAbsoluteSize()"
  fms_getAbsoluteSize :: Node a -> JSRef b

getAbsoluteSize :: Node a -> IO (Maybe Size)
getAbsoluteSize = fromJSRef . fms_getAbsoluteSize

-- | Returns the current Render Size of the node.
-- Note that the render size is asynchronous (will always be one frame behind)
-- and needs to be explicitely calculated by setting the proper size mode.
foreign import javascript safe "($1).getRenderSize()"
  fms_getRenderSize :: Node a -> JSRef b

getRenderSize :: Node a -> IO (Maybe Size)
getRenderSize = fromJSRef . fms_getRenderSize
