{-# LANGUAGE JavaScriptFFI #-}
module Famous.Core.Scene where

import GHCJS.Foreign
import GHCJS.Types

import Famous.Core.Node

data Scene_ a

type Scene a = Node (Scene_ a)
