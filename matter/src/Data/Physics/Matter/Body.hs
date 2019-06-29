module Data.Physics.Matter.Body(
    Body(..)
  ) where

import Control.Monad.IO.Class
import Data.Physics.Matter.World
import Language.Javascript.JSaddle

-- | A Matter.Body is a rigid body that can be simulated by a Matter.Engine
newtype Body = Body { unBody :: JSVal }

instance WorldAddable Body where
  worldAdd w = worldAddRaw w . unBody
  {-# INLINE worldAdd #-}

#ifdef ghcjs_HOST_OS

#else

#endif
