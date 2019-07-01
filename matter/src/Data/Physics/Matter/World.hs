module Data.Physics.Matter.World(
    World(..)
  , worldAddRaw
  , worldRemoveRaw
  , WorldAddable(..)
  , WorldRemovable(..)
  ) where

import Control.Monad.IO.Class
import Language.Javascript.JSaddle

-- | A Matter.World is a Matter.Composite body, which is a collection of Matter.Body, Matter.Constraint and other Matter.Composite.
newtype World = World { unWorld :: JSVal }

-- | Add an object to world. Helper for `WorldAddable` typeclass.
worldAddRaw :: MonadJSM m => World -> JSVal -> m ()
{-# INLINE worldAddRaw #-}

-- | Remove an object from world. Helper for `WorldRemovable` typeclass.
worldRemoveRaw :: MonadJSM m => World -> JSVal -> m ()
{-# INLINE worldRemoveRaw #-}

class WorldAddable a where
  -- | Add some object to physics world
  worldAdd :: MonadJSM m => World -> a -> m ()

instance (WorldAddable a, ToJSVal a) => WorldAddable [a] where
  worldAdd w as = worldAddRaw w =<< liftJSM (toJSVal as)
  {-# INLINE worldAdd #-}

class WorldRemovable a where
  -- | Add some object from physics world
  worldRemove :: MonadJSM m => World -> a -> m ()

instance (WorldRemovable a, ToJSVal a) => WorldRemovable [a] where
  worldRemove w as = worldRemoveRaw w =<< liftJSM (toJSVal as)
  {-# INLINE worldRemove #-}

#ifdef ghcjs_HOST_OS

foreign import javascript safe "Matter.World.add($1, $2)"
  jsWorldAdd :: World -> JSVal -> IO ()

worldAddRaw w v = liftIO $ jsWorldAdd w v

foreign import javascript safe "Matter.World.remove($1, $2)"
  jsWorldRemove :: World -> JSVal -> IO ()

worldRemoveRaw w v = liftIO $ jsWorldRemove w v

#else

worldAddRaw = error "worldAddRaw: unimplemented"
worldRemoveRaw = error "worldRemoveRaw: unimplemented"

#endif
