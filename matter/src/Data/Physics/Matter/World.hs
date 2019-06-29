module Data.Physics.Matter.World(
    World(..)
  , worldAddRaw
  , WorldAddable(..)
  ) where

import Control.Monad.IO.Class
import Language.Javascript.JSaddle

-- | A Matter.World is a Matter.Composite body, which is a collection of Matter.Body, Matter.Constraint and other Matter.Composite.
newtype World = World { unWorld :: JSVal }

-- | Add an object to world. Helper for `WorldAddable` typeclass.
worldAddRaw :: MonadJSM m => World -> JSVal -> m ()
{-# INLINE worldAddRaw #-}

class WorldAddable a where
  -- | Add some object to physics world
  worldAdd :: MonadJSM m => World -> a -> m ()

instance (WorldAddable a, ToJSVal a) => WorldAddable [a] where
  worldAdd w as = worldAddRaw w =<< liftJSM (toJSVal as)
  {-# INLINE worldAdd #-}

#ifdef ghcjs_HOST_OS

foreign import javascript safe "Matter.World.add($1, $2)"
  jsWorldAdd :: World -> JSVal -> IO ()

worldAddRaw w v = liftIO $ jsWorldAdd w v

#else

worldAddRaw = error "worldAddRaw: unimplemented"

#endif
