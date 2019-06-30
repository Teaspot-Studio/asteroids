module Asteroids.Game.Physics(
    PhysicsEngine(..)
  , HasPhysicsEngine
  , initPhysicsEngine
  , getPhysicsEngine
  , getPhysicsWorld
  ) where

import Apecs
import Language.Javascript.JSaddle (MonadJSM)
import Linear
import System.Random

import qualified Data.Physics.Matter as MT

-- | Global component that tracks random number generator
newtype PhysicsEngine = PhysicsEngine { unPhysicsEngine :: Maybe MT.Engine }

-- | Shortcut to reduce size of function signatures that uses the `PhysicsEngine` component.
type HasPhysicsEngine w m = (
    Get w m PhysicsEngine
  , Set w m PhysicsEngine
  )

instance Semigroup PhysicsEngine where
  _ <> b = b

instance Monoid PhysicsEngine where
  mempty = PhysicsEngine Nothing

instance Component PhysicsEngine where
  type Storage PhysicsEngine = Global PhysicsEngine

-- | Setup phyicis engine
initPhysicsEngine :: (Set w m PhysicsEngine, MonadJSM m) => SystemT w m ()
initPhysicsEngine = do
  e <- lift MT.engineCreate
  set global . PhysicsEngine . Just $ e

-- | Get random value in range (inclusive)
getPhysicsEngine :: (HasPhysicsEngine w m) => SystemT w m MT.Engine
getPhysicsEngine = do
  PhysicsEngine me <- get global
  maybe (fail "Physics engine wasn't initialized!") pure me

-- | Get global physics world to add new items to it
getPhysicsWorld :: (HasPhysicsEngine w m, MonadJSM m) => SystemT w m MT.World
getPhysicsWorld = do
  e <- getPhysicsEngine
  lift $ MT.engineWorld e
