module Asteroids.Game.World(
    World(..)
  , initWorld
  , fillWorld
  , stepWorld
  ) where

import Apecs
import Asteroids.Game.Asteroid
import Asteroids.Game.Material
import Asteroids.Game.Physics
import Asteroids.Game.Player
import Asteroids.Game.Random
import Asteroids.Game.Rigid
import Asteroids.Game.Shape
import Asteroids.Game.Ship
import Asteroids.Game.Transform
import Control.Monad
import Control.Monad.IO.Class
import Language.Javascript.JSaddle (JSM)

 -- | Global storage for all components. With 'r' type variable we pass extra
 -- storages for renderer extension, where frontend will store its own state.
data World r = World {
  worldEntityCounter :: !(Storage EntityCounter)
, worldRigid         :: !(Storage Rigid)
, worldShape         :: !(Storage Shape)
, worldMaterial      :: !(Storage Material)
, worldGen           :: !(Storage Gen)
, worldTrans         :: !(Storage Trans)
, worldPhysicsEngine :: !(Storage PhysicsEngine)
, worldPlayers       :: !(Storage Players)
, worldOwnedBy       :: !(Storage OwnedBy)
, worldShip          :: !(Storage Ship)
, worldExtra         :: !r
}

-- | Initialize new empty world
initWorld :: MonadIO m => r -> m (World r)
initWorld r = World
  <$> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> pure r

instance Monad m => Has (World r) m EntityCounter where
  getStore = asks worldEntityCounter
  {-# INLINE getStore #-}

instance Monad m => Has (World r) m Rigid where
  getStore = asks worldRigid
  {-# INLINE getStore #-}

instance Monad m => Has (World r) m Shape where
  getStore = asks worldShape
  {-# INLINE getStore #-}

instance Monad m => Has (World r) m Material where
  getStore = asks worldMaterial
  {-# INLINE getStore #-}

instance Monad m => Has (World r) m Gen where
  getStore = asks worldGen
  {-# INLINE getStore #-}

instance Monad m => Has (World r) m Trans where
  getStore = asks worldTrans
  {-# INLINE getStore #-}

instance Monad m => Has (World r) m PhysicsEngine where
  getStore = asks worldPhysicsEngine
  {-# INLINE getStore #-}

instance Monad m => Has (World r) m Players where
  getStore = asks worldPlayers
  {-# INLINE getStore #-}

instance Monad m => Has (World r) m OwnedBy where
  getStore = asks worldOwnedBy
  {-# INLINE getStore #-}

instance Monad m => Has (World r) m Ship where
  getStore = asks worldShip
  {-# INLINE getStore #-}

-- | Generate initial objects in world
fillWorld :: SystemT (World r) JSM ()
fillWorld = do
  initPhysicsEngine
  spawnPlayer 0
  _ <- replicateM 10 spawnAsteroid
  pure ()

-- | Calculate one simulation step
stepWorld :: Double -> SystemT (World r) JSM ()
stepWorld dt = do
  stepRigids dt
