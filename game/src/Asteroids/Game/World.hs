module Asteroids.Game.World(
    World(..)
  , initWorld
  , fillWorld
  , stepWorld
  ) where

import Apecs
import Asteroids.Game.Asteroid
import Asteroids.Game.Material
import Asteroids.Game.Random
import Asteroids.Game.Rigid
import Asteroids.Game.Shape
import Control.Monad
import Control.Monad.IO.Class

 -- | Global storage for all components
data World = World {
  worldEntityCounter :: !(Storage EntityCounter)
, worldRigid         :: !(Storage Rigid)
, worldShape         :: !(Storage Shape)
, worldMaterial      :: !(Storage Material)
, worldGen           :: !(Storage Gen)
}

-- | Initialize new empty world
initWorld :: MonadIO m => m World
initWorld = World
  <$> explInit
  <*> explInit
  <*> explInit
  <*> explInit
  <*> explInit

instance Monad m => Has World m EntityCounter where
  getStore = asks worldEntityCounter
  {-# INLINE getStore #-}

instance Monad m => Has World m Rigid where
  getStore = asks worldRigid
  {-# INLINE getStore #-}

instance Monad m => Has World m Shape where
  getStore = asks worldShape
  {-# INLINE getStore #-}

instance Monad m => Has World m Material where
  getStore = asks worldMaterial
  {-# INLINE getStore #-}

instance Monad m => Has World m Gen where
  getStore = asks worldGen
  {-# INLINE getStore #-}

-- | Generate initial objects in world
fillWorld :: SystemT World IO ()
fillWorld = do
  _ <- replicateM 10 spawnAsteroid
  pure ()

-- | Calculate one simulation step
stepWorld :: Double -> SystemT World IO ()
stepWorld dt = pure ()
