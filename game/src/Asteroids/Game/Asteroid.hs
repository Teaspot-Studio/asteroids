module Asteroids.Game.Asteroid(
    spawnAsteroid
  , HasAsteroid
  ) where

import Apecs
import Asteroids.Game.Material
import Asteroids.Game.Physics
import Asteroids.Game.Random
import Asteroids.Game.Rigid
import Asteroids.Game.Shape
import Asteroids.Game.Transform
import Control.Monad.IO.Class
import Data.Splaton
import Language.Javascript.JSaddle (MonadJSM)
import Linear

import qualified Data.Vector.Unboxed as V

type HasAsteroid w m = (
    Has w m Shape
  , Has w m Rigid
  , Has w m Material
  , HasPhysicsEngine w m
  )

spawnAsteroid :: (HasGen w m, HasAsteroid w m, Has w m EntityCounter, MonadJSM m) => SystemT w m Entity
spawnAsteroid = do
  shape <- genAsteroidShape
  rigid <- genAsteroidRigid shape
  newEntity (shape, rigid, mat)
  where
    mat = Material {
        materialFill = 0xc4c4c4
      , materialFillAlpha = 1.0
      , materialLine = 0x7a7777
      , materialLineWidth = 10
      , materialLineAlpha = 0.5
      }

genAsteroidShape :: HasGen w m => SystemT w m Shape
genAsteroidShape = do
  n :: Int <- genRandom 3 10
  let da = 2 * pi / fromIntegral n
  ps <- flip traverse [0 .. n-1] $ \i -> do
    d :: Double <- genRandom 10 75
    let angle = Radian $ fromIntegral i * da
        va = ang2vec angle
    pure $ (d *) <$> va
  pure $ Shape . Polygon . V.fromList $ ps

genAsteroidRigid :: (HasGen w m, HasPhysicsEngine w m, MonadJSM m) => Shape -> SystemT w m Rigid
genAsteroidRigid (Shape p) = do
  let dense = 10
  t <- genRandom (V2 0 0) (V2 800 800)
  r <- genRandom 0 (2*pi)
  v <- genRandom (-3) 3
  newRigid t r v dense $ V.toList $ polygonPoints p
