module Asteroids.Game.Asteroid(
    spawnAsteroid
  , HasAsteroid
  ) where

import Apecs
import Asteroids.Game.Material
import Asteroids.Game.Random
import Asteroids.Game.Rigid
import Asteroids.Game.Shape
import Asteroids.Game.Transform
import Control.Monad.IO.Class
import Data.Splaton
import Linear

import qualified Data.Vector.Unboxed as V

type HasAsteroid w m = (
    Has w m Shape
  , Has w m Rigid
  , Has w m Material
  , Has w m Trans
  )

spawnAsteroid :: (HasGen w m, HasAsteroid w m, Has w m EntityCounter, MonadIO m) => SystemT w m Entity
spawnAsteroid = do
  shape <- genAsteroidShape
  rigid <- genAsteroidRigid shape
  trans <- genAsteroidTransform
  newEntity (shape, rigid, trans, mat)
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

genAsteroidRigid :: HasGen w m => Shape -> SystemT w m Rigid
genAsteroidRigid (Shape p) = do
  let area = polygonArea p
      dense = 10
      mass = area * dense
  v <- genRandom (-10) 10
  pure Rigid {
      rigidMass = mass
    , rigidVelocity = v
    }

genAsteroidTransform :: HasGen w m => SystemT w m Trans
genAsteroidTransform = do
  t <- genRandom (V2 0 0) (V2 800 800)
  r <- genRandom 0 (2*pi)
  pure $ Trans $ T2 t (Radian r) 1.0
