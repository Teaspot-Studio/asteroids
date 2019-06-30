{-# LANGUAGE MultiWayIf #-}
module Asteroids.Game.Rigid(
    Rigid(..)
  , HasRigid
  , stepRigids
  , newRigid
  , rigidTransform
  ) where

import Apecs
import Asteroids.Game.Physics
import Asteroids.Game.Shape
import Asteroids.Game.Store.Cache
import Asteroids.Game.Transform
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.Splaton
import Language.Javascript.JSaddle (MonadJSM)
import Linear

import Debug.Trace

import qualified Data.Physics.Matter as MT

newtype Rigid = Rigid {
  rigidBody :: MT.Body
}

-- | Shortcut to reduce size of function signatures that uses the `Rigid` component.
type HasRigid w m = (
    Get w m Rigid
  , Set w m Rigid
  , Destroy w m Rigid
  , Members w m Rigid
  )

instance Component Rigid where
  type Storage Rigid = PCache 100 (Map Rigid)

newRigid ::(Has w m PhysicsEngine, MonadJSM m)
  => V2 Double -- ^ Position
  -> Double -- ^ Angle
  -> V2 Double -- ^ Velocity
  -> Double -- ^ Density
  -> [V2 Double] -- ^ Bounding points
  -> SystemT w m Rigid
newRigid (V2 x y) a v d vs = do
  w <- getPhysicsWorld
  b <- lift $ do
    b <- MT.bodiesFromVertecies x y vs $ Just MT.BodyOptions {
        boptsPlugin = [
          MT.WrapPlugin 0 800
        ]
      }
    MT.bodySetAngle b a
    MT.bodySetDensity b d
    MT.bodySetVelocity b v
    MT.bodySetAirFriction b 0.0
    MT.worldAdd w b
    pure b
  pure $ Rigid b

-- | Evolve positions and physics of rigid bodies
stepRigids :: (HasRigid w m, Has w m Shape, Has w m PhysicsEngine, HasTrans w m, MonadJSM m) => Double -> SystemT w m ()
stepRigids dt = do
  e <- getPhysicsEngine
  lift $ MT.engineUpdate e dt 1.0

-- | Extract position/rotation/scale from the physics body
rigidTransform :: MonadJSM m => Rigid -> m (T2 Double)
rigidTransform (Rigid b) = do
  p <- MT.bodyPosition b
  a <- MT.bodyAngle b
  pure $ T2 p (Radian a) 1.0
