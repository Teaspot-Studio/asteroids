module Asteroids.Game.Rigid(
    Rigid(..)
  , HasRigid
  , stepRigids
  ) where

import Apecs
import Asteroids.Game.Shape
import Asteroids.Game.Store.Cache
import Asteroids.Game.Transform
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.Splaton
import Linear

data Rigid = Rigid {
  rigidMass      :: !Double
, rigidVelocity  :: !(V2 Double)
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

-- | Evolve positions and physics of rigid bodies
stepRigids :: (HasRigid w m, Has w m Shape, HasTrans w m, MonadIO m) => Double -> SystemT w m ()
stepRigids dt = do
  applyVelocity dt
  resolveCollisions

-- | Move all rigids according of their velocities
applyVelocity :: (HasRigid w m, HasTrans w m) => Double -> SystemT w m ()
applyVelocity dt = cmap $ \(r@Rigid{..}, Trans t) -> let
  t' = t { t2Translation = t2Translation t + fmap (dt *) rigidVelocity }
  in (r, Trans t')

-- | Solve collisions for rigid bodies
resolveCollisions :: (HasRigid w m, Has w m Shape, HasTrans w m, MonadIO m) => SystemT w m ()
resolveCollisions = cmapM_ $ \(r1, Trans t1, sh1, e1) -> cmapM_ $ \(r2, Trans t2, sh2, e2) ->
  when (e1 /= e2) $ do
    let mcollided = resolveCollision r1 t1 sh1 r2 t2 sh2
    flip traverse_ mcollided $ \(r2', t2') -> set e2 (r2', Trans t2')

-- | Resolve single collision
resolveCollision ::
     Rigid -- ^ First body
  -> T2 Double -- ^ Position
  -> Shape -- ^ Collision shape
  -> Rigid -- ^ Second body
  -> T2 Double -- ^ Second position
  -> Shape -- ^ Second shape
  -> Maybe (Rigid, T2 Double) -- ^ If collision occured, return new second body
resolveCollision r1 t1 sh1 r2 t2 sh2
  | Just v <- shapesCollision sh1 t1 sh2 t2 = let
    r3 = r2
    t3 = t2 { t2Translation = t2Translation t2 + v }
    in Just (r3, t3)
  | otherwise = Nothing
