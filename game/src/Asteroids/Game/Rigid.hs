module Asteroids.Game.Rigid(
    Rigid(..)
  , HasRigid
  , stepRigids
  ) where

import Apecs
import Asteroids.Game.Store.Cache
import Asteroids.Game.Transform
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
stepRigids :: (HasRigid w m, HasTrans w m) => Double -> SystemT w m ()
stepRigids dt = do
  cmapM $ \(r@Rigid{..}, Trans t) -> do
    let t' = t { t2Translation = t2Translation t + fmap (dt *) rigidVelocity }
    pure (r, Trans t')
