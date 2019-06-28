module Asteroids.Game.Rigid(
    Rigid(..)
  ) where

import Apecs
import Linear
import Data.Splaton

data Rigid = Rigid {
  rigidMass      :: !Double
, rigidTransform :: !(T2 Double)
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
  type Storage Rigid = Cache 100 (Map Rigid)
