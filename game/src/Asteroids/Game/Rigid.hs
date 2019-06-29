module Asteroids.Game.Rigid(
    Rigid(..)
  , HasRigid
  ) where

import Apecs
import Asteroids.Game.Store.Cache
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
