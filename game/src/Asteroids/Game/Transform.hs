module Asteroids.Game.Transform(
    Trans(..)
  , HasTrans
  ) where

import Apecs
import Asteroids.Game.Store.Cache
import Data.Splaton
import Linear

newtype Trans = Trans {
  unTrans :: T2 Double
}

-- | Shortcut to reduce size of function signatures that uses the `Trans` component.
type HasTrans w m = (
    Get w m Trans
  , Set w m Trans
  , Destroy w m Trans
  , Members w m Trans
  )

instance Component Trans where
  type Storage Trans = PCache 100 (Map Trans)
