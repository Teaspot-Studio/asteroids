module Asteroids.Game.Material(
    Material(..)
  , HasMaterial
  , Color
  ) where

import Apecs
import Asteroids.Game.Store.Cache

type Color = Int

-- | Visual properties of entity
data Material = Material {
  materialFill      :: !Color
, materialFillAlpha :: !Double
, materialLine      :: !Color
, materialLineWidth :: !Double
, materialLineAlpha :: !Double
} deriving (Show)

-- | Shortcut to reduce size of function signatures that uses the `Material` component.
type HasMaterial w m = (
    Get w m Material
  , Set w m Material
  , Destroy w m Material
  , Members w m Material
  )

instance Component Material where
  type Storage Material = PCache 100 (Map Material)
