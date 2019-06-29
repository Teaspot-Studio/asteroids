module Asteroids.Game.Shape(
    Shape(..)
  , HasShape
  ) where

import Apecs
import Asteroids.Game.Store.Cache
import Data.Splaton

newtype Shape = Shape {
  shapePolygon :: Polygon Double
}
-- | Shortcut to reduce size of function signatures that uses the `Shape` component.
type HasShape w m = (
    Get w m Shape
  , Set w m Shape
  , Destroy w m Shape
  , Members w m Shape
  )

instance Component Shape where
  type Storage Shape = PCache 100 (Map Shape)
