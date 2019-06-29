module Asteroids.Game.Shape(
    Shape(..)
  , HasShape
  , shapesCollision 
  ) where

import Apecs
import Asteroids.Game.Store.Cache
import Data.Splaton
import Linear

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

-- | Calculate shape collision and return relative translation for second shape
shapesCollision :: Shape -> T2 Double -> Shape -> T2 Double -> Maybe (V2 Double)
shapesCollision (Shape p1) t1 (Shape p2) t2 = polygonSqueezeOutPolygon
  (applyTransform t1 p1) (applyTransform t2 p2)
