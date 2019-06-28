module Data.Splaton.Vector(
    V2F
  , V3F
  , V4F
  , v2
  , v3
  , v4
  , extend3D
  , HasPointTest(..)
  ) where

import Linear

-- | 2D vector with floating point coords
type V2F = V2 Double
-- | 3D vector with floating point coords
type V3F = V3 Double
-- | 4D vector with floating point coords
type V4F = V4 Double

-- | Make vector with equal elements
v2 :: a -> V2 a
v2 a = V2 a a

-- | Make vector with equal elements
v3 :: a -> V3 a
v3 a = V3 a a a

-- | Make vector with equal elements
v4 :: a -> V4 a
v4 a = V4 a a a a

-- | Helper to extend to 3D space
extend3D :: Num a => V2 a -> V3 a
extend3D (V2 x y) = V3 x y 0

-- | Type that can be tested if point is inside
class HasPointTest a where
  type PointTestVec a :: *

  isPointInside :: PointTestVec a -> a -> Bool
