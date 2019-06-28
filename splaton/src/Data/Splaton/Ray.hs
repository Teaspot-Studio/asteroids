module Data.Splaton.Ray(
  -- * Ray
    Ray2(..)
  , Ray3(..)
  , ray
  , rayNorm
  , rayOrigin
  , rayDirection
  , rayPoint
  , raySegment
  , rayIntersect
  , rayIntersection
  , raySegmentsIntersection
  , isRaysOnLine
  , rayProject
  , rayProjectPoint
  , rayProjectDist
  , rayProjectInterval
  , normVecToRay
  , distanceToRay
  , normVecToSegment
  -- * 1D segments of ray
  , Interval
  , canonizeInterval
  , intersectIntervals
  , squeezeInterval
  ) where

import Control.Monad
import Data.Splaton.Approx
import Data.Splaton.Bounding
import Data.Splaton.Transform
import Data.Splaton.Vector
import Linear

-- | Ray is defined by start point and direction. Length of direction defines
-- units of local coordinates (e.x. 2.5 means point on ray in 2.5 * dirLength distance from origin).
class Ray r where
  -- | Base vector of ray
  type RayVec r :: * -> *

  -- | Make ray from two points without normalized direction. Points betwen origin
  -- and second point will be mapped to 0 .. 1 local coordinates.
  ray :: Num a => RayVec r a -> RayVec r a -> r a

  -- | Make ray from two points with normalized direction
  rayNorm :: (Floating a, Epsilon a) => RayVec r a -> RayVec r a -> r a

  -- | Get origin of ray
  rayOrigin :: r a -> RayVec r a

  -- | Get direciton of ray
  rayDirection :: r a -> RayVec r a

  -- | Calculate point across the ray on given distance in
  -- terms of direction vector units.
  rayPoint :: Num a => r a -> a -> RayVec r a

  -- | Return two points, origin and end of direction vector
  raySegment :: Fractional a => r a -> (RayVec r a, RayVec r a)

  -- | Intersect two rays, return distance in turms of direction vectors
  rayIntersect :: (Ord a, ApproxEq a, Floating a) => r a -> r a -> Maybe (a, a)

  -- | Get intersection point of rays
  rayIntersection :: (Ord a, ApproxEq a, Floating a) => r a -> r a -> Maybe (RayVec r a)

  -- | Intersect unit segments of rays
  raySegmentsIntersection :: (Ord a, ApproxEq a, Floating a) => r a -> r a -> Maybe (RayVec r a)

  -- | Check if the given rays is located on single line
  isRaysOnLine :: (Eq a, Floating a) => r a -> r a -> Bool

  -- | Project vector on ray
  rayProject :: Fractional a => r a -> RayVec r a -> RayVec r a

  -- | Project point on ray
  rayProjectPoint :: Fractional a => r a -> RayVec r a -> RayVec r a

  -- | Calculate distance on ray from origin to projection of point on ray in terms
  -- direction vector units.
  rayProjectDist :: (Floating a, Ord a) => r a -> RayVec r a -> a

  -- | Project second ray on the first and return two points in local coordinates
  -- of the first ray.
  rayProjectInterval :: (Floating a, Ord a) => r a -> r a -> Interval a

  -- | Return distance to line that ray contains
  normVecToRay :: Fractional a => r a -> RayVec r a -> RayVec r a

  -- | Return distance vec to unit segment that ray contains
  normVecToSegment :: (Floating a, Ord a) => r a -> RayVec r a -> RayVec r a

  -- | Return distance to line that ray contains
  distanceToRay :: Floating a => r a -> RayVec r a -> a

-- | 2D ray.
data Ray2 a = Ray2 !(V2 a) !(V2 a)
  deriving (Eq, Show)

-- | Bounds only origin and 1.0 point of ray
instance (Ord a, Num a) => HasBounding (Ray2 a) (BB2 a) where
  boundingOf (Ray2 v1 dv) = canonizeBox $ BB2 v1 (v1 + dv)
  {-# INLINE boundingOf #-}

instance (Transform t, ApplyTransform t (V2 a), Ord a) => ApplyTransform t (Ray2 a) where
  applyTransform t (Ray2 a b) = Ray2 (applyTransform t a) (applyTransform (setTransformTranslation 0 t) b)
  {-# INLINE applyTransform #-}

instance Ray Ray2 where
  type RayVec Ray2 = V2

  ray v1 v2 = Ray2 v1 (v2 - v1)
  {-# INLINE ray #-}

  rayNorm v1 v2 = Ray2 v1 (normalize $ v2 - v1)
  {-# INLINE rayNorm #-}

  rayOrigin (Ray2 o _) = o
  {-# INLINE rayOrigin #-}

  rayDirection (Ray2 _ d) = d
  {-# INLINE rayDirection #-}

  rayPoint r d = rayOrigin r + V2 d d * rayDirection r
  {-# INLINE rayPoint #-}

  raySegment r = (rayOrigin r, rayPoint r 1.0)
  {-# INLINE raySegment #-}

  rayIntersect a@(Ray2 v01 r1) b@(Ray2 v02 r2)
    | det `approxEq` 0.0 = if distanceToRay a v02 `approxEq` 0.0
      then let
        v02' = project r1 (v02 - v01)
        r1l = norm r1
        i1 = V2 0.0 1.0
        i20 = sign (dot r1 v02') * norm v02' / r1l
        i21 = i20 + sign (dot r1 r2) * norm r2 / r1l
        i2 = V2 i20 i21
        in case intersectIntervals i1 i2 of
              Nothing -> Nothing
              Just (V2 i30 _) -> Just (i30, rayProjectDist b $ rayPoint a i30)
      else Nothing
    | otherwise = Just (t, u)
    where
      dv = v02 - v01
      det = crossZ r1 r2
      t = crossZ dv r2 / det
      u = crossZ dv r1 / det
  {-# INLINABLE rayIntersect #-}

  rayIntersection r1 r2 = do
    (t, _) <- rayIntersect r1 r2
    pure $ rayPoint r1 t
  {-# INLINE rayIntersection #-}

  raySegmentsIntersection r1 r2 = do
    (t, u) <- rayIntersect r1 r2
    guard $ t >= 0 && t <= 1.0 && u >=0 && u <= 1.0
    pure $ rayPoint r1 t
  {-# INLINE raySegmentsIntersection #-}

  isRaysOnLine a@(Ray2 v01 r1) b@(Ray2 v02 r2) = det == 0.0 && distanceToRay a v02 == 0.0
    where
      dv = v02 - v01
      det = crossZ r1 r2
  {-# INLINE isRaysOnLine #-}

  rayProject r = project (rayDirection r)
  {-# INLINE rayProject #-}

  rayProjectPoint r v = rayOrigin r + project (rayDirection r) (v - rayOrigin r)
  {-# INLINE rayProjectPoint #-}

  rayProjectDist r v = let
    dv = v - rayOrigin r
    v' = project (rayDirection r) dv
    s = sign $ rayDirection r `dot` v'
    in s * norm v' / norm (rayDirection r)
  {-# INLINE rayProjectDist #-}

  rayProjectInterval (Ray2 v01 r1) (Ray2 v02 r2) = canonizeInterval $ V2 i20 i21
    where
      v02' = project r1 (v02 - v01)
      r1l = norm r1
      i20 = sign (dot r1 v02') * norm v02' / r1l
      i21 = i20 + sign (dot r1 r2) * norm r2 / r1l
  {-# INLINE rayProjectInterval #-}

  normVecToRay r v = let
    dv = v - rayOrigin r
    dvp = project (rayDirection r) dv
    in dvp - dv
  {-# INLINE normVecToRay #-}

  normVecToSegment r v = let
    dv = v - rayOrigin r
    dvp = project (rayDirection r) dv
    s = sign $ rayDirection r `dot` dvp
    rproj = s * norm dvp
    dv1 = rayOrigin r - v
    dv2 = rayPoint r 1.0 - v
    in if rproj >= 0 && rproj <= norm (rayDirection r)
          then dvp - dv
          else if norm dv1 <= norm dv2
            then dv1
            else dv2
  {-# INLINE normVecToSegment #-}

  distanceToRay r v = norm $ normVecToRay r v
  {-# INLINE distanceToRay #-}

-- | 3D ray.
data Ray3 a = Ray3 !(V3 a) !(V3 a)
  deriving (Eq, Show)

-- | Signum function without 0
sign :: (Ord a, Fractional a) => a -> a
sign v = if v < 0 then -1.0 else 1.0

-- | 1D interval that defines segment on line
type Interval = V2

-- | Ensure that 1D interval contains minv maxv values.
canonizeInterval :: Ord a => Interval a -> Interval a
canonizeInterval (V2 i0 i1) = V2 (min i0 i1) (max i0 i1)

-- | Calculate intersection of two intervals on single line.
--
-- First case:
-- ```
-- +------+
--     +-----+
--     +--+
-- ```
--
-- Second case:
-- ```
--    +-----+
-- +----+
--    +-+
-- ```
--
-- Third case:
-- ```
-- +-------+
--   +---+
--   +---+
-- ```
--
-- Fourth case:
-- ```
-- +----+
--        +----+
-- ```
intersectIntervals :: Ord a => Interval a -> Interval a -> Maybe (Interval a)
intersectIntervals i1 i2
  | v31 < v30 = Nothing
  | otherwise = Just $ V2 v30 v31
  where
    V2 v10 v11 = canonizeInterval i1
    V2 v20 v21 = canonizeInterval i2
    v30 = max v10 v20
    v31 = min v11 v21

-- | Move the second interval to not intersect the first interval
squeezeInterval :: (Ord a, Fractional a) => Interval a -> Interval a -> Interval a
squeezeInterval v1 v2 = case intersectIntervals v1 v2 of
  Nothing -> v2
  Just _ -> if i1c > i2c
    then V2 (i10-i2l) i10
    else V2 i11 (i11+i2l)
  where
    V2 i10 i11 = canonizeInterval v1
    V2 i20 i21 = canonizeInterval v2
    i1c = (i10 + i11) / 2
    i2c = (i20 + i21) / 2
    i2l = i21 - i20
