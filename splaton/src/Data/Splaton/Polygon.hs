{-# LANGUAGE OverloadedLists #-}
module Data.Splaton.Polygon(
    Polygon(..)
  , mapPolygon
  , polygonPoints
  , quad
  , rect
  , polygonSegments
  , bottomPoints
  , isIntersect
  , minkowskiSum
  , minkowskiDiff
  , polygonBounds
  , polygonCentroid
  , polygonIntersectRay
  , polygonIntersectSegment
  , polygonCorrectPoint
  , polygonSqueezeOutPoint
  , polygonGetColinearSegment
  , polygonClosestCorner
  , polygonSqueezeOutSegment
  , polygonSqueezeOutPolygon
  , distanceVecToPoly
  , distanceToPolygon
  , polygonDistanceBetween
  , polygonArea
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Foldable (foldl')
import Data.List (sortBy, minimumBy)
import Data.Maybe
import Data.Ord
import Data.Splaton.Angle
import Data.Splaton.Approx
import Data.Splaton.Bounding
import Data.Splaton.Ray
import Data.Splaton.Transform
import Data.Splaton.Vector
import Data.Vector.Unboxed (Vector, Unbox)
import GHC.Generics
import Linear
import Safe

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

-- | 2D convex polygon. Implemnted as points in CCW order, not including first point in end
newtype Polygon a = Polygon { unPolygon :: Vector (V2 a)}
  deriving (Eq, Ord, Show, Generic)

instance (Unbox a, Transform t, ApplyTransform t (V2 a)) => ApplyTransform t (Polygon a) where
  applyTransform t (Polygon vs) = Polygon $ V.map (applyTransform t) vs
  {-# INLINE applyTransform #-}

-- | Explicit mapping function as contents is unboxed
mapPolygon :: (Unbox a, Unbox b) => (a -> b) -> Polygon a -> Polygon b
mapPolygon f (Polygon va) = Polygon $ V.map (fmap f) va

-- | Get polygon internal points in CCW order
polygonPoints :: Polygon a -> Vector (V2 a)
polygonPoints = unPolygon

-- | Construct polygon from 4 vertecies in CCW order.
quad :: Unbox a => V2 a -> V2 a -> V2 a -> V2 a -> Polygon a
quad a b c d = Polygon [a, b, c, d]

-- | Construct polygon from start point and sizes (width and height)
rect :: (Ord a, Num a, Unbox a) => V2 a -> V2 a -> Polygon a
rect v0@(V2 v0x v0y) (V2 width height)
  | width < 0 = rect (V2 (v0x+width) v0y) (V2 (-width) height)
  | height < 0 = rect (V2 v0x (v0y+height)) (V2 width (-height))
  | otherwise = quad v0 v1 v2 v3
  where
    v1 = V2 (v0x + width) v0y
    v2 = V2 (v0x + width) (v0y + height)
    v3 = V2 v0x (v0y + height)

-- | Get CCW list of line segments for given polygon
polygonSegments :: Unbox a => Polygon a -> [(V2 a, V2 a)]
polygonSegments (Polygon vs)
  | V.null vs = []
  | otherwise = vss `zip` drop 1 vss ++ [(V.last vs, V.head vs)]
    where
      vss = V.toList vs

-- | Get CCW list of points that starts at bottom left point
bottomPoints :: forall a . (Floating a, Ord a, Unbox a) => Polygon a -> Vector (V2 a)
bottomPoints (Polygon vs)
  | V.null vs = V.empty
  | otherwise = V.drop i vs <> V.take i vs
  where
    projectDiag = rayProjectDist (ray 0 1 :: Ray2 a)
    i = V.minIndexBy (comparing projectDiag) vs

-- | Get CCW list of lline sgments for given polygon starting the bottom left point
bottomSegments :: (Ord a, Unbox a) => Polygon a -> [(V2 a, V2 a)]
bottomSegments (Polygon vs)
  | V.null vs = []
  | otherwise = vss `zip` drop 1 vss ++ [(vlast, vhead)]
  where
    i = V.minIndex vs
    vlast = if i == 0 then V.last vs else vs V.! (i-1)
    vhead = V.head $ V.drop i vs
    vss = V.toList (V.drop i vs) ++ V.toList (V.take i vs)

instance (Ord a, Num a, Unbox a) => HasPointTest (Polygon a) where
  type PointTestVec (Polygon a) = V2 a

  isPointInside v p@(Polygon vs)
    | V.length vs <= 2 = False
    | otherwise = flip all (polygonSegments p) $ \(v0, v1) -> let
      boundV = v1 - v0
      pointV = v - v0
      in crossZ boundV pointV >= 0
  {-# INLINABLE isPointInside #-}

-- | Test if first polygon itersects or fully inside of other polygon
isIntersect :: (Ord a, Num a, Unbox a) => Polygon a -> Polygon a -> Bool
isIntersect p1@(Polygon vs) p2 = V.any (`isPointInside` p2) vs

-- | Get bounding box for the polygon. For polygon without points will return
-- bounding box with zero volume.
polygonBounds :: (Ord a, Num a, Unbox a) => Polygon a -> BB2 a
polygonBounds (Polygon vs)
  | V.null vs = BB2 0 0
  | otherwise = let
    xs = V.map (\(V2 x _) -> x) vs
    ys = V.map (\(V2 _ y) -> y) vs
    minx = V.minimum xs
    miny = V.minimum ys
    maxx = V.maximum xs
    maxy = V.maximum ys
    in BB2 (V2 minx miny) (V2 maxx maxy)

instance (Ord a, Num a, Unbox a) => HasBounding (Polygon a) (BB2 a) where
  boundingOf = polygonBounds
  {-# INLINE boundingOf #-}

-- | Calculate centroid of polygon
polygonCentroid :: (Fractional a, Unbox a) => Polygon a -> V2 a
polygonCentroid (Polygon vs)
  | V.null vs = 0
  | otherwise = V.sum vs / fromIntegral (V.length vs)

-- | Calculate intersections with ray (origin + direction). Use 'Maybe' for
-- closest intersection and `[]` to get them all.
polygonIntersectRay :: (Alternative m, Monad m, Ord a, Unbox a, ApproxEq a, Floating a) => Polygon a -> Ray2 a -> m (a, V2 a)
polygonIntersectRay p rs
  | V.null (polygonPoints p) = empty
  | otherwise = foldl' (<|>) empty . fmap pure . sortBy (comparing fst) . catMaybes . fmap intersect $ polygonSegments p
  where
    intersect (v0, v1) = do
      (alpha, beta) <- rayIntersect rs (Ray2 v0 (v1 - v0))
      guard $ beta >= 0.0 && beta <= 1.0 && alpha >= 0.0
      let newv = rayPoint rs alpha
      pure (alpha, newv)

-- | Calculate intersections with ray segment (origin + direction).
polygonIntersectSegment :: (Alternative m, Monad m, Ord a, Unbox a, ApproxEq a, Floating a) => Polygon a -> Ray2 a -> m (a, V2 a)
polygonIntersectSegment p r = do
  (alpha, v) <- polygonIntersectRay p r
  guard $ alpha <= 1.0
  pure (alpha, v)

-- | Calculate closest point on line between centroid and given point that is not inside the
-- polygon (on the surface). Useful for position correction after collision.
--
-- Works even if point is outside the polygon, the result will be the projected point on closest
-- edge of the polygon.
--
-- Returns `Nothing` only when input point is equal to centroid of the polygon.
polygonCorrectPoint :: (Ord a, Unbox a, ApproxEq a, Floating a) => Polygon a -> V2 a -> Maybe (V2 a)
polygonCorrectPoint p = fmap snd . polygonIntersectRay p . ray (polygonCentroid p)

-- | If point inside the polygon, caclulate closest point on surface of the polygon.
-- Useful for position correction after collision.
--
-- Returns `Nothing` when the point is not inside the polygon. Works even when squeezing point
-- is located outside the triangle.
polygonSqueezeOutPoint :: (Ord a, Eq a, Num a, Unbox a, Floating a, ApproxEq a) => Polygon a -> V2 a -> Maybe (V2 a)
polygonSqueezeOutPoint p v
  | not (v `isPointInside` p) = Nothing
  | v == polygonCentroid p    = polygonSqueezeOutPoint p (v + 0.0001)
  | otherwise                 = polygonCorrectPoint p v

-- | Return a polygon edge that is lays on given ray and within its 0.0 .. 1.0 local coordinates.
-- The third element in result is projected interval of ray into segment.
polygonGetColinearSegment :: (Ord a, Num a, Floating a, Unbox a) => Polygon a -> Ray2 a -> Maybe (V2 a, V2 a, V2 a)
polygonGetColinearSegment p r = go $ polygonSegments p
  where
    go [] = Nothing
    go ((v0, v1) : segs) = let
      vr = Ray2 v0 (v1-v0)
      V2 i0 i1 = rayProjectInterval vr r
      inRange = (i0 >= 0.0 && i0 <= 1.0) || (i1 >= 0.0 && i1 <= 1.0)
      in if isRaysOnLine vr r && inRange then Just (v0, v1, V2 i0 i1) else go segs

-- | Get polygon closest corner point to the given position
polygonClosestCorner :: (Ord a, Floating a, Unbox a, ApproxEq a) => Polygon a -> V2 a -> Maybe (V2 a)
polygonClosestCorner p v
  | V.null (polygonPoints p) = Nothing
  | otherwise = Just . V.minimumBy (comparing $ distance v) . polygonPoints $ p

-- | If ray intersects the polygon (either origin or points between origin and direction vector end are inside),
-- returns relative translation where the ray segment doesn't intersect the polygon.
polygonSqueezeOutSegment :: forall a . (ApproxEq a, Floating a, Ord a, Eq a, Unbox a) => Polygon a -> Ray2 a -> Maybe (V2 a)
polygonSqueezeOutSegment p r = case polygonIntersectSegment p r :: [(a, V2 a)] of
  [(_, v)] -> squezeAlong v
  [(_, v1), (_, v2)] -> let
    v3 = (v1 + v2) / 2
    cornerDv = do
      cv <- polygonClosestCorner p v3
      pure $ cv - v3
    alternatives = catMaybes $ if v1 == v3
      then [squezeAlong v1, squezeAlong v2]
      else [squezeAlong v1, squezeAlong v2, cornerDv]
    in minimumByMay (comparing abs) alternatives
  _ -> fmap (\v -> v - rayOrigin r) $ polygonSqueezeOutPoint p $ rayPoint r 0.5
  where
    squezeAlong v = let
      outv = v - polygonCentroid p
      outvp = rayProject r outv
      end = if rayDirection r `dot` outv <= 0 then 1.0 else 0.0
      in Just $ v - rayPoint r end

-- | Calculate minkowski sum of two polygons. O(n+m) algorithm.
--
-- The algorithm uses merge sort for united set of points.
minkowskiSum :: (Num a, RealFloat a, Unbox a) => Polygon a -> Polygon a -> Polygon a
minkowskiSum p1 p2
  | v1s == 0 = p2
  | v2s == 0 = p1
  | otherwise = Polygon $ V.take reslen $ runST $ do
    acc <- VM.new (reslen + 1)
    VM.write acc 0 $ V.head s1 + V.head s2
    merge acc 1 (addLast s1) (addLast s2)
    V.unsafeFreeze acc
  where
    reslen = v1s + v2s
    v1s = V.length . polygonPoints $ p1
    v2s = V.length . polygonPoints $ p2
    addLast vs = V.toList $ vs `V.snoc` V.head vs
    s1 = bottomPoints p1
    s2 = bottomPoints p2
    angLess a b = angelBetween2 (V2 1 0) a <= angelBetween2 (V2 1 0) b

    merge _ _ [] [] = pure ()
    merge _ _ [] [_] = pure ()
    merge _ _ [_] [] = pure ()
    merge _ _ [_] [_] = pure ()
    merge !acc !i vss  wss = do
      let (dv, vs', ws') = case (vss, wss) of
            (v1:v2:vs, w1:w2:ws) -> if angLess (v2 - v1) (w2 - w1)
              then (v2 - v1, v2:vs, w1:w2:ws)
              else (w2 - w1, v1:v2:vs, w2:ws)
            (v1:v2:vs, _) -> (v2 - v1, v2:vs, [])
            (_, w1:w2:ws) -> (w2 - w1, [], w2:ws)
      v <- VM.read acc (i-1)
      VM.write acc i (v + dv)
      merge acc (i+1) vs' ws'

-- | Calculate minkowski difference of two polygons. See 'minkowskiSum'.
--
-- This is used for collision detection and penetration depth calculation.
minkowskiDiff :: (RealFloat a, Unbox a) => Polygon a -> Polygon a -> Polygon a
minkowskiDiff p1 p2 = minkowskiSum p1 np2
  where
    np2 = Polygon $ V.map negate $ polygonPoints p2

-- | Calculate closest point of polygons intersections and return the relative
-- translation for the second polygon.
polygonSqueezeOutPolygon :: (RealFloat a, Unbox a) => Polygon a -> Polygon a -> Maybe (V2 a)
polygonSqueezeOutPolygon p1 p2
  | not collision = Nothing
  | otherwise = Just $ negate $ distanceVecToPoly diff 0
  where
    diff = minkowskiDiff p2 p1
    collision = isPointInside 0 diff

-- | Return vector that is perpendicular to given polygon that passes through given point
distanceVecToPoly :: forall a . (Ord a, Floating a, Unbox a) => Polygon a -> V2 a -> V2 a
distanceVecToPoly p v
  | V.null (polygonPoints p) = negate v
  | otherwise = minimumBy (comparing norm) . fmap mkPerp . polygonSegments $ p
  where
    mkPerp (v1, v2) = normVecToSegment (ray v1 v2 :: Ray2 a) v

-- | Return distance to closest point of polygon
distanceToPolygon :: forall a . (Ord a, Floating a, Unbox a) => Polygon a -> V2 a -> a
distanceToPolygon p v = norm $ distanceVecToPoly p v

-- | Distance between two polygons. Returns minimal distance between any points between two polygons
polygonDistanceBetween :: (Ord a, Floating a, Unbox a, ApproxEq a) => Polygon a -> Polygon a -> a
polygonDistanceBetween p1 p2 = distance v1 v2
  where
    c1 = polygonCentroid p1
    c2 = polygonCentroid p2
    dv = ray c1 c2
    v1 = fromMaybe c1 . fmap snd $ polygonIntersectRay p1 dv
    v2 = fromMaybe c2 . fmap snd $ polygonIntersectRay p2 dv

-- | Calculate polygon area
polygonArea :: (Fractional a, Unbox a) => Polygon a -> a
polygonArea p
  | V.null (polygonPoints p) = 0
  | otherwise = (0.5 *) . sum . fmap (\(V2 x1 y1, V2 x2 y2) -> x1*y2 - x2*y1) . polygonSegments $ p
