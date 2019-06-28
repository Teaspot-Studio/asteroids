module Data.Splaton.Bounding(
    BoundingBox(..)
  , HasBounding(..)
  , BB2(..)
  , BB3(..)
  ) where

import Data.Splaton.Transform
import Linear

-- | Generic operations with bounding boxes
class BoundingBox bb where
  -- | Vector type of bounding box points
  type BoundingBoxVec bb :: * -> *

  -- | Transform min and max of bounding box to preserve invariant min <= max
  canonizeBox :: Ord a => bb a -> bb a

  -- | Return 'True' if given bounds share space
  boundingIntersects :: (Ord a, Fractional a) => bb a -> bb a -> Bool

  -- | Move bounding box by given vector
  boundingTranslate :: Num a => BoundingBoxVec bb a -> bb a -> bb a

  -- | Uniformly scale bounding box
  scaleBounds :: Num a => a -> bb a -> bb a

  -- | Uniformly scale bounding box around its center
  resizeBounds :: Fractional a => a -> bb a -> bb a

  -- | Test whether the point is inside the bounding box
  boundsIsPointInside :: Ord a => BoundingBoxVec bb a -> bb a -> Bool

  -- | Get width of bounding box
  boundingWidth :: Num a => bb a -> a

  -- | Get height of bounding box
  boundingHeight :: Num a => bb a -> a

-- | Define that `a` has defined bounding box
class HasBounding a b where
  -- | Return minimal area bounding box of the value
  boundingOf :: a -> b

-- | Two dimension bounding box
data BB2 a = BB2 !(V2 a) !(V2 a)
  deriving (Eq, Show)

instance BoundingBox BB2 where
  type BoundingBoxVec BB2 = V2

  canonizeBox (BB2 (V2 minx miny) (V2 maxx maxy)) = BB2
    (V2 (min minx maxx) (min miny maxy))
    (V2 (max minx maxx) (max miny maxy))
  {-# INLINE canonizeBox #-}

  boundingIntersects (BB2 (V2 minx1 miny1) (V2 maxx1 maxy1)) (BB2 (V2 minx2 miny2) (V2 maxx2 maxy2)) =
       abs (cx1 - cx2) * 2 < width1 + width2
    && abs (cy1 - cy2) * 2 < height1 + height2
    where
      cx1 = (maxx1 + minx1) / 2
      cy1 = (maxy1 + miny1) / 2
      cx2 = (maxx2 + minx2) / 2
      cy2 = (maxy2 + miny2) / 2
      width1 = maxx1 - minx1
      width2 = maxx2 - minx2
      height1 = maxy1 - miny1
      height2 = maxy2 - miny2
  {-# INLINE boundingIntersects #-}

  boundingTranslate v (BB2 v1 v2) = BB2 (v1+v) (v2+v)
  {-# INLINE boundingTranslate #-}

  scaleBounds v (BB2 v1 v2) = BB2 (fmap (v*) v1) (fmap (v*) v2)
  {-# INLINE scaleBounds #-}

  resizeBounds v (BB2 v1 v2) = BB2 (c - s') (c + s')
    where
      c = (v1 + v2) / 2
      s = (v2 - v1) / 2
      s' = fmap (v*) s
  {-# INLINE resizeBounds #-}

  boundsIsPointInside (V2 x y) (BB2 (V2 minx miny) (V2 maxx maxy)) =
    x >= minx && x <= maxx && y >= miny && y <= maxy
  {-# INLINE boundsIsPointInside #-}

  boundingWidth (BB2 (V2 minx _) (V2 maxx _)) = maxx - minx
  {-# INLINE boundingWidth #-}

  boundingHeight (BB2 (V2 _ miny) (V2 _ maxy)) = maxy - miny
  {-# INLINE boundingHeight #-}

instance (Transform t, ApplyTransform t (V2 a), Ord a) => ApplyTransform t (BB2 a) where
  applyTransform t (BB2 (V2 minx miny) (V2 maxx maxy)) = BB2 (V2 minx' miny') (V2 maxx' maxy')
    where
      V2 p1x p1y = applyTransform t $ V2 minx miny
      V2 p2x p2y = applyTransform t $ V2 maxx miny
      V2 p3x p3y = applyTransform t $ V2 maxx maxy
      V2 p4x p4y = applyTransform t $ V2 minx maxy
      xs = [p1x, p2x, p3x, p4x]
      ys = [p1y, p2y, p3y, p4y]
      minx' = minimum xs
      miny' = minimum ys
      maxx' = maximum xs
      maxy' = maximum ys
  {-# INLINE applyTransform #-}

-- | Three dimension bounding box
data BB3 a = BB3 !(V3 a) !(V3 a)
  deriving (Eq, Show)

instance BoundingBox BB3 where
  type BoundingBoxVec BB3 = V3

  canonizeBox (BB3 (V3 minx miny minz) (V3 maxx maxy maxz)) = BB3
    (V3 (min minx maxx) (min miny maxy) (min minz maxz))
    (V3 (max minx maxx) (max miny maxy) (max minz maxz))
  {-# INLINE canonizeBox #-}

  boundingIntersects (BB3 (V3 minx1 miny1 minz1) (V3 maxx1 maxy1 maxz1)) (BB3 (V3 minx2 miny2 minz2) (V3 maxx2 maxy2 maxz2)) =
       abs (cx1 - cx2) * 2 < width1 + width2
    && abs (cy1 - cy2) * 2 < height1 + height2
    && abs (cz1 - cz2) * 2 < depth1 + depth2
    where
      cx1 = (maxx1 + minx1) / 2
      cy1 = (maxy1 + miny1) / 2
      cz1 = (maxz1 + minz1) / 2
      cx2 = (maxx2 + minx2) / 2
      cy2 = (maxy2 + miny2) / 2
      cz2 = (maxz2 + minz2) / 2
      width1 = maxx1 - minx1
      width2 = maxx2 - minx2
      height1 = maxy1 - miny1
      height2 = maxy2 - miny2
      depth1 = maxy1 - miny1
      depth2 = maxy2 - miny2
  {-# INLINE boundingIntersects #-}

  boundingTranslate v (BB3 v1 v2) = BB3 (v1+v) (v2+v)
  {-# INLINE boundingTranslate #-}

  scaleBounds v (BB3 v1 v2) = BB3 (fmap (v*) v1) (fmap (v*) v2)
  {-# INLINE scaleBounds #-}

  resizeBounds v (BB3 v1 v2) = BB3 (c - s') (c + s')
    where
      c = (v1 + v2) / 2
      s = (v2 - v1) / 2
      s' = fmap (v*) s
  {-# INLINE resizeBounds #-}

  boundsIsPointInside (V3 x y z) (BB3 (V3 minx miny minz) (V3 maxx maxy maxz)) =
    x >= minx && x <= maxx && y >= miny && y <= maxy && z >= minz && z <= maxz
  {-# INLINE boundsIsPointInside #-}

  boundingWidth (BB3 (V3 minx _ _) (V3 maxx _ _)) = maxx - minx
  {-# INLINE boundingWidth #-}

  boundingHeight (BB3 (V3 _ miny _) (V3 _ maxy _)) = maxy - miny
  {-# INLINE boundingHeight #-}

instance (Transform t, ApplyTransform t (V3 a), Ord a) => ApplyTransform t (BB3 a) where
  applyTransform t (BB3 (V3 minx miny minz) (V3 maxx maxy maxz)) = BB3 (V3 minx' miny' minz') (V3 maxx' maxy' maxz')
    where
      V3 p1x p1y p1z = applyTransform t $ V3 minx miny minz
      V3 p2x p2y p2z = applyTransform t $ V3 maxx miny minz
      V3 p3x p3y p3z = applyTransform t $ V3 maxx maxy minz
      V3 p4x p4y p4z = applyTransform t $ V3 minx maxy minz
      V3 p5x p5y p5z = applyTransform t $ V3 minx miny minz
      V3 p6x p6y p6z = applyTransform t $ V3 maxx miny maxz
      V3 p7x p7y p7z = applyTransform t $ V3 maxx maxy maxz
      V3 p8x p8y p8z = applyTransform t $ V3 minx maxy maxz
      xs = [p1x, p2x, p3x, p4x, p5x, p6x, p7x, p8x]
      ys = [p1y, p2y, p3y, p4y, p5y, p6y, p7y, p8y]
      zs = [p1z, p2z, p3z, p4z, p5z, p6z, p7z, p8z]
      minx' = minimum xs
      miny' = minimum ys
      minz' = minimum zs
      maxx' = maximum xs
      maxy' = maximum ys
      maxz' = maximum zs
  {-# INLINE applyTransform #-}
