module Data.Splaton.Transform(
    Transform(..)
  , ApplyTransform(..)
  , T3(..)
  , T2(..)
  ) where

import Data.Splaton.Angle
import Data.Splaton.Vector
import Linear

-- | Generic transformation type
class ( Monoid a
  , Num (TransformTranslation a)
  , Num (TransformRotation a)
  , Num (TransformScale a)
  ) => Transform a where
  -- | Associated component that descirbes translation
  type TransformTranslation a :: *
  -- | Associated component that describes rotation
  type TransformRotation a :: *
  -- | Associated component that descirbes scaling
  type TransformScale a :: *

  -- | Create transformation from translation only
  fromTranslation :: TransformTranslation a -> a
  -- | Create transformation from rotation only
  fromRotation :: TransformRotation a -> a
  -- | Create transformation from scaling only
  fromScale :: TransformScale a -> a

  -- | Get transltion component
  transformTranslation :: a -> TransformTranslation a
  -- | Get rotation component
  transformRotation :: a -> TransformRotation a
  -- | Get scaling component
  transformScale :: a -> TransformScale a

  -- | Set transltion component
  setTransformTranslation :: TransformTranslation a -> a -> a
  -- | Set rotation component
  setTransformRotation :: TransformRotation a -> a -> a
  -- | Set scaling component
  setTransformScale :: TransformScale a -> a -> a

  -- | Inverse operations in the transformation
  --
  -- ```
  -- applyTransform (reverseTransform t <> t) === id
  -- ```
  reverseTransform :: a -> a

-- | Typeclass thtat defines things that can apply transformation to themselves
class Transform t => ApplyTransform t a where
  -- | Apply full transformation
  applyTransform :: t -> a -> a
  applyTransform t = applyTranslation t . applyRotation t . applyScale t

  -- | Apply only translation
  applyTranslation :: t -> a -> a
  applyTranslation = applyTransform . (fromTranslation :: TransformTranslation t -> t) . transformTranslation
  {-# INLINE applyTranslation #-}

  -- | Apply only rotation
  applyRotation :: t -> a -> a
  applyRotation = applyTransform . (fromRotation :: TransformRotation t -> t) . transformRotation
  {-# INLINE applyRotation #-}

  -- | Apply only scale
  applyScale :: t -> a -> a
  applyScale =  applyTransform . (fromScale :: TransformScale t -> t) . transformScale
  {-# INLINE applyScale #-}

  {-# MINIMAL applyTransform | (applyScale, applyRotation, applyTranslation) #-}

-- | 3D transformation that contains translation, rotation and scale
data T3 a = T3 {
-- | Move object by given translation vector
  t3Translation :: !(V3 a)
-- | Rotate CW by given angle
, t3Rotation    :: !(Quaternion a)
-- | Scale object by given vector
, t3Scale       :: !(V3 a)
} deriving (Show)

instance RealFloat a => Semigroup (T3 a) where
  T3 t1 r1 s1 <> T3 t2 r2 s2 = T3 (t1 + t2) (r1 * r2) (s1 * s2)
  {-# INLINE (<>) #-}

-- | Helper for quaternion that behaves as no rotation. Simple 0 quaternion
-- causes problems as it not represents any rotation at all.
noRotation :: (RealFloat a, Epsilon a) => Quaternion a
noRotation = axisAngle (V3 0 0 1) 0

instance (RealFloat a, Epsilon a) => Monoid (T3 a) where
  mempty = T3 0 noRotation 1
  {-# INLINE mempty #-}

instance (RealFloat a, Epsilon a) => Transform (T3 a) where
  type TransformTranslation (T3 a) = V3 a
  type TransformRotation (T3 a) = Quaternion a
  type TransformScale (T3 a) = V3 a

  fromTranslation t = T3 t noRotation 1
  {-# INLINE fromTranslation #-}

  fromRotation t = T3 0 t 1
  {-# INLINE fromRotation #-}

  fromScale t = T3 0 noRotation t
  {-# INLINE fromScale #-}

  transformTranslation = t3Translation
  {-# INLINE transformTranslation #-}

  transformRotation = t3Rotation
  {-# INLINE transformRotation #-}

  transformScale = t3Scale
  {-# INLINE transformScale #-}

  reverseTransform (T3 t r s) = T3 (negate t) (negate r) (recip s)
  {-# INLINE reverseTransform #-}

  setTransformTranslation v t = t { t3Translation = v }
  {-# INLINE setTransformTranslation #-}

  setTransformRotation v t = t { t3Rotation = v }
  {-# INLINE setTransformRotation #-}

  setTransformScale v t = t { t3Scale = v }
  {-# INLINE setTransformScale #-}

instance (RealFloat a, Epsilon a, Conjugate a) => ApplyTransform (T3 a) (V2 a) where
  applyScale (T3 _ _ (V3 x y _)) (V2 ax ay) = V2 (x * ax) (y * ay)
  {-# INLINE applyScale #-}

  applyRotation (T3 _ q _) v = let V3 x' y' _ = frotate q (extend3D v) in V2 x' y'
  {-# INLINE applyRotation #-}

  applyTranslation (T3 (V3 x y _) _ _) (V2 ax ay) = V2 (ax + x) (ay + y)
  {-# INLINE applyTranslation #-}

instance (RealFloat a, Epsilon a, Conjugate a) => ApplyTransform (T3 a) (V3 a) where
  applyScale (T3 _ _ s) a = s * a
  {-# INLINE applyScale #-}

  applyRotation (T3 _ q _) v = frotate q v
  {-# INLINE applyRotation #-}

  applyTranslation (T3 t _ _) a = t + a
  {-# INLINE applyTranslation #-}

-- | 2D transformation that contains translation, rotation and scale
data T2 a = T2 {
-- | Move object by given translation vector
  t2Translation :: !(V2 a)
-- | Rotate CW by given angle
, t2Rotation    :: !(Radian a)
-- | Scale object by given vector
, t2Scale       :: !(V2 a)
} deriving (Show)

instance RealFloat a => Semigroup (T2 a) where
  T2 t1 r1 s1 <> T2 t2 r2 s2 = T2 (t1 + t2) (r1 * r2) (s1 * s2)
  {-# INLINE (<>) #-}

instance RealFloat a => Monoid (T2 a) where
  mempty = T2 0 0 1
  {-# INLINE mempty #-}

instance RealFloat a => Transform (T2 a) where
  type TransformTranslation (T2 a) = V2 a
  type TransformRotation (T2 a) = Radian a
  type TransformScale (T2 a) = V2 a

  fromTranslation t = T2 t 0 1
  {-# INLINE fromTranslation #-}

  fromRotation t = T2 0 t 1
  {-# INLINE fromRotation #-}

  fromScale t = T2 0 0 t
  {-# INLINE fromScale #-}

  transformTranslation = t2Translation
  {-# INLINE transformTranslation #-}

  transformRotation = t2Rotation
  {-# INLINE transformRotation #-}

  transformScale = t2Scale
  {-# INLINE transformScale #-}

  reverseTransform (T2 t r s) = T2 (negate t) (negate r) (recip s)
  {-# INLINE reverseTransform #-}

  setTransformTranslation v t = t { t2Translation = v }
  {-# INLINE setTransformTranslation #-}

  setTransformRotation v t = t { t2Rotation = v }
  {-# INLINE setTransformRotation #-}

  setTransformScale v t = t { t2Scale = v }
  {-# INLINE setTransformScale #-}

instance (RealFloat a, Conjugate a, Epsilon a) => ApplyTransform (T2 a) (V3 a) where
  applyScale (T2 _ _ (V2 sx sy)) a = (V3 sx sy 1) * a
  {-# INLINE applyScale #-}

  applyRotation (T2 _ (Radian a) _) v = frotate (axisAngle (V3 0 0 1) a) v
  {-# INLINE applyRotation #-}

  applyTranslation (T2 t _ _) a = extend3D t + a
  {-# INLINE applyTranslation #-}

instance (RealFloat a) => ApplyTransform (T2 a) (V2 a) where
  applyScale (T2 _ _ s) a = s * a
  {-# INLINE applyScale #-}

  applyRotation (T2 _ a _) v = rotate2 a v
  {-# INLINE applyRotation #-}

  applyTranslation (T2 t _ _) a = t + a
  {-# INLINE applyTranslation #-}
