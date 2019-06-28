module Data.Splaton.Angle(
    Radian(..)
  , RadianF
  , angelBetween2
  , angelBetweenSigned
  , Degree(..)
  , DegreeF
  , Angle(..)
  , ang2vec
  , vec2ang
  , angLerp
  , rotate2
  , frotate
  ) where

import Data.Splaton.Approx
import Linear

-- | Strictly typed wrapper for radian angle
newtype Radian a = Radian { unRadian :: a }
  deriving (Enum, Eq, Floating, Fractional, Epsilon, Num, Ord, Real, RealFrac, Show)

-- | Shortcut for floating number variation
type RadianF = Radian Double

-- | Angel between two vectors in [0, 2*Pi) range
angelBetween2 :: (Ord a, Num a, Floating a, RealFloat a) => V2 a -> V2 a -> Radian a
angelBetween2 v1 v2
  | a < 0 = Radian $ a + 2*pi
  | otherwise = Radian a
  where
    Radian a = angelBetweenSigned v1 v2

-- | Angel between two vectors in [-Pi, Pi) range
angelBetweenSigned :: (RealFloat a) => V2 a -> V2 a -> Radian a
angelBetweenSigned (V2 x1 y1) (V2 x2 y2) = Radian $ atan2 det dotp
  where
    det = x1*y2 - y1*x2
    dotp = x1*x2 + y1*y2

-- | Strictly typed wrapper for degree angle
newtype Degree a = Degree { unDegree :: a }
  deriving (Enum, Eq, Floating, Fractional, Epsilon, Num, Ord, Real, RealFrac, Show)

-- | Shortcut for floating number variation
type DegreeF = Degree Double

-- | Generic angle type
class Angle ang where
  -- | Convert to radian angle
  toRadian :: Floating a => ang a -> Radian a

  -- | Convert from radian angle
  fromRadian :: Floating a => Radian a -> ang a

  -- | Convert to degree angle
  toDegree :: Floating a => ang a -> Degree a
  toDegree = toDegree . toRadian
  {-# INLINE toDegree #-}

  -- | Convert from degree angle
  fromDegree :: Floating a => Degree a -> ang a
  fromDegree = fromRadian . toRadian
  {-# INLINE fromDegree #-}

instance Angle Radian where
  toRadian = id
  {-# INLINE toRadian #-}
  fromRadian = id
  {-# INLINE fromRadian #-}
  toDegree (Radian v) = Degree (180.0 / pi * v)
  {-# INLINE toDegree #-}

instance Angle Degree where
  toRadian (Degree v) = Radian (pi / 180.0 * v)
  {-# INLINE toRadian #-}
  fromRadian (Radian v) = Degree (180.0 / pi * v)
  {-# INLINE fromRadian #-}
  toDegree = id
  {-# INLINE toDegree #-}
  fromDegree = id
  {-# INLINE fromDegree #-}

instance Epsilon a => ApproxEq (Radian a) where
  approxEq = approxEqEpsilon
  {-# INLINE approxEq #-}
instance Epsilon a => ApproxEq (Degree a) where
  approxEq = approxEqEpsilon
  {-# INLINE approxEq #-}

-- | Convert angle to unit vector
ang2vec :: (Floating a, Angle ang) => ang a -> V2 a
ang2vec = (\(Radian v) -> V2 (cos v) (sin v)) . toRadian
{-# INLINE ang2vec #-}

-- | Convert unit vector to angle
vec2ang :: (Floating a, Angle ang) => V2 a -> ang a
vec2ang (V2 x _) = fromRadian $ Radian $ acos x
{-# INLINE vec2ang #-}

-- | Linear interpolation of 2D angles
angLerp :: (Floating a, Epsilon a, Angle ang) => a -> ang a -> ang a -> ang a
angLerp t a b = vec2ang $ normalize $ lerp t (ang2vec b) (ang2vec a)
{-# INLINE angLerp #-}

-- | 2D rotation of vector
rotate2 :: (RealFloat a, Angle ang) => ang a -> V2 a -> V2 a
rotate2 ang (V2 x y) = V2 (cos a * x - sin a * y) (sin a * x + cos a * y)
  where
    Radian a = toRadian ang
{-# INLINE rotate2 #-}

-- | Apply a rotation to a vector. It is make no allocations, but `rotate` from
-- linear does.
frotate :: (Conjugate a, RealFloat a) => Quaternion a -> V3 a -> V3 a
frotate q v = ijk where
  Quaternion _ ijk = q * Quaternion 0 v * conjugate q
