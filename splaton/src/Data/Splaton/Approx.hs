module Data.Splaton.Approx(
    ApproxEq(..)
  , approxEqEpsilon
  ) where

import Linear

class ApproxEq a where
  -- | Return 'True' if given values approximately equals to each other
  approxEq :: ApproxEq a => a -> a -> Bool

-- | `ApproxEq` implentation within `Epsilon` compare
approxEqEpsilon :: Epsilon a => a -> a -> Bool
approxEqEpsilon a b = nearZero $ a - b

instance ApproxEq a => ApproxEq (Maybe a) where
  approxEq Nothing Nothing = True
  approxEq (Just a) (Just b) = approxEq a b
  approxEq _ _ = False
  {-# INLINE approxEq #-}

instance Epsilon a => ApproxEq (V2 a) where
  approxEq = approxEqEpsilon
  {-# INLINE approxEq #-}

instance Epsilon a => ApproxEq (V3 a) where
  approxEq = approxEqEpsilon
  {-# INLINE approxEq #-}

instance Epsilon a => ApproxEq (V4 a) where
  approxEq = approxEqEpsilon
  {-# INLINE approxEq #-}
