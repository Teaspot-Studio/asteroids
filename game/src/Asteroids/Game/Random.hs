module Asteroids.Game.Random(
    Gen(..)
  , HasGen
  , genRandom
  ) where

import Apecs
import Linear
import System.Random

-- | Global component that tracks random number generator
newtype Gen = Gen { unGen :: StdGen }

-- | Shortcut to reduce size of function signatures that uses the `Gen` component.
type HasGen w m = (
    Get w m Gen
  , Set w m Gen
  )

instance Semigroup Gen where
  _ <> b = b

instance Monoid Gen where
  mempty = Gen $ mkStdGen 42

instance Component Gen where
  type Storage Gen = Global Gen

-- | Set seed of random generator
initRandom :: Set w m Gen => Int -> SystemT w m ()
initRandom = set global . Gen . mkStdGen

-- | Get random value in range (inclusive)
genRandom :: (HasGen w m, Random a) => a -> a -> SystemT w m a
genRandom minv maxv = do
  Gen g1 <- get global
  let (a, g2) = randomR (minv, maxv) g1
  set global (Gen g2)
  pure a

instance Random a => Random (V2 a) where
  randomR (V2 minx miny, V2 maxx maxy) g = let
    (!x, !g1) = randomR (minx, maxx) g
    (!y, !g2) = randomR (miny, maxy) g1
    in (V2 x y, g2)
  {-# INLINE randomR #-}

  random g = let
    (!x, !g1) = random g
    (!y, !g2) = random g1
    in (V2 x y, g2)
  {-# INLINE random #-}
