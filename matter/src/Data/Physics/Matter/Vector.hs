module Data.Physics.Matter.Vector(
    Vec(..)
  , toVec
  , fromVec
  , toVecJSON
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Language.Javascript.JSaddle
import Linear

-- | The Matter.Vector module contains methods for creating and manipulating
-- vectors. Vectors are the basis of all the geometry related operations in the
-- engine. A Matter.Vector object is of the form { x: 0, y: 0 }.
newtype Vec = Vec { unVec :: JSVal }

-- | Convert haskell vector to internal
toVec :: MonadJSM m => V2 Double -> m Vec
{-# INLINE toVec #-}

-- | Convert to haskell vector
fromVec :: MonadJSM m => Vec -> m (V2 Double)
{-# INLINE fromVec #-}

#ifdef ghcjs_HOST_OS

foreign import javascript safe "{ x: $1, y: $2 }"
  jsToVec :: Double -> Double -> IO Vec

foreign import javascript safe "$1.x"
  jsVecX :: Vec -> IO Double
foreign import javascript safe "$1.y"
  jsVecY :: Vec -> IO Double

toVec (V2 x y) = liftIO $ jsToVec x y
fromVec v = liftIO $ V2 <$> jsVecX v <*> jsVecY v

#else
toVec = error "toVec: unimplemented"
fromVec = error "fromVec: unimplemented"
#endif

-- | Encode vector as { x: 0, y: 0 }
toVecJSON :: V2 Double -> Value
toVecJSON (V2 x y) = object [ "x" .= x, "y" .= y ]
{-# INLINE toVecJSON #-}
