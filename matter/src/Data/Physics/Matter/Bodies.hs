module Data.Physics.Matter.Bodies(
    bodiesCircle
  , bodiesFromVertecies
  , bodiesPolygon
  , bodiesRectangle
  , bodiesTrapezoid
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Physics.Matter.Body
import Data.Physics.Matter.Vector
import Language.Javascript.JSaddle
import Linear

-- | Creates a new rigid body model with a circle hull.
bodiesCircle :: MonadJSM m
  => Double -- ^ X
  -> Double -- ^ Y
  -> Double -- ^ Radius
  -> Maybe BodyOptions -- ^ Optional options
  -> m Body
{-# INLINE bodiesCircle #-}

-- | Creates a body using the supplied vertices (or an array containing multiple sets of vertices).
bodiesFromVertecies :: MonadJSM m
  => Double -- ^ X
  -> Double -- ^ Y
  -> [V2 Double] -- ^ Verts
  -> Maybe BodyOptions -- ^ Optional options
  -> m Body
{-# INLINE bodiesFromVertecies #-}

-- | Creates a new rigid body model with a regular polygon hull with the given number of sides.
bodiesPolygon :: MonadJSM m
  => Double -- ^ X
  -> Double -- ^ Y
  -> Int -- ^ Sides
  -> Double -- ^ Radius
  -> Maybe BodyOptions -- ^ Optional options
  -> m Body
{-# INLINE bodiesPolygon #-}

-- | Creates a new rigid body model with a rectangle hull.
bodiesRectangle :: MonadJSM m
  => Double -- ^ X
  -> Double -- ^ Y
  -> Double -- ^ Width
  -> Double -- ^ Height
  -> Maybe BodyOptions -- ^ Optional options
  -> m Body
{-# INLINE bodiesRectangle #-}

-- | Creates a new rigid body model with a trapezoid hull.
bodiesTrapezoid :: MonadJSM m
  => Double -- ^ X
  -> Double -- ^ Y
  -> Double -- ^ Width
  -> Double -- ^ Height
  -> Double -- ^ Slope
  -> Maybe BodyOptions -- ^ Optional options
  -> m Body
{-# INLINE bodiesTrapezoid #-}

#ifdef ghcjs_HOST_OS

foreign import javascript safe "Matter.Bodies.circle($1, $2, $3, $4)"
  jsBodiesCircle :: Double -> Double -> Double -> JSVal -> IO Body

foreign import javascript safe "Matter.Bodies.fromVertices($1, $2, $3, $4)"
  jsBoidiesFromVertecies :: Double -> Double -> JSVal -> JSVal -> IO Body

foreign import javascript safe "Matter.Bodies.polygon($1, $2, $3, $4, $5)"
  jsBodiesPolygon :: Double -> Double -> Int -> Double -> JSVal -> IO Body

foreign import javascript safe "Matter.Bodies.rectangle($1, $2, $3, $4, $5)"
  jsBodiesRectangle :: Double -> Double -> Double -> Double -> JSVal -> IO Body

foreign import javascript safe "Matter.Bodies.trapezoid($1, $2, $3, $4, $5, $6)"
  jsBodiesTrapezoid :: Double -> Double -> Double -> Double -> Double -> JSVal -> IO Body

bodiesCircle x y r opts = liftIO $ jsBodiesCircle x y r =<< encodeBodyOptions opts
bodiesFromVertecies x y vs opts = liftIO $ do
  vs' <- toJSVal =<< traverse (fmap unVec . toVec) vs
  b <- jsBoidiesFromVertecies x y vs' =<< encodeBodyOptions opts
  when (isUndefined . unBody $ b) $ fail $ "Failed to create body " <> show (x, y, vs, opts)
  pure b
bodiesPolygon x y i r opts = liftIO $ jsBodiesPolygon x y i r =<< encodeBodyOptions opts
bodiesRectangle x y w h opts = liftIO $ jsBodiesRectangle x y w h =<< encodeBodyOptions opts
bodiesTrapezoid x y w h s opts = liftIO $ jsBodiesTrapezoid x y w h s =<< encodeBodyOptions opts

#else

bodiesCircle = error "bodiesCircle: unimplemented"
bodiesFromVertecies = error "bodiesFromVertecies: unimplemented"
bodiesPolygon = error "bodiesPolygon: unimplemented"
bodiesRectangle = error "bodiesRectangle: unimplemented"
bodiesTrapezoid = error "bodiesTrapezoid: unimplemented"

#endif
