module Data.Physics.Matter.Bodies(
    bodiesCircle
  , bodiesFromVertecies
  , bodiesPolygon
  , bodiesRectangle
  , bodiesTrapezoid
  ) where

import Control.Monad.IO.Class
import Data.Physics.Matter.Body
import Language.Javascript.JSaddle

-- | Creates a new rigid body model with a circle hull.
bodiesCircle :: MonadJSM m
  => Double -- ^ X
  -> Double -- ^ Y
  -> Double -- ^ Radius
  -> m Body
{-# INLINE bodiesCircle #-}

-- | Creates a body using the supplied vertices (or an array containing multiple sets of vertices).
bodiesFromVertecies :: MonadJSM m
  => Double -- ^ X
  -> Double -- ^ Y
  -> [(Double, Double)] -- ^ Verts
  -> m Body
{-# INLINE bodiesFromVertecies #-}

-- | Creates a new rigid body model with a regular polygon hull with the given number of sides.
bodiesPolygon :: MonadJSM m
  => Double -- ^ X
  -> Double -- ^ Y
  -> Int -- ^ Sides
  -> Double -- ^ Radius
  -> m Body
{-# INLINE bodiesPolygon #-}

-- | Creates a new rigid body model with a rectangle hull.
bodiesRectangle :: MonadJSM m
  => Double -- ^ X
  -> Double -- ^ Y
  -> Double -- ^ Width
  -> Double -- ^ Height
  -> m Body
{-# INLINE bodiesRectangle #-}

-- | Creates a new rigid body model with a trapezoid hull.
bodiesTrapezoid :: MonadJSM m
  => Double -- ^ X
  -> Double -- ^ Y
  -> Double -- ^ Width
  -> Double -- ^ Height
  -> Double -- ^ Slope
  -> m Body
{-# INLINE bodiesTrapezoid #-}

#ifdef ghcjs_HOST_OS

foreign import javascript safe "Matter.Bodies.circle($1, $2, $3)"
  jsBodiesCircle :: Double -> Double -> Double -> IO Body

foreign import javascript safe "Matter.Bodies.fromVertices($1, $2, $3)"
  jsBoidiesFromVertecies :: Double -> Double -> JSVal -> IO Body

foreign import javascript safe "Matter.Bodies.polygon($1, $2, $3, $4)"
  jsBodiesPolygon :: Double -> Double -> Int -> Double -> IO Body

foreign import javascript safe "Matter.Bodies.rectangle($1, $2, $3, $4)"
  jsBodiesRectangle :: Double -> Double -> Double -> Double -> IO Body

foreign import javascript safe "Matter.Bodies.trapezoid($1, $2, $3, $4, $5)"
  jsBodiesTrapezoid :: Double -> Double -> Double -> Double -> Double -> IO Body

bodiesCircle x y r = liftIO $ jsBodiesCircle x y r
bodiesFromVertecies x y vs = liftIO $ jsBoidiesFromVertecies x y =<< toJSVal vs
bodiesPolygon x y i r = liftIO $ jsBodiesPolygon x y i r
bodiesRectangle x y w h = liftIO $ jsBodiesRectangle x y w h
bodiesTrapezoid x y w h s = liftIO $ jsBodiesTrapezoid x y w h s

#else

bodiesCircle = error "bodiesCircle: unimplemented"
bodiesFromVertecies = error "bodiesFromVertecies: unimplemented"
bodiesPolygon = error "bodiesPolygon: unimplemented"
bodiesRectangle = error "bodiesRectangle: unimplemented"
bodiesTrapezoid = error "bodiesTrapezoid: unimplemented"

#endif
