module Data.Physics.Matter.Body(
    Body(..)
  , BodyOptions(..)
  , encodeBodyOptions
  , bodyApplyForce
  , bodyRotate
  , bodyScale
  , bodySetAngle
  , bodySetAngularVelocity
  , bodySetDensity
  , bodySetInertia
  , bodySetMass
  , bodySetPosition
  , bodySetStatic
  , bodySetVelocity
  , bodySetAirFriction
  , bodyPosition
  , bodyAngle
  , bodyAirFriction
  , module Data.Physics.Matter.Plugin
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Foldable (foldl')
import Data.Physics.Matter.Plugin
import Data.Physics.Matter.Vector
import Data.Physics.Matter.World
import Language.Javascript.JSaddle hiding (Object)
import Linear

-- | A Matter.Body is a rigid body that can be simulated by a Matter.Engine
newtype Body = Body { unBody :: JSVal }

instance WorldAddable Body where
  worldAdd w = worldAddRaw w . unBody
  {-# INLINE worldAdd #-}

-- | Additional options for creation of body
data BodyOptions = BodyOptions {
  boptsPlugin :: [PluginOptions]
}

instance ToJSON BodyOptions where
  toJSON BodyOptions{..} = object [
      "plugin" .= Object (foldl' merge mempty $ fmap toJSON boptsPlugin)
    ]
    where
      merge !acc (Object kv) = acc <> kv
      merge !acc _ = acc
  {-# INLINE toJSON #-}

instance ToJSVal BodyOptions where
  toJSVal = toJSVal . toJSON
  {-# INLINE toJSVal #-}

-- | Encode body options in format that matter.js understands it
encodeBodyOptions :: MonadJSM m => Maybe BodyOptions -> m JSVal
encodeBodyOptions mopts = liftJSM $ case mopts of
  Nothing -> toJSVal () -- undefined
  Just opts -> toJSVal opts

-- | Applies a force to a body from a given world-space position, including resulting torque.
bodyApplyForce :: MonadJSM m
  => Body
  -> V2 Double -- ^ pos
  -> V2 Double -- ^ force
  -> m ()
{-# INLINE bodyApplyForce #-}

-- | Rotates a body by a given angle relative to its current angle, without imparting any angular velocity.
bodyRotate :: MonadJSM m
  => Body
  -> Double
  -> m ()
{-# INLINE bodyRotate #-}

-- | Scales the body, including updating physical properties (mass, area, axes, inertia), from a world-space point (default is body centre).
bodyScale :: MonadJSM m
  => Body
  -> Double -- ^ x
  -> Double -- ^ y
  -> m ()
{-# INLINE bodyScale #-}

-- | Sets the angle of the body instantly. Angular velocity, position, force etc. are unchanged.
bodySetAngle :: MonadJSM m
  => Body
  -> Double
  -> m ()
{-# INLINE bodySetAngle #-}

-- | Sets the angular velocity of the body instantly. Position, angle, force etc. are unchanged.
bodySetAngularVelocity :: MonadJSM m
  => Body
  -> Double
  -> m ()
{-# INLINE bodySetAngularVelocity #-}

-- | Sets the density of the body. Mass and inertia are automatically updated to reflect the change.
bodySetDensity :: MonadJSM m
  => Body
  -> Double
  -> m ()
{-# INLINE bodySetDensity #-}

-- | Sets the moment of inertia (i.e. second moment of area) of the body of the body. Inverse inertia is automatically updated to reflect the change. Mass is not changed.
bodySetInertia :: MonadJSM m
  => Body
  -> Double
  -> m ()
{-# INLINE bodySetInertia #-}

-- | Sets the mass of the body. Inverse mass, density and inertia are automatically updated to reflect the change.
bodySetMass :: MonadJSM m
  => Body
  -> Double
  -> m ()
{-# INLINE bodySetMass #-}

-- | Sets the position of the body instantly. Velocity, angle, force etc. are unchanged.
bodySetPosition :: MonadJSM m
  => Body
  -> V2 Double
  -> m ()
{-# INLINE bodySetPosition #-}

-- | Sets the body as static, including isStatic flag and setting mass and inertia to Infinity.
bodySetStatic :: MonadJSM m
  => Body
  -> Bool
  -> m ()
{-# INLINE bodySetStatic #-}

-- | Sets the linear velocity of the body instantly. Position, angle, force etc. are unchanged.
bodySetVelocity :: MonadJSM m
  => Body
  -> V2 Double
  -> m ()
{-# INLINE bodySetVelocity #-}

-- | A Number that defines the air friction of the body (air resistance).
-- A value of 0 means the body will never slow as it moves through space.
-- The higher the value, the faster a body slows when moving through space.
-- The effects of the value are non-linear.
bodySetAirFriction :: MonadJSM m
  => Body
  -> Double
  -> m ()
{-# INLINE bodySetAirFriction #-}

-- | Get position in world of body
bodyPosition :: MonadJSM m
  => Body
  -> m (V2 Double)
{-# INLINE bodyPosition #-}

-- | Get angle of body in radians
bodyAngle :: MonadJSM m
  => Body
  -> m Double
{-# INLINE bodyAngle #-}

-- | A Number that defines the air friction of the body (air resistance).
bodyAirFriction :: MonadJSM m
  => Body
  -> m Double
{-# INLINE bodyAirFriction #-}

#ifdef ghcjs_HOST_OS

foreign import javascript safe "Matter.Body.applyForce($1, $2, $3)"
  jsBodiesCircle :: Body -> Vec -> Vec -> IO ()

bodyApplyForce b p f = liftIO $ do
  p' <- toVec p
  f' <- toVec f
  jsBodiesCircle b p' f'

foreign import javascript safe "Matter.Body.rotate($1, $2)"
  jsBodyRotate :: Body -> Double -> IO ()

bodyRotate b r = liftIO $ jsBodyRotate b r

foreign import javascript safe "Matter.Body.slale($1, $2, $3)"
  jsBodyScale :: Body -> Double -> Double -> IO ()

bodyScale b x y = liftIO $ jsBodyScale b x y

foreign import javascript safe "Matter.Body.setAngle($1, $2)"
  jsBodySetAngle :: Body -> Double -> IO ()

bodySetAngle b r = liftIO $ jsBodySetAngle b r

foreign import javascript safe "Matter.Body.setAngularVelocity($1, $2)"
  jsBodySetAngularVelocity :: Body -> Double -> IO ()

bodySetAngularVelocity b r = liftIO $ jsBodySetAngularVelocity b r

foreign import javascript safe "Matter.Body.setDensity($1, $2)"
  jsBodySetDensity :: Body -> Double -> IO ()

bodySetDensity b r = liftIO $ jsBodySetDensity b r

foreign import javascript safe "Matter.Body.setInertia($1, $2)"
  jsBodySetInertia :: Body -> Double -> IO ()

bodySetInertia b r = liftIO $ jsBodySetInertia b r

foreign import javascript safe "Matter.Body.setMass($1, $2)"
  jsBodySetMass :: Body -> Double -> IO ()

bodySetMass b r = liftIO $ jsBodySetMass b r

foreign import javascript safe "Matter.Body.setPosition($1, $2)"
  jsBodySetPosition :: Body -> Vec -> IO ()

bodySetPosition b p = liftIO $ jsBodySetPosition b =<< toVec p

foreign import javascript safe "Matter.Body.setStatic($1, $2)"
  jsBodySetStatic :: Body -> Bool -> IO ()

bodySetStatic b v = liftIO $ jsBodySetStatic b v

foreign import javascript safe "Matter.Body.setVelocity($1, $2)"
  jsBodySetVelocity :: Body -> Vec -> IO ()

bodySetVelocity b v = liftIO $ jsBodySetVelocity b =<< toVec v

foreign import javascript safe "$1.frictionAir = $2"
  jsBodySetAirFriction :: Body -> Double -> IO ()

bodySetAirFriction b v = liftIO $ jsBodySetAirFriction b v

foreign import javascript safe "$1.position"
  jsBodyPosition :: Body -> IO Vec

bodyPosition b = liftIO $ fromVec =<< jsBodyPosition b

foreign import javascript safe "$1.angle"
  jsBodyAngle :: Body -> IO Double

bodyAngle b = liftIO $ jsBodyAngle b

foreign import javascript safe "$1.frictionAir"
  jsBodyAirFriction :: Body -> IO Double

bodyAirFriction b = liftIO $ jsBodyAirFriction b

#else

bodyApplyForce = error "bodyApplyForce: unimplemented"
bodyRotate = error "bodyRotate: unimplemented"
bodyScale = error "bodyScale: unimplemented"
bodySetAngle = error "bodySetAngle: unimplemented"
bodySetAngularVelocity = error "bodySetAngularVelocity: unimplemented"
bodySetDensity = error "bodySetDensity: unimplemented"
bodySetInertia = error "bodySetInertia: unimplemented"
bodySetMass = error "bodySetMass: unimplemented"
bodySetPosition = error "bodySetPosition: unimplemented"
bodySetStatic = error "bodySetStatic: unimplemented"
bodySetVelocity = error "bodySetVelocity: unimplemented"
bodySetAirFriction = error "bodySetAirFriction: unimplemented"
bodyPosition = error "bodyPosition: unimplemented"
bodyAngle = error "bodyAngle: unimplemented"
bodyAirFriction = error "bodyAirFriction: unimplemented"

#endif
