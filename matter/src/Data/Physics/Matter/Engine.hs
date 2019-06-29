module Data.Physics.Matter.Engine(
    Engine(..)
  , engineCreate
  , engineWorld
  , engineUpdate
  ) where

import Control.Monad.IO.Class
import Data.Physics.Matter.World
import Language.Javascript.JSaddle

-- | Reference to js engine object
newtype Engine = Engine { unEngine :: JSVal }

-- | Allocate new matter engine
engineCreate :: MonadJSM m => m Engine
{-# INLINE engineCreate #-}

-- | Get world object from engine
engineWorld :: MonadJSM m => Engine -> m World
{-# INLINE engineWorld #-}

-- | Moves the simulation forward in time by delta ms.
engineUpdate :: MonadJSM m => Engine
  -> Double -- ^ Delta (default 16.666) ms
  -> Double -- ^ Correction (default 1.0)
  -> m ()
{-# INLINE engineUpdate #-}

#ifdef ghcjs_HOST_OS

foreign import javascript safe "Matter.Engine.create()"
  jsEngineCreate :: IO Engine

foreign import javascript safe "$1.world"
  jsEngineWorld :: Engine -> IO World

foreign import javascript safe "Matter.Engine.update($1, $2, $3)"
  jsEngineUpdate :: Engine -> Double -> Double -> IO ()

engineCreate = liftIO $ jsEngineCreate
engineWorld e = liftIO $ jsEngineWorld e
engineUpdate e dt c = liftIO $ jsEngineUpdate e dt c

#else

engineCreate = error "engineCreate: unimplemented"
engineWorld = error "engineWorld: unimplemented"
engineUpdate = error "engineUpdate: unimplemented"

#endif
