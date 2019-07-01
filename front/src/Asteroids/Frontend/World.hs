module Asteroids.Frontend.World(
    RWorld(..)
  , RenderWorld
  , initRenderWorld
  , fillRenderWorld
  , stepRenderWorld
  ) where

import Apecs
import Asteroids.Frontend.App
import Asteroids.Frontend.Drawable
import Asteroids.Game.Entity
import Asteroids.Game.Random
import Asteroids.Game.World
import Control.Monad
import Control.Monad.IO.Class
import Data.Graphics.Pixi
import Language.Javascript.JSaddle (JSM)
import System.Random

data RWorld = RWorld {
  rworldDrawable :: !(Storage Drawable)
, rworldApp      :: !(Storage App)
}

type RenderWorld = World RWorld

initRenderWorld :: MonadIO m => m RenderWorld
initRenderWorld = initWorld =<< RWorld
  <$> explInit
  <*> explInit

instance Monad m => Has RenderWorld m Drawable where
  getStore = asks $ rworldDrawable . worldExtra
  {-# INLINE getStore #-}

instance Monad m => Has RenderWorld m App where
  getStore = asks $ rworldApp . worldExtra
  {-# INLINE getStore #-}

fillRenderWorld :: PixiApp -> SystemT RenderWorld JSM ()
fillRenderWorld app = do
  initPixiApp app
  seed <- liftIO randomIO
  initRandom seed
  fillWorld
  spawnDrawables

stepRenderWorld :: Double -> SystemT RenderWorld JSM ()
stepRenderWorld dt = do
  stepWorld dt
  drawDrawables
  processRemoved destroyDrawable
