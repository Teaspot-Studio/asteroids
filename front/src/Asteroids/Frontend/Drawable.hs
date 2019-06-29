module Asteroids.Frontend.Drawable(
    Drawable(..)
  , HasDrawable
  , spawnDrawables
  , drawDrawables
  ) where

import Apecs
import Asteroids.Frontend.App
import Asteroids.Frontend.Render
import Asteroids.Game.Material
import Asteroids.Game.Shape
import Asteroids.Game.Transform
import Asteroids.Game.Asteroid
import Asteroids.Game.Store.Cache
import Control.Monad.IO.Class
import Data.Graphics.Pixi
import Data.Splaton

newtype Drawable = Drawable {
  drawableGraphics :: PixiGraphics
}

-- | Shortcut to reduce size of function signatures that uses the `Drawable` component.
type HasDrawable w m = (
    Get w m Drawable
  , Set w m Drawable
  , Destroy w m Drawable
  , Members w m Drawable
  )

instance Component Drawable where
  type Storage Drawable = PCache 100 (Map Drawable)

-- | Attach drawable to each shape with material
spawnDrawables :: (HasDrawable w m, HasShape w m, HasMaterial w m, HasApp w m, MonadIO m) => SystemT w m ()
spawnDrawables = do
  app <- getPixiApp
  cmapM_ $ \(sh :: Shape, _ :: Material, eid) -> do
    g <- newGraphics
    pixiAddChild app g
    eid $= Drawable g

-- | Draw all drawables in the world
drawDrawables :: (HasDrawable w m, HasAsteroid w m, MonadIO m) => SystemT w m ()
drawDrawables = do
  cmapM_ $ \(Shape ps, mat, Trans t, Drawable g) -> do
    graphicsClear g
    withMaterial g mat $ drawPolygon g $ mapPolygon realToFrac $ applyTransform t ps
