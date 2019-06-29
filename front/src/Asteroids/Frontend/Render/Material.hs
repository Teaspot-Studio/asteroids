module Asteroids.Frontend.Render.Material(
    withMaterial
  ) where

import Asteroids.Game.Material
import Control.Monad.IO.Class
import Data.Graphics.Pixi

withMaterial :: MonadIO m => PixiGraphics -> Material -> m a -> m a
withMaterial g Material{..} ma = do
  beginFill g materialFill (realToFrac materialFillAlpha)
  lineStyle g (realToFrac materialLineWidth) materialLine (realToFrac materialLineAlpha)
  a <- ma
  endFill g
  pure a
