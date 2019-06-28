module Asteroids.Frontend.Render.Polygon(
    drawPolygon
  ) where

import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.Graphics.Pixi
import Data.Splaton
import Linear

import qualified Data.Vector.Unboxed as V

drawPolygon :: MonadIO m => PixiGraphics -> Polygon Float -> m ()
drawPolygon g p
  | V.null (polygonPoints p) = pure ()
  | otherwise = do
    let points = polygonPoints p
        V2 x0 y0 = V.head points
    moveTo g x0 y0
    traverse_ draw $ V.toList $ V.tail points
    lineTo g x0 y0
    where
      draw (V2 x y) = lineTo g x y
