module Data.Graphics.Pixi.Graphics(
    Color
  , Alpha
  , Width
  , Height
  , Radius
  , X
  , Y
  , Rotation
  , PixiGraphics(..)
  , newGraphics
  , graphicsSetX
  , graphicsSetY
  , graphicsSetRotation
  , graphicsClear
  , beginFill
  , endFill
  , lineStyle
  , moveTo
  , lineTo
  , drawRect
  , drawCircle
  , bezierCurveTo
  , quadraticCurveTo
  , graphicsSetOnClick
  , graphicsSetInteractive
  ) where

import Control.Monad.IO.Class
import Language.Javascript.JSaddle

#ifdef ghcjs_HOST_OS
import GHCJS.Foreign.Callback
#endif

-- | Hex defined color
type Color = Int

-- | Alpha color value
type Alpha = Float

-- | Width units
type Width = Float

-- | Width units
type Height = Float

-- | Width units
type Radius = Float

-- | X coordinate
type X = Float

-- | Y coordinate
type Y = Float

-- | Angle units in radians
type Rotation = Float

-- | Handle to the main app of Pixi
newtype PixiGraphics = PixiGraphics { unPixiGraphics :: JSVal }

#ifdef ghcjs_HOST_OS

foreign import javascript safe "new PIXI.Graphics()"
  js_pixi_new_graphics :: IO JSVal

-- | Create new PIXI element with drawing context
newGraphics :: MonadIO m => m PixiGraphics
newGraphics = fmap PixiGraphics $ liftIO js_pixi_new_graphics
{-# INLINABLE newGraphics #-}

foreign import javascript safe "$1.x = $2;"
  js_pixi_graphics_set_x :: JSVal -> X -> IO ()

-- | Set x component of position for graphics node
graphicsSetX :: MonadIO m => PixiGraphics -> X -> m ()
graphicsSetX (PixiGraphics g) x = liftIO $ js_pixi_graphics_set_x g x
{-# INLINABLE graphicsSetX #-}

foreign import javascript safe "$1.y = $2;"
  js_pixi_graphics_set_y :: JSVal -> X -> IO ()

-- | Set y component of position for graphics node
graphicsSetY :: MonadIO m => PixiGraphics -> Y -> m ()
graphicsSetY (PixiGraphics g) y = liftIO $ js_pixi_graphics_set_y g y
{-# INLINABLE graphicsSetY #-}

foreign import javascript safe "$1.rotation = $2;"
  js_pixi_graphics_set_rotation :: JSVal -> Rotation -> IO ()

-- | Set rotation around axis in radians
graphicsSetRotation :: MonadIO m => PixiGraphics -> Rotation -> m ()
graphicsSetRotation (PixiGraphics g) r = liftIO $ js_pixi_graphics_set_rotation g r
{-# INLINABLE graphicsSetRotation #-}

foreign import javascript safe "$1.clear();"
  js_pixi_graphics_clear :: JSVal-> IO ()

-- | Clear all drawing context
graphicsClear :: MonadIO m => PixiGraphics -> m ()
graphicsClear (PixiGraphics g) = liftIO $ js_pixi_graphics_clear g
{-# INLINABLE graphicsClear #-}

foreign import javascript safe "$1.beginFill($2, $3);"
  js_pixi_begin_fill :: JSVal -> Color -> Alpha -> IO ()

-- | Start drawing filled figure with given graphics context
beginFill :: MonadIO m => PixiGraphics -> Color -> Alpha -> m ()
beginFill (PixiGraphics g) v alpha = liftIO $ js_pixi_begin_fill g v alpha
{-# INLINABLE beginFill #-}

foreign import javascript safe "$1.endFill();"
  js_pixi_end_fill :: JSVal -> IO ()

-- | Finish drawing filled figure with given graphics context
endFill :: MonadIO m => PixiGraphics -> m ()
endFill (PixiGraphics g) = liftIO $ js_pixi_end_fill g
{-# INLINABLE endFill #-}

foreign import javascript safe "$1.lineStyle($2, $3, $4);"
  js_pixi_line_style :: JSVal -> Width -> Color -> Alpha -> IO ()

-- | Set line width color and alpha for given context
lineStyle :: MonadIO m => PixiGraphics -> Width -> Color -> Alpha -> m ()
lineStyle (PixiGraphics g) w c a = liftIO $ js_pixi_line_style g w c a
{-# INLINABLE lineStyle #-}

foreign import javascript safe "$1.moveTo($2, $3);"
  js_pixi_move_to :: JSVal -> X -> Y -> IO ()

-- | Move current cursor of drawing context to the point
moveTo :: MonadIO m => PixiGraphics -> X -> Y -> m ()
moveTo (PixiGraphics g) x y = liftIO $ js_pixi_move_to g x y
{-# INLINABLE moveTo #-}

foreign import javascript safe "$1.lineTo($2, $3);"
  js_pixi_line_to :: JSVal -> X -> Y -> IO ()

-- | Draw line from current cursor of drawing context to the point
lineTo :: MonadIO m => PixiGraphics -> X -> Y -> m ()
lineTo (PixiGraphics g) x y = liftIO $ js_pixi_line_to g x y
{-# INLINABLE lineTo #-}

foreign import javascript safe "$1.drawRect($2, $3, $4, $5);"
  js_pixi_draw_rect :: JSVal -> X -> Y -> Width -> Height -> IO ()

-- | Draw rectangular with the current context
drawRect :: MonadIO m => PixiGraphics -> X -> Y -> Width -> Height -> m ()
drawRect (PixiGraphics g) x y w h = liftIO $ js_pixi_draw_rect g x y w h
{-# INLINABLE drawRect #-}

foreign import javascript safe "$1.drawCircle($2, $3, $4);"
  js_pixi_draw_circle :: JSVal -> X -> Y -> Radius -> IO ()

-- | Draw circle with the current context
drawCircle :: MonadIO m => PixiGraphics -> X -> Y -> Radius -> m ()
drawCircle (PixiGraphics g) x y r = liftIO $ js_pixi_draw_circle g x y r
{-# INLINABLE drawCircle #-}

foreign import javascript safe "$1.quadraticCurveTo($2, $3, $4, $5);"
  js_pixi_quadratic_curve_to :: JSVal -> X -> Y -> X -> Y -> IO ()

-- | Calculate the points for a quadratic bezier curve and then draws it. Based on: https://stackoverflow.com/questions/785097/how-do-i-implement-a-bezier-curve-in-c
quadraticCurveTo :: MonadIO m => PixiGraphics
  -> X -- ^ Control point
  -> Y -- ^ Control point
  -> X -- ^ Desitnation point
  -> Y -- ^ Destination point
  -> m ()
quadraticCurveTo (PixiGraphics g) cpx cpy dx dy = liftIO $ js_pixi_quadratic_curve_to g cpx cpy dx dy
{-# INLINABLE quadraticCurveTo #-}

foreign import javascript safe "$1.bezierCurveTo($2, $3, $4, $5, $6, $7);"
  js_pixi_bezier_curve_to :: JSVal -> X -> Y -> X -> Y -> X -> Y -> IO ()

-- | Calculate the points for a bezier curve and then draws it.
bezierCurveTo :: MonadIO m => PixiGraphics
  -> X -- ^ First control point
  -> Y -- ^ First control point
  -> X -- ^ Second control point
  -> Y -- ^ Second control point
  -> X -- ^ Dist point
  -> Y -- ^ Dist point
  -> m ()
bezierCurveTo (PixiGraphics g) cpX cpY cpX2 cpY2 toX toY = liftIO $ js_pixi_bezier_curve_to g cpX cpY cpX2 cpY2 toX toY
{-# INLINABLE bezierCurveTo #-}

foreign import javascript safe "$1.on('pointertap', $2);"
  js_pixi_graphics_set_on_click :: JSVal -> Callback (IO ()) -> IO ()

-- | Call function on mouse click
graphicsSetOnClick :: MonadIO m
  => PixiGraphics
  -> IO ()
  -> m ()
graphicsSetOnClick (PixiGraphics a) f = liftIO $ do
  cb <- asyncCallback f
  js_pixi_graphics_set_on_click a cb
{-# INLINABLE graphicsSetOnClick #-}

foreign import javascript safe "$1.interactive = $2;"
  js_pixi_graphics_interactive :: JSVal -> Bool -> IO ()

-- | Enable events for inputs for the graphics
graphicsSetInteractive :: MonadIO m
  => PixiGraphics
  -> Bool
  -> m ()
graphicsSetInteractive  (PixiGraphics g) v = liftIO $ do
  js_pixi_graphics_interactive g v
{-# INLINABLE graphicsSetInteractive #-}

#else

-- | Create new PIXI element with drawing context
newGraphics :: MonadIO m => m PixiGraphics
newGraphics = error "newGraphics: unimplemented"

-- | Set x component of position for graphics node
graphicsSetX :: MonadIO m => PixiGraphics -> X -> m ()
graphicsSetX = error "graphicsSetX: unimplemented"

-- | Set y component of position for graphics node
graphicsSetY :: MonadIO m => PixiGraphics -> Y -> m ()
graphicsSetY = error "graphicsSetY: unimplemented"

-- | Set rotation around axis in radians
graphicsSetRotation :: MonadIO m => PixiGraphics -> Rotation -> m ()
graphicsSetRotation = error "graphicsSetRotation: unimplemented"

-- | Clear all drawing context
graphicsClear :: MonadIO m => PixiGraphics -> m ()
graphicsClear = error "graphicsClear: unimplemented"

-- | Create new PIXI element with drawing context
beginFill :: MonadIO m => PixiGraphics -> Color -> Alpha -> m ()
beginFill = error "beginFill: unimplemented"

-- | Finish drawing filled figure with given graphics context
endFill :: MonadIO m => PixiGraphics -> m ()
endFill = error "endFill: unimplemented"

-- | Set line width color and alpha for given context
lineStyle :: MonadIO m => PixiGraphics -> Width -> Color -> Alpha -> m ()
lineStyle = error "lineStyle: unimplemented"

-- | Move current cursor of drawing context to the point
moveTo :: MonadIO m => PixiGraphics -> X -> Y -> m ()
moveTo = error "moveTo: unimplemented"

-- | Draw line from current cursor of drawing context to the point
lineTo :: MonadIO m => PixiGraphics -> X -> Y -> m ()
lineTo = error "lineTo: unimplemented"

-- | Draw rectangular with the current context
drawRect :: MonadIO m => PixiGraphics -> X -> Y -> Width -> Height -> m ()
drawRect = error "drawRect: unimplemented"

-- | Draw circle with the current context
drawCircle :: MonadIO m => PixiGraphics -> X -> Y -> Radius -> m ()
drawCircle = error "drawCircle: unimplemented"

-- | Calculate the points for a bezier curve and then draws it.
bezierCurveTo :: MonadIO m => PixiGraphics
  -> X -- ^ First control point
  -> Y -- ^ First control point
  -> X -- ^ Second control point
  -> Y -- ^ Second control point
  -> X -- ^ Dist point
  -> Y -- ^ Dist point
  -> m ()
bezierCurveTo = error "bezierCurveTo: unimplemented"

-- | Calculate the points for a quadratic bezier curve and then draws it. Based on: https://stackoverflow.com/questions/785097/how-do-i-implement-a-bezier-curve-in-c
quadraticCurveTo :: MonadIO m => PixiGraphics
  -> X -- ^ Control point
  -> Y -- ^ Control point
  -> X -- ^ Desitnation point
  -> Y -- ^ Destination point
  -> m ()
quadraticCurveTo = error "quadraticCurveTo: unimplemented"

-- | Call function on mouse click
graphicsSetOnClick :: MonadIO m
  => PixiGraphics
  -> IO ()
  -> m ()
graphicsSetOnClick = error "graphicsSetOnClick: unimplemented"

-- | Enable events for inputs for the graphics
graphicsSetInteractive :: MonadIO m
  => PixiGraphics
  -> Bool
  -> m ()
graphicsSetInteractive = error "graphicsSetInteractive: unimplemented"

#endif
