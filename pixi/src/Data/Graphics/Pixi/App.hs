module Data.Graphics.Pixi.App(
    PixiApp(..)
  , pixiNewApp
  , pixiAppendTo
  , pixiStageInteractive
  , pixiAddChild
  , pixiSetOnClick
  , pixiAddTicker
  ) where

import Control.Monad.IO.Class
import Data.Graphics.Pixi.Graphics
import Language.Javascript.JSaddle

#ifdef ghcjs_HOST_OS
import GHCJS.Foreign.Callback
#endif

-- | Handle to the main app of Pixi
newtype PixiApp = PixiApp { unPixiApp :: JSVal }

#ifdef ghcjs_HOST_OS

foreign import javascript safe "new PIXI.Application($1, $2, { antialias: true, backgroundColor: $3 })"
  js_pixi_new_app :: Int -> Int -> Color -> IO JSVal

-- | Create new PIXI element with drawing context
pixiNewApp :: MonadIO m
  => Int -- ^ Width
  -> Int -- ^ Height
  -> Color -- ^ Background color
  -> m PixiApp
pixiNewApp w h c = fmap PixiApp $ liftIO $ js_pixi_new_app w h c
{-# INLINABLE pixiNewApp #-}

foreign import javascript safe "$2.appendChild($1.view);"
  js_pixi_append_to :: JSVal -> JSVal -> IO ()

-- | Attach PIXI element to the given dom element
pixiAppendTo :: MonadIO m
  => PixiApp
  -> JSVal
  -> m ()
pixiAppendTo (PixiApp a) e = liftIO $ js_pixi_append_to a e
{-# INLINABLE pixiAppendTo #-}

foreign import javascript safe "$1.stage.interactive = $2;"
  js_pixi_stage_interactive :: JSVal -> Bool -> IO ()

-- | Enable interactive events on PIXI element
pixiStageInteractive :: MonadIO m
  => PixiApp
  -> Bool
  -> m ()
pixiStageInteractive (PixiApp a) v = liftIO $ js_pixi_stage_interactive a v
{-# INLINABLE pixiStageInteractive #-}

foreign import javascript safe "$1.stage.addChild($2);"
  js_pixi_add_child :: JSVal -> JSVal -> IO ()

-- | Attach graphics context to the PIXI element
pixiAddChild :: MonadIO m
  => PixiApp
  -> PixiGraphics
  -> m ()
pixiAddChild (PixiApp a) (PixiGraphics v) = liftIO $ js_pixi_add_child a v
{-# INLINABLE pixiAddChild #-}

foreign import javascript safe "$1.stage.on('pointertap', $2);"
  js_pixi_set_on_click :: JSVal -> Callback (IO ()) -> IO ()

-- | Call function on mouse click
pixiSetOnClick :: MonadIO m
  => PixiApp
  -> JSM ()
  -> m ()
pixiSetOnClick (PixiApp a) f = liftIO $ do
  cb <- asyncCallback f
  js_pixi_set_on_click a cb
{-# INLINABLE pixiSetOnClick #-}

foreign import javascript safe "$1.ticker.add($2);"
  js_pixi_add_ticker :: JSVal -> Callback (IO ()) -> IO ()

-- | Call function on each draw tick
pixiAddTicker :: MonadIO m
  => PixiApp
  -> JSM ()
  -> m ()
pixiAddTicker (PixiApp a) f = liftIO $ do
  cb <- asyncCallback f
  js_pixi_add_ticker a cb
{-# INLINABLE pixiAddTicker #-}

#else

-- | Create new PIXI element with drawing context
pixiNewApp :: MonadIO m
  => Int -- ^ Width
  -> Int -- ^ Height
  -> Color -- ^ Background color
  -> m PixiApp
pixiNewApp _ _ _ = error "pixiNewApp: unimplemented"

-- | Attach PIXI element to the given dom element
pixiAppendTo :: MonadIO m
  => PixiApp
  -> JSVal
  -> m ()
pixiAppendTo = error "pixiAppendTo: unimplemented"

-- | Enable interactive events on PIXI element
pixiStageInteractive :: MonadIO m
  => PixiApp
  -> Bool
  -> m ()
pixiStageInteractive = error "pixiStageInteractive: unimplemented"

-- | Attach graphics context to the PIXI element
pixiAddChild :: MonadIO m
  => PixiApp
  -> PixiGraphics
  -> m ()
pixiAddChild = error "pixiAddChild: unimplemented"

-- | Call function on mouse click
pixiSetOnClick :: MonadIO m
  => PixiApp
  -> JSM ()
  -> m ()
pixiSetOnClick = error "pixiSetOnClick: unimplemented"

-- | Call function on each draw tick
pixiAddTicker :: MonadIO m
  => PixiApp
  -> JSM ()
  -> m ()
pixiAddTicker = error "pixiAddTicker: unimplemented"

#endif
