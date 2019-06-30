module Asteroids.Frontend.Input(
    windowKeyDown
  , windowKeyUp
  , windowKeyPress
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Language.Javascript.JSaddle
import Reflex.Dom

#ifdef ghcjs_HOST_OS
import GHCJS.Foreign.Callback
#endif

windowKeyDown :: (TriggerEvent t m, MonadIO m) => Key -> m (Event t ())
windowKeyUp :: (TriggerEvent t m, MonadIO m) => Key -> m (Event t ())

windowKeyPress :: (TriggerEvent t m, MonadHold t m, Reflex t, MonadIO m) => Key -> m (Dynamic t Bool)
windowKeyPress k = do
  downE <- windowKeyDown k
  upE <- windowKeyUp k
  holdDyn False $ leftmost [ True <$ downE, False <$ upE ]

#ifdef ghcjs_HOST_OS

foreign import javascript safe "$1.keyCode"
  jsGetKeyCode :: JSVal -> IO KeyCode

foreign import javascript safe "window.addEventListener( 'keydown', $1, false )"
  jsAddWindowKeyDown :: Callback (JSVal -> IO ()) -> IO ()

windowKeyDown k = do
  (e, fire) <- newTriggerEvent
  liftIO $ do
    cb <- asyncCallback1 $ \v -> do
      kc <- jsGetKeyCode v
      when (keyCodeLookup kc == k) $ fire ()
    jsAddWindowKeyDown cb
  pure e

foreign import javascript safe "window.addEventListener( 'keyup', $1, false )"
  jsAddWindowKeyUp :: Callback (JSVal -> IO ()) -> IO ()

windowKeyUp k = do
  (e, fire) <- newTriggerEvent
  liftIO $ do
    cb <- asyncCallback1 $ \v -> do
      kc <- jsGetKeyCode v
      when (keyCodeLookup kc == k) $ fire ()
    jsAddWindowKeyUp cb
  pure e

#else

windowKeyDown = error "windowKeyDown: unimplemented"
windowKeyUp = error "windowKeyUp: unimplemented"

#endif
