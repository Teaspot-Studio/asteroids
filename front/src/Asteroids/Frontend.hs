module Asteroids.Frontend(
    frontend
  ) where

import Apecs
import Asteroids.Frontend.Input
import Asteroids.Frontend.Monad
import Asteroids.Frontend.Render
import Asteroids.Frontend.World
import Asteroids.Game
import Asteroids.Util
import Control.Monad
import Control.Monad.IO.Class
import Data.Graphics.Pixi
import Data.IORef
import Data.Splaton
import Data.Time
import GHCJS.DOM.Types (unElement)
import Language.Javascript.JSaddle
import Linear
import Reflex.Dom

frontend :: MonadFront t m => m ()
frontend = do
  (elmnt, _) <- el' "div" $ pure ()
  cntrs <- newGameControls
  liftJSM $ drawGame cntrs $ unElement . _element_raw $ elmnt

data GameControls = GameControls {
  shipUpRef     :: !(IORef Bool)
, shipDownRef   :: !(IORef Bool)
, shipLeftRef   :: !(IORef Bool)
, shipRightRef  :: !(IORef Bool)
}

makeDynamicRef :: MonadFront t m => Dynamic t a -> m (IORef a)
makeDynamicRef d = do
  a0 <- sample . current $ d
  ref <- liftIO $ newIORef a0
  performEvent_ $ ffor (updated d) $ liftIO . writeIORef ref
  pure ref

newGameControls :: MonadFront t m => m GameControls
newGameControls = do
  upD <- windowKeyPress ArrowUp
  downD <- windowKeyPress ArrowDown
  leftD <- windowKeyPress ArrowLeft
  rightD <- windowKeyPress ArrowRight
  GameControls
    <$> makeDynamicRef upD
    <*> makeDynamicRef downD
    <*> makeDynamicRef leftD
    <*> makeDynamicRef rightD

reactInput :: GameControls -> SystemT RenderWorld IO ()
reactInput GameControls{..} = do
  fforInput shipUpRef $ processPlayerInput 0 ShipTrust
  fforInput shipLeftRef $ processPlayerInput 0 ShipLeft
  fforInput shipRightRef $ processPlayerInput 0 ShipRight
  fforNone [shipLeftRef, shipRightRef] $ processPlayerInput 0 ShipTurnStop
  where
    fforInput r ma = do
      v <- liftIO $ readIORef r
      when v ma
    fforNone rs ma = do
      vs <- liftIO $ traverse readIORef rs
      unless (or vs) ma  

drawGame :: GameControls -> JSVal -> JSM ()
drawGame cntrls e = do
  app <- pixiNewApp 800 800 0x000000
  pixiAppendTo app e
  pixiStageInteractive app True

  w <- initRenderWorld
  runWith w $ fillRenderWorld app

  t0 <- liftIO getCurrentTime
  tickRef <- liftIO $ newIORef t0
  pixiAddTicker app $ do
    t2 <- liftIO getCurrentTime
    dt <- liftIO $ atomicModifyIORef' tickRef $ \t1 -> (t2, realToFrac $ t2 `diffUTCTime` t1)
    runWith w $ do
      reactInput cntrls
      stepRenderWorld dt

-- example :: JSVal -> IO ()
-- example e = do
--   app <- pixiNewApp 800 800 0x000000
--   pixiAppendTo app e
--   pixiStageInteractive app True
--
--   g <- newGraphics
--   beginFill g 0xFF3300 1.0
--   lineStyle g 10 0xffd900 1
--
--   moveTo g 50 50
--   lineTo g 250 50
--   lineTo g 100 100
--   lineTo g 250 220
--   lineTo g 50 220
--   lineTo g 50 50
--   endFill g
--
--   beginFill g 0xFF700B 1
--   lineStyle g 10 0xFF0000 0.8
--
--   moveTo g 210 300
--   lineTo g 450 320
--   lineTo g 570 350
--   quadraticCurveTo g 600 0 480 100
--   lineTo g 330 120
--   lineTo g 410 200
--   lineTo g 210 300
--   endFill g
--
--   lineStyle g 2 0x0000FF 1
--   drawRect g 50 280 100 100
--
--   lineStyle g 0 0 1.0
--   beginFill g 0xFFFF0B 0.5
--   drawCircle g 470 200 100
--   endFill g
--
--   lineStyle g 20 0x33FF00 1.0
--   moveTo g 30 30
--   lineTo g 600 300
--
--   pixiAddChild app g
--
--   thing <- newGraphics
--   pixiAddChild app thing
--   graphicsSetX thing 400
--   graphicsSetY thing 300
--
--   pixiSetOnClick app $ do
--     lw <- randomIO
--     lc :: Float <- randomIO
--     lineStyle g (lw * 30) (round $ lc * fromIntegral 0xFFFFFF) 1
--     sx <- randomIO
--     sy <- randomIO
--     moveTo g (sx * 800) (sy * 600)
--     [c1x, c1y, c2x, c2y, dx, dy] <- replicateM 6 randomIO
--     bezierCurveTo g (c1x * 800) (c1y * 600) (c2x * 800) (c2y * 600) (dx * 800) (dy * 600)
--
--   countRef <- newIORef 0
--   pixiAddTicker app $ do
--     c <- readIORef countRef
--     writeIORef countRef $ c + 0.1
--
--     graphicsClear thing
--     lineStyle thing 10  0xff0000 1
--     beginFill thing 0xffFF00 0.5
--
--     moveTo thing (-120 + sin c * 20) (-100 + cos c * 20)
--     lineTo thing ( 120 + cos c * 20) (-100 + sin c * 20)
--     lineTo thing ( 120 + sin c * 20) ( 100 + cos c * 20)
--     lineTo thing (-120 + cos c * 20) ( 100 + sin c * 20)
--     lineTo thing (-120 + sin c * 20) (-100 + cos c * 20)
--
--     graphicsSetRotation thing $ c * 0.1
