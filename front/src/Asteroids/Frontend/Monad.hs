module Asteroids.Frontend.Monad(
    MonadFront
  ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Language.Javascript.JSaddle
import Reflex.Dom

type MonadFront t m = (
    MonadHold t m
  , PostBuild t m
  , DomBuilder t m
  , MonadFix m
  , MonadIO m
  , MonadJSM m
  , DomBuilderSpace m ~ GhcjsDomSpace
  )
