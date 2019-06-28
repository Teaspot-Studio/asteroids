module Asteroids.Frontend.Monad(
    MonadFront
  ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Reflex.Dom

type MonadFront t m = (
    MonadHold t m
  , PostBuild t m
  , DomBuilder t m
  , MonadFix m
  , MonadIO m
  , DomBuilderSpace m ~ GhcjsDomSpace
  )
