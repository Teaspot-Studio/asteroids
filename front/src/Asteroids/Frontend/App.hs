module Asteroids.Frontend.App(
    App(..)
  , HasApp
  , initPixiApp
  , getPixiApp
  ) where

import Apecs
import Data.Graphics.Pixi

-- | Global component that tracks pixi graphics context
newtype App = App { unApp :: Maybe PixiApp }

-- | Shortcut to reduce size of function signatures that uses the `App` component.
type HasApp w m = (
    Get w m App
  , Set w m App
  )

instance Semigroup App where
  _ <> b = b

instance Monoid App where
  mempty = App Nothing

instance Component App where
  type Storage App = Global App

-- | Set global for pixi app
initPixiApp :: Set w m App => PixiApp -> SystemT w m ()
initPixiApp = set global . App . Just

-- | Getting global
getPixiApp :: Get w m App => SystemT w m PixiApp
getPixiApp = do
  mapp <- fmap unApp $ get global
  maybe (fail "Uinitialized PixiApp, did you call initPixiApp?") pure mapp 
