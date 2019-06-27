module Main where

import Asteroids.Frontend
import Asteroids.Frontend.Style
import Reflex.Dom

main :: IO ()
main = mainWidgetWithCss frontendCssBS frontend
