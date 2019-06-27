module Asteroids.Frontend.Style(
    frontendCss
  , frontendCssBS
  ) where

import Clay
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)

frontendCssBS :: ByteString
frontendCssBS = selfcss
  where
    selfcss = toStrict . encodeUtf8 . renderWith compact [] $ frontendCss

frontendCss :: Css
frontendCss = do
  html ? textAlign center
  ".counter" |> button ? do
    marginRight (px 10)
    marginLeft  (px 10)
