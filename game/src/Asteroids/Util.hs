module Asteroids.Util(
    (<>)
  , showt
  ) where

import Data.Semigroup ((<>))
import Data.Text (Text, pack)

showt :: Show a => a -> Text
showt = pack . show
