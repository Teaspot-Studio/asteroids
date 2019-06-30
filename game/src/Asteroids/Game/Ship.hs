module Asteroids.Game.Ship(
    Ship(..)
  , HasShip
  , spawnPlayer
  ) where

import Apecs
import Asteroids.Game.Material
import Asteroids.Game.Physics
import Asteroids.Game.Player
import Asteroids.Game.Rigid
import Asteroids.Game.Shape
import Asteroids.Game.Store.Cache
import Control.Monad.IO.Class
import Data.Splaton
import Language.Javascript.JSaddle (MonadJSM)
import Linear

import Debug.Trace

import qualified Data.Vector.Unboxed as V

data Ship = Ship {
  -- | Ship maximum trust of engines
  shipMaxTrust :: !Double
}

-- | Shortcut to reduce size of function signatures that uses the `Ship` component.
type HasShip w m = (
    Get w m Ship
  , Set w m Ship
  , Destroy w m Ship
  , Members w m Ship
  , HasOwnedBy w m
  , HasShape w m
  , HasRigid w m
  , HasMaterial w m
  )

instance Component Ship where
  type Storage Ship = PCache 100 (Map Ship)

-- | Spawn player and its ship
spawnPlayer :: (HasShip w m, Has w m EntityCounter, HasPhysicsEngine w m, MonadJSM m)
  => PlayerNum -> SystemT w m Entity
spawnPlayer pnum = do
  r <- makeShipRigid shipShape
  traceM "!!!"
  newEntity (Ship 10, OwnedBy pnum, r, shipShape, shipMaterial pnum)

shipShape :: Shape
shipShape = Shape $ Polygon $ V.fromList [
    V2 30 0, V2 (-15) 15, V2 0 0, V2 (-15) (-15)
  ]

shipMaterial :: PlayerNum -> Material
shipMaterial i = Material {
    materialFill = playerFillColor i
  , materialFillAlpha = 1.0
  , materialLine = playerStrokeColor i
  , materialLineWidth = 5
  , materialLineAlpha = 0.5
  }

makeShipRigid :: (HasPhysicsEngine w m, MonadJSM m) => Shape -> SystemT w m Rigid
makeShipRigid (Shape p) = do
  let dense = 1
  newRigid 400 0 0 dense $ V.toList $ polygonPoints p
