module Asteroids.Game.Ship(
    Ship(..)
  , HasShip
  , spawnPlayer
  , ShipControl(..)
  , shipControl
  , processPlayerInput
  , destroyShip
  ) where

import Apecs
import Asteroids.Game.Material
import Asteroids.Game.Entity
import Asteroids.Game.Physics
import Asteroids.Game.Player
import Asteroids.Game.Rigid
import Asteroids.Game.Shape
import Asteroids.Game.Store.Cache
import Control.Monad
import Control.Monad.IO.Class
import Data.Splaton
import Language.Javascript.JSaddle (MonadJSM, liftJSM)
import Linear

import Debug.Trace

import qualified Data.Vector.Unboxed as V
import qualified Data.Physics.Matter as MT

data Ship = Ship {
  -- | Ship maximum trust of engines
  shipMaxTrust  :: !Double
  -- | Ship turning speed
, shipTurnSpeed :: !Double
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
  let ship = Ship {
          shipMaxTrust = 100000
        , shipTurnSpeed = 0.05
        }
  eid <- newEntity (ship, OwnedBy pnum, r, shipShape, shipMaterial pnum)
  bindRigid r eid
  pure eid

shipShape :: Shape
shipShape = Shape $ Polygon $ V.fromList [
    V2 30 0, V2 (-10) 15, V2 0 0, V2 (-10) (-15)
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

-- | Controls commands of ship
data ShipControl = ShipTrust | ShipLeft | ShipRight | ShipTurnStop
  deriving (Eq, Show)

-- | Apply controls to ship
shipControl :: (HasShip w m, MonadJSM m) => Entity -> ShipControl -> SystemT w m ()
shipControl e c = do
  (Ship{..}, Rigid b) <- get e
  lift $ case c of
    ShipTrust -> do
      p <- MT.bodyPosition b
      a <- MT.bodyAngle b
      let dv = ang2vec $ Radian a
      MT.bodyApplyForce b p (fmap (shipMaxTrust *) dv)
    ShipLeft -> MT.bodySetAngularVelocity b (negate shipTurnSpeed)
    ShipRight -> MT.bodySetAngularVelocity b shipTurnSpeed
    ShipTurnStop -> MT.bodySetAngularVelocity b 0

-- | Process commands from given player
processPlayerInput :: (HasShip w m, MonadJSM m) => PlayerNum -> ShipControl -> SystemT w m ()
processPlayerInput p c = cmapM_ $ \(OwnedBy p', _ :: Ship, eid) -> when (p' == p) $ shipControl eid c

-- | If given entity is ship, destroy it
destroyShip :: (HasShip w m, Has w m PhysicsEngine, HasRemoved w m, MonadJSM m) => Entity -> SystemT w m ()
destroyShip eid = do
  isShip <- exists eid (Proxy :: Proxy Ship)
  when isShip $ do
    Rigid r <- get eid
    w <- getPhysicsWorld
    lift $ MT.worldRemove w r
    destroy eid (Proxy :: Proxy (Ship, OwnedBy, Shape, Material))
    fireRemoved eid
