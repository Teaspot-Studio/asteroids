module Asteroids.Game.Collision(
    processCollisions
  ) where

import Apecs
import Asteroids.Game.Entity
import Asteroids.Game.Physics
import Asteroids.Game.Rigid
import Asteroids.Game.Ship
import Data.Foldable (traverse_)
import Language.Javascript.JSaddle (JSM, liftJSM)

import qualified Data.Physics.Matter as MT

-- | Destroy ship on collision and split asteroids.
processCollisions :: (HasPhysicsEngine w JSM, HasRemoved w JSM, HasShip w JSM) => SystemT w JSM ()
processCollisions = do
  e <- getPhysicsEngine
  w <- ask
  lift $ MT.listenCollisions e MT.CollisionStart $ \pairsObj -> runWith w $ do
    bs <- lift $ MT.collisionEventPairs pairsObj
    flip traverse_ bs $ \(ba', bb') -> do
      ba <- lift $ MT.bodyRootParent ba'
      bb <- lift $ MT.bodyRootParent bb'
      mae <- lift $ rigidEntity $ Rigid ba
      mbe <- lift $ rigidEntity $ Rigid bb
      flip traverse_ ((,) <$> mae <*> mbe) $ \(ae, be) -> do
        destroyShip ae
        destroyShip be
