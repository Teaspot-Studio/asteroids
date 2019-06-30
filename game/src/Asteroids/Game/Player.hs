module Asteroids.Game.Player(
    PlayerNum
  , Player(..)
  , Players
  , addPlayer
  , getPlayer
  , forAllPlayers_
  , OwnedBy(..)
  , HasOwnedBy
  , playerFillColor
  , playerStrokeColor
  ) where

import Apecs
import Asteroids.Game.Material
import Asteroids.Game.Store.Cache
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.IntMap (IntMap)

import qualified Data.IntMap as M

-- | ID of player
type PlayerNum = Int

-- | Global per player component
data Player = Player {
  -- | Player number as id
  playerNum :: !PlayerNum
  -- | Score of player
, playerScore :: !Int
}

-- | All known players storage
type Players = IntMap Player

instance Component Players where
  type Storage Players = Global Players

-- | Add new player to game
addPlayer :: (Set w m Players, MonadIO m) => Player -> SystemT w m ()
addPlayer p = modify global $ M.insert (playerNum p) p

-- | Get player by it number
getPlayer :: Get w m Players => PlayerNum -> SystemT w m (Maybe Player)
getPlayer i = fmap (M.lookup i) $ get global

-- | Perform action for all players
forAllPlayers_ :: Get w m Players => (Player -> SystemT w m ()) -> SystemT w m ()
forAllPlayers_ f = do
  ps :: Players <- get global
  traverse_ f ps

-- | Component that indicates that given entity is belongs to given player
newtype OwnedBy = OwnedBy { ownedBy :: PlayerNum }

-- | Shortcut to reduce size of function signatures that uses the `OwnedBy` component.
type HasOwnedBy w m = (
    Get w m OwnedBy
  , Set w m OwnedBy
  , Destroy w m OwnedBy
  , Members w m OwnedBy
  )

instance Component OwnedBy where
  type Storage OwnedBy = PCache 100 (Map OwnedBy)

playerFillColor :: PlayerNum -> Color
playerFillColor i = case i of
  0 -> 0xe02121
  _ -> 0x3782e5

playerStrokeColor :: PlayerNum -> Color
playerStrokeColor i = case i of
  0 -> 0xa52626
  _ -> 0x366baf
