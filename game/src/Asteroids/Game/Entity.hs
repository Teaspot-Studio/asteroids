module Asteroids.Game.Entity(
    Removed(..)
  , HasRemoved
  , fireRemoved
  , processRemoved
  ) where

import Apecs
import Data.Foldable (traverse_)
import Data.Sequence (Seq)

import qualified Data.Sequence as S

-- | Store collection of remove entities to process them in rendering world layer
newtype Removed = Removed { unRemoved :: Seq Entity }

-- | Shortcut to reduce size of function signatures that uses the `Removed` component.
type HasRemoved w m = (
    Get w m Removed
  , Set w m Removed
  )

instance Semigroup Removed where
  _ <> b = b

instance Monoid Removed where
  mempty = Removed mempty

instance Component Removed where
  type Storage Removed = Global Removed

-- | Store entity id that was removed
fireRemoved :: HasRemoved w m => Entity -> SystemT w m ()
fireRemoved e = modify global $ Removed . (S.|> e) . unRemoved

-- | Perform actions for each removed entity and clear collection of removed entities
processRemoved :: HasRemoved w m => (Entity -> SystemT w m ()) -> SystemT w m ()
processRemoved f = do
  Removed es <- get global
  traverse_ f es
  set global $ Removed mempty
