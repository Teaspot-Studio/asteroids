-- | Workaround for https://github.com/jonascarpay/apecs/issues/31
module Asteroids.Game.Store.Cache(
    PCache
  ) where

import           Control.Monad.Reader
import qualified Data.IntMap.Strict          as M
import           Data.IORef
import           Data.Maybe                  (fromJust)
import           Data.Proxy
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           GHC.TypeLits

import Apecs.Core
import Apecs.Stores

-- | A cache around another store.
--   Caches store their members in a fixed-size vector, so operations run in O(1).
--   Caches can provide huge performance boosts, especially for large numbers of components.
--   The cache size is given as a type-level argument.
--
--   Note that iterating over a cache is linear in cache size, so sparsely populated caches might actually decrease performance.
--   In general, the exact size of the cache does not matter as long as it reasonably approximates the number of components present.
--
--   The cache uses entity (-2) to internally represent missing entities, so be wary when manually manipulating entities.
data PCache (n :: Nat) s =
  PCache Int (VM.IOVector Int) (VM.IOVector (Elem s)) s

cacheMiss :: t
cacheMiss = error "Cache miss!"

type instance Elem (PCache n s) = Elem s

instance (MonadIO m, ExplInit m s, KnownNat n, Cachable s) => ExplInit m (PCache n s) where
  {-# INLINE explInit #-}
  explInit = do
    let n = fromIntegral$ natVal (Proxy @n)
    tags <- liftIO$ VM.replicate n (-2)
    cache <- liftIO$ VM.replicate n cacheMiss
    child <- explInit
    return (PCache n tags cache child)

instance (MonadIO m, ExplGet m s) => ExplGet m (PCache n s) where
  {-# INLINE explGet #-}
  explGet (PCache n tags cache s) ety = do
    let index = ety `rem` n
    tag <- liftIO$ VM.unsafeRead tags index
    if tag == ety
       then liftIO$ VM.unsafeRead cache index
       else explGet s ety

  {-# INLINE explExists #-}
  explExists (PCache n tags _ s) ety = do
    tag <- liftIO$ VM.unsafeRead tags (ety `rem` n)
    if tag == ety then return True else explExists s ety

instance (MonadIO m, ExplSet m s) => ExplSet m (PCache n s) where
  {-# INLINE explSet #-}
  explSet (PCache n tags cache s) ety x = do
    let index = ety `rem` n
    tag <- liftIO$ VM.unsafeRead tags index
    when (tag /= (-2) && tag /= ety) $ do
      cached <- liftIO$ VM.unsafeRead cache index
      explSet s tag cached
    liftIO$ VM.unsafeWrite tags  index ety
    liftIO$ VM.unsafeWrite cache index x

instance (MonadIO m, ExplDestroy m s) => ExplDestroy m (PCache n s) where
  {-# INLINE explDestroy #-}
  explDestroy (PCache n tags cache s) ety = do
    let index = ety `rem` n
    tag <- liftIO$ VM.unsafeRead tags (ety `rem` n)
    if tag == ety
       then do
         liftIO$ VM.unsafeWrite tags  index (-2)
         liftIO$ VM.unsafeWrite cache index cacheMiss
       else explDestroy s ety

instance (MonadIO m, ExplMembers m s) => ExplMembers m (PCache n s) where
  {-# INLINE explMembers #-}
  explMembers (PCache _ tags _ s) = do
    cached <- liftIO $ V.filter (/= (-2)) <$> V.freeze tags
    stored <- explMembers s
    return $! V.convert cached U.++ stored
