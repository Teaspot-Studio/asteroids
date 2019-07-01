module Data.Physics.Matter.Events(
    CollisionEvent(..)
  , collisionEventPairs
  , CollisionEventType(..)
  , listenCollisions
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Physics.Matter.Body
import Data.Physics.Matter.Engine
import Data.Text (Text)
import Language.Javascript.JSaddle

#ifdef ghcjs_HOST_OS
import GHCJS.Foreign.Callback
#endif

-- | Wrapper around object with collision event info
newtype CollisionEvent = CollisionEvent { unCollisionEvent :: JSVal }

-- | Possible events that can be listened
data CollisionEventType = CollisionStart | CollisionActive | CollisionEnd
  deriving (Eq, Show)

-- | Map haskell enum to js event name
collisionEventType :: CollisionEventType -> Text
collisionEventType v = case v of
  CollisionStart -> "collisionStart"
  CollisionActive -> "collisionActive"
  CollisionEnd -> "collisionEnd"
{-# INLINE collisionEventType #-}

-- | Extract pairs of collided bodies
collisionEventPairs :: MonadJSM m => CollisionEvent -> m [(Body, Body)]
{-# INLINE collisionEventPairs #-}

-- | Start listening collision events
listenCollisions :: MonadJSM m => Engine -> CollisionEventType -> (CollisionEvent -> JSM ()) -> m ()
{-# INLINE listenCollisions #-}

#ifdef ghcjs_HOST_OS

foreign import javascript safe "$1.pairs"
  jsPairs :: CollisionEvent -> IO JSVal

foreign import javascript safe "$1.bodyA"
  jsPairA :: JSVal -> IO Body
foreign import javascript safe "$1.bodyB"
  jsPairB :: JSVal -> IO Body

collisionEventPairs e = liftIO $ do
  pairs :: [JSVal] <- maybe (fail "collisionEventPairs: event.pairs is not array!") pure =<< fromJSVal =<< jsPairs e
  flip traverse pairs $ \pair -> (,)
    <$> jsPairA pair
    <*> jsPairB pair

--foreign import javascript safe "console.log($1, $2); Matter.Events.on($1, $2, $3)"
foreign import javascript safe "Matter.Events.on($1, $2, function(event) {$3(event);})"
  jsEngineOn :: Engine -> Text -> Callback (JSVal -> IO ()) -> IO ()

listenCollisions e et f = liftIO $ do
  cb <- asyncCallback1 $ f . CollisionEvent
  jsEngineOn e (collisionEventType et) cb

#else
collisionEventPairs = error "collisionEventPairs: unimplemented"
listenCollisions = error "listenCollisions: unimplemented"
#endif
