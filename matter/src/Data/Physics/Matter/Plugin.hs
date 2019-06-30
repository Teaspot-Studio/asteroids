module Data.Physics.Matter.Plugin(
    PluginOptions(..)
  , usePluginRaw
  , Plugin(..)
  , pluginName
  , usePlugin
  ) where

import Data.Aeson
import Data.Physics.Matter.Vector
import Data.Text (Text)
import Language.Javascript.JSaddle
import Linear

-- | Known plugins options
data PluginOptions =
  -- | matter-wrap plugin
  WrapPlugin {
      wrapPluginMin :: !(V2 Double)
    , wrapPluginMax :: !(V2 Double)
    }
  deriving (Show)

instance ToJSON PluginOptions where
  toJSON WrapPlugin{..} = object [
      "wrap" .= object [
        "min" .= toVecJSON wrapPluginMin
      , "max" .= toVecJSON wrapPluginMax
      ]
    ]
  {-# INLINE toJSON #-}

instance ToJSVal PluginOptions where
  toJSVal = toJSVal . toJSON
  {-# INLINE toJSVal #-}

-- | Enable given plugin in engine
usePluginRaw :: MonadJSM m => Text -> m ()
{-# INLINE usePluginRaw #-}

-- | Statically known plugins
data Plugin = PluginWrap
  deriving (Eq, Show)

-- | Get name of plugin that can be passed to Matter.use
pluginName :: Plugin -> Text
pluginName v = case v of
  PluginWrap -> "matter-wrap"

-- | Enable given plugin in engine
usePlugin :: MonadJSM m => Plugin -> m ()
usePlugin = usePluginRaw . pluginName
{-# INLINE usePlugin #-}

#ifdef ghcjs_HOST_OS

foreign import javascript safe "Matter.use($1)"
  jsUsePlugin :: Text -> IO ()

usePluginRaw p = liftJSM $ jsUsePlugin p

#else

usePluginRaw = error "usePlugin: unimplemented"

#endif
