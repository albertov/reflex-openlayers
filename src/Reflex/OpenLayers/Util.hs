{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE JavaScriptFFI #-}

module Reflex.OpenLayers.Util (
    olSetter
  , initProp
  , initPropWith
  , initOLProp
  , initOLPropWith
  ) where
import Reflex
import Reflex.Dom

import Control.Lens (Lens', (^.))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHCJS.Marshal (ToJSVal(toJSVal))
import GHCJS.Types
import GHCJS.DOM.Types (toJSString)

initProp
  :: (ToJSVal a, ToJSVal b, MonadWidget t m)
  => (a -> b -> WidgetHost m ())
  -> Lens' s (Dynamic t b)
  -> a -> s -> m ()
initProp setter lens_ = initPropWith setter lens_ return

initPropWith
  :: (ToJSVal a, ToJSVal b, MonadWidget t m)
  => (a -> b -> WidgetHost m ())
  -> Lens' s (Dynamic t c)
  -> (c -> WidgetHost m b)
  -> a -> s -> m ()
initPropWith setter lens_ build jsVal cfg = do
  schedulePostBuild $ do
    src <- build =<< sample (current (cfg^.lens_))
    setter jsVal src
  performEvent_ $
    fmap (build >=> setter jsVal)
    (updated (cfg^.lens_))

initOLProp
  :: (ToJSVal a, ToJSVal b, MonadWidget t m)
  => String
  -> Lens' s (Dynamic t b)
  -> a -> s -> m ()
initOLProp = initProp . olSetter

initOLPropWith
  :: (ToJSVal a, ToJSVal b, MonadWidget t m)
  => String
  -> Lens' s (Dynamic t c)
  -> (c -> WidgetHost m b)
  -> a -> s -> m ()
initOLPropWith = initPropWith . olSetter

olSetter
  :: (MonadIO m , ToJSVal a, ToJSVal b)
  => String -> a -> b -> m ()
olSetter propName obj val = liftIO $ do
  obj' <- toJSVal obj
  val' <- toJSVal val
  js_olSetter (toJSString propName)  obj' val'

foreign import javascript unsafe "$2[$1]($3)"
  js_olSetter :: JSString -> JSVal -> JSVal -> IO ()
