{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

module Reflex.OpenLayers.Util (olSetter) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import GHCJS.Marshal (ToJSVal(toJSVal))
import GHCJS.Types
import GHCJS.DOM.Types (toJSString)


olSetter
  :: (MonadIO m , ToJSVal a, ToJSVal b)
  => String -> a -> b -> m ()
olSetter propName obj val = liftIO $ do
  obj' <- toJSVal obj
  val' <- toJSVal val
  js_olSetter (toJSString propName)  obj' val'

foreign import javascript unsafe "$2[$1]($3)"
  js_olSetter :: JSString -> JSVal -> JSVal -> IO ()
