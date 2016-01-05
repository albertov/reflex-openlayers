{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI #-}

module Reflex.OpenLayers.Event (on, on_) where

import GHCJS.Marshal (ToJSVal(toJSVal), FromJSVal(fromJSVal))
import GHCJS.Types
import GHCJS.DOM.Types (toJSString)
import GHCJS.Foreign.Callback (
    Callback
  , OnBlocked(ContinueAsync)
  , syncCallback1'
  , syncCallback1
  , releaseCallback
  )

import Control.Monad ((>=>))

data Event = Event

instance FromJSVal Event where
  fromJSVal _ = return (Just Event)

on :: (ToJSVal a, ToJSVal b, FromJSVal e)
   => String -> a -> (e -> IO b) -> IO (IO ())
on eventName ob cb = do
  jsOb <- toJSVal ob
  jsCb <- syncCallback1' cb'
  key <- js_on jsOb (toJSString eventName) jsCb
  return (js_unByKey jsOb key >> releaseCallback jsCb)
  where
    cb' = (fromJSVal >=> maybe eErr cb >=> toJSVal)
    eErr = fail "Reflex.OpenLayers.Event.on: could not convert event"

on_ :: (FromJSVal e, ToJSVal a)
    => String -> a -> (e -> IO ()) -> IO (IO ())
on_ eventName ob cb = do
  jsOb <- toJSVal ob
  jsCb <- syncCallback1 ContinueAsync cb'
  key <- js_on jsOb (toJSString eventName) jsCb
  return (js_unByKey jsOb key >> releaseCallback jsCb)
  where
    cb' = (fromJSVal >=> maybe eErr cb)
    eErr = fail "Reflex.OpenLayers.Event.on_: could not convert event"

foreign import javascript unsafe "$1['on']($2, $3)"
  js_on :: JSVal -> JSString -> Callback (JSVal -> IO a) -> IO JSVal

foreign import javascript unsafe "$1['unByKey']($2)"
  js_unByKey :: JSVal -> JSVal -> IO ()
