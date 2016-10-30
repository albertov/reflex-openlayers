{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.OpenLayers.Event (on, on_, wrapOLEvent, wrapOLEvent_) where

import Reflex.Dom

import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))

import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Foreign.Callback (
    OnBlocked(ContinueAsync)
  , syncCallback1'
  , syncCallback1
  , releaseCallback
  )
import GHCJS.Foreign.QQ
import GHCJS.Types (JSVal, jsval)

on :: (PToJSVal a, PToJSVal b, PFromJSVal e)
   => String -> a -> (e -> IO b) -> IO (IO ())
on eventName ob cb = do
  cb' <- syncCallback1' (fmap pToJSVal . cb . pFromJSVal)
  let jsCb = jsval cb'
  key :: JSVal <- [jsu|$r=`ob.on(`eventName, `jsCb);|]
  return ([jsu_|`ob.unByKey(`key);|] >> releaseCallback cb')

on_ :: (PFromJSVal e, PToJSVal a)
    => String -> a -> (e -> IO ()) -> IO (IO ())
on_ eventName ob cb = do
  cb' <- syncCallback1 ContinueAsync (cb . pFromJSVal)
  let jsCb = jsval cb'
  key :: JSVal <- [jsu|$r=`ob.on(`eventName, `jsCb);|]
  return ([jsu_|`ob.unByKey(`key);|] >> releaseCallback cb')

wrapOLEvent
  :: (MonadIO m, TriggerEvent t m, PFromJSVal ev, PToJSVal o)
  => String -> o -> (ev -> IO a) -> m (Event t a)
wrapOLEvent eventName ob cb = do
  (ev,trig) <- newTriggerEvent
  _unsubscribe <- liftIO (on_ eventName ob (trig <=< cb))
  -- FIXME: unsubscribe not handled
  return ev

wrapOLEvent_
  :: (MonadIO m, TriggerEvent t m, PToJSVal o)
  => String -> o -> IO a -> m (Event t a)
wrapOLEvent_ eventName ob = wrapOLEvent eventName ob . (\f (_::JSVal) -> f)
