{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.OpenLayers.Event (on, on_, wrapOLEvent, wrapOLEvent_) where

import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Foreign.Callback (
    OnBlocked(ContinueAsync)
  , syncCallback1'
  , syncCallback1
  , releaseCallback
  )
import GHCJS.Foreign.QQ
import GHCJS.Types (JSVal, jsval)
import Reflex.Dom
import Reflex.Host.Class
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Dependent.Sum (DSum (..))

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
  :: forall (m :: * -> *) t (h :: * -> *) t1 a e a1.
     ( MonadReflexCreateTrigger t1 m, HasPostGui t h m
     , PFromJSVal e
     , PToJSVal a
     , EventTrigger t1 ~ EventTrigger t
     )
  => String -> a -> (e -> IO a1) -> m (Event t1 a1)
wrapOLEvent eventName ob cb = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  newEventWithTrigger $ \trig -> do
    unsubscribe <- liftIO $ on_ eventName ob $ \e -> do
      v <- cb e
      postGui $ runWithActions [trig :=> return v]
    return (liftIO unsubscribe)

wrapOLEvent_
  :: forall (m :: * -> *) t (h :: * -> *) t1 a a1.
     ( HasPostGui t h m
     , MonadReflexCreateTrigger t1 m
     , PToJSVal a
     , EventTrigger t1 ~ EventTrigger t
     )
  => String -> a -> IO a1 -> m (Event t1 a1)
wrapOLEvent_ eventName ob cb = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  newEventWithTrigger $ \trig -> do
    unsubscribe <- liftIO $ on_ eventName ob $ \(_::JSVal) -> do
      v <- cb
      postGui $ runWithActions [trig :=> return v]
    return (liftIO unsubscribe)
