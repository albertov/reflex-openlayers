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

wrapOLEvent eventName ob cb = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  newEventWithTrigger $ \trig -> do
    unsubscribe <- liftIO $ on_ eventName ob $ \e -> do
      v <- cb e
      postGui $ runWithActions [trig :=> v]
    return (liftIO unsubscribe)

wrapOLEvent_ eventName ob cb = do
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  newEventWithTrigger $ \trig -> do
    unsubscribe <- liftIO $ on_ eventName ob $ \(_::JSVal) -> do
      v <- cb
      postGui $ runWithActions [trig :=> v]
    return (liftIO unsubscribe)
