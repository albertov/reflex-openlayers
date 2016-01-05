{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.OpenLayers.Util (
    olSetter
  , initProp
  , initPropWith
  , initOLProp
  , initOLPropWith
  , wrapObservableProp
  , dynInitialize
  , dynInitializeWith
  ) where

import Reflex.OpenLayers.Event

import Reflex
import Reflex.Dom
import Reflex.Host.Class (newEventWithTrigger)

import Control.Lens (Lens', (^.), lens)
import Control.Monad (forM_, (>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Dependent.Sum (DSum (..))
import qualified Data.Map as M
import GHCJS.Marshal (ToJSVal(toJSVal), FromJSVal(fromJSVal))
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal))
import GHCJS.Types
import GHCJS.DOM.Types (toJSString)
import qualified JavaScript.Object as O
import Data.Char
import System.IO.Unsafe (unsafePerformIO)

instance PToJSVal a => PToJSVal (M.Map String a) where
  pToJSVal m = unsafePerformIO $ do
    o <- O.create
    forM_ (M.toList m) $ \(k,v) -> do
      O.setProp (toJSString k) (pToJSVal v) o
    return (jsval o)

initProp
  :: (ToJSVal a, MonadWidget t m)
  => (JSVal -> a -> WidgetHost m ())
  -> Lens' cfg (Dynamic t a)
  -> JSVal -> cfg -> m ()
initProp setter lens_ = initPropWith setter lens_ return

initPropWith
  :: (ToJSVal a, MonadWidget t m)
  => (JSVal -> a -> WidgetHost m ())
  -> Lens' cfg (Dynamic t b)
  -> (b -> WidgetHost m a)
  -> JSVal -> cfg -> m ()
initPropWith setter lens_ build jsVal cfg = do
  schedulePostBuild $ do
    src <- build =<< sample (current (cfg^.lens_))
    setter jsVal src
  performEvent_ $
    fmap (build >=> setter jsVal)
    (updated (cfg^.lens_))

wrapObservableProp
  :: (ToJSVal a, FromJSVal b, MonadWidget t m)
  => String
  -> (a -> b)
  -> JSVal
  -> a
  -> Event t a
  -> m (Dynamic t b)
wrapObservableProp name wrap obj initialValue eSet = do
  let jName  = toJSString name
      err = fail "wrapObservableProp.set (fromJSVal)"
      set = liftIO . (toJSVal >=> googSet jName (pToJSVal False) obj)
      get = liftIO (googGet jName obj >>= fromJSVal >>= maybe err return)
  performEvent_ $ fmap set eSet
  schedulePostBuild $ set initialValue

  postGui <- askPostGui
  runWithActions <- askRunWithActions
  -- subscribe to change event
  ev <- newEventWithTrigger $ \trig -> do
    unsubscribe <- liftIO $ on_ ("change:"++name) obj $ \(_::JSVal) -> do
      val <- get
      postGui $ runWithActions [trig :=> val]
    return (liftIO unsubscribe)
  holdDyn (wrap initialValue) ev

initOLProp
  :: (ToJSVal a, MonadWidget t m)
  => String
  -> Lens' cfg (Dynamic t a)
  -> JSVal -> cfg -> m ()
initOLProp = initProp . olSetter

initOLPropWith
  :: (ToJSVal a, MonadWidget t m)
  => String
  -> Lens' cfg (Dynamic t b)
  -> (b -> WidgetHost m a)
  -> JSVal -> cfg -> m ()
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

foreign import javascript unsafe "$3['set']($1,$4,$2)"
  googSet :: JSString -> JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$2['get']($1)"
  googGet :: JSString -> JSVal -> IO JSVal

dynInitializeWith
  :: MonadWidget t m
  => (a -> m b) -> Dynamic t a -> (b -> WidgetHost m ()) -> m ()
dynInitializeWith build v f = addVoidAction . fmap f =<< dyn =<< mapDyn build v

dynInitialize
  :: MonadWidget t m
  => Dynamic t a -> (a -> WidgetHost m ()) -> m ()
dynInitialize = dynInitializeWith return
