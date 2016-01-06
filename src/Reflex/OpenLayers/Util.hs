{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.OpenLayers.Util (
    SyncJS (..)
  , dynInitialize
  , dynInitializeWith
  , syncJS_
  , syncEqProp
  ) where

import Reflex.OpenLayers.Event

import Reflex
import Reflex.Dom

import Control.Arrow
import Control.Monad (forM_, when, void, (>=>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Map as M
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Marshal (fromJSVal)
import GHCJS.Foreign.QQ
import GHCJS.Types
import GHCJS.DOM.Types (toJSString)
import qualified JavaScript.Object as O
import System.IO.Unsafe (unsafePerformIO)

class SyncJS a t where
  syncJS :: MonadWidget t m => JSVal -> a -> m (Maybe JSVal)

syncJS_ js =
  syncJS js >=> maybe (return ()) (const (error "syncJS_: expected Nothing"))

syncEqProp :: (MonadIO m, Eq a, PToJSVal a, PFromJSVal a)
           => JSVal -> JSString -> a -> m ()
syncEqProp jsObj name val = liftIO $
  when ([jsu'|$r=`jsObj.get(`name);|] /= val && not (isNull (pToJSVal val)))
    [jsu_|`jsObj.set(`name, `val);|]
{-# INLINE syncEqProp #-}

instance PToJSVal a => PToJSVal (M.Map String a) where
  pToJSVal m = unsafePerformIO $ do
    o <- O.create
    forM_ (M.toList m) $ \(k,v) -> do
      O.setProp (toJSString k) (pToJSVal v) o
    return (jsval o)

instance (Show a, PFromJSVal a) => PFromJSVal (M.Map String a) where
  pFromJSVal = M.fromList
             . maybe (error "should not happen") toAssocs
             . unsafePerformIO
             . fromJSVal
             . keyValues

    where
      toAssocs [] = []
      toAssocs (_:[]) = error "pFromJSVal: should not happen;"
      toAssocs (k:v:r) = (pFromJSVal k, pFromJSVal v):toAssocs r
      keyValues :: JSVal -> JSVal
      keyValues o = [jsu'|
        $r=[];
        for (var k in `o) {$r.push(k); $r.push(`o[k]);}
        |]

dynInitializeWith
  :: MonadWidget t m
  => (a -> m b) -> Dynamic t a -> (b -> WidgetHost m ()) -> m ()
dynInitializeWith build v f = addVoidAction . fmap f =<< dyn =<< mapDyn build v

dynInitialize
  :: MonadWidget t m
  => Dynamic t a -> (a -> WidgetHost m ()) -> m ()
dynInitialize = dynInitializeWith return
