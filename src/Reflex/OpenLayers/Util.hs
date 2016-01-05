{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.OpenLayers.Util (
    dynInitialize
  , dynInitializeWith
  ) where

import Reflex.OpenLayers.Event

import Reflex
import Reflex.Dom

import Control.Monad (forM_)
import qualified Data.Map as M
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Foreign.QQ
import GHCJS.Types
import GHCJS.DOM.Types (toJSString)
import qualified JavaScript.Object as O
import System.IO.Unsafe (unsafePerformIO)

instance PToJSVal a => PToJSVal (M.Map String a) where
  pToJSVal m = unsafePerformIO $ do
    o <- O.create
    forM_ (M.toList m) $ \(k,v) -> do
      O.setProp (toJSString k) (pToJSVal v) o
    return (jsval o)

dynInitializeWith
  :: MonadWidget t m
  => (a -> m b) -> Dynamic t a -> (b -> WidgetHost m ()) -> m ()
dynInitializeWith build v f = addVoidAction . fmap f =<< dyn =<< mapDyn build v

dynInitialize
  :: MonadWidget t m
  => Dynamic t a -> (a -> WidgetHost m ()) -> m ()
dynInitialize = dynInitializeWith return
