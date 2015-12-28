{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}

module Reflex.OpenLayers.Source (
    IsSource (..)
  , Source
  , IsSourceConfig (..)
  , SourceConfig

  , ImageWMSConfig (..)
  , ImageWMS (..)
  , imageWMS

  , MapQuestConfig (..)
  , MapQuestLayer (..)
  , MapQuest (..)
  , mapQuest
) where

import qualified JavaScript.Object as O
import Data.Typeable (Typeable, cast)
import qualified Data.Map as M
import Control.Monad (liftM, forM_)
import Control.Monad.IO.Class (liftIO)
import GHCJS.Marshal (ToJSVal(toJSVal))
import GHCJS.Marshal.Pure (pToJSVal)
import GHCJS.Types (JSVal, JSString, jsval)
import GHCJS.DOM.Types
import Reflex.Dom (Reflex, MonadWidget)

class IsSourceConfig l where
  source :: MonadWidget t m => l t -> m (Source t)

data SourceConfig t =  forall l. IsSourceConfig l => SourceConfig (l t)

instance IsSourceConfig SourceConfig where
  source (SourceConfig s) = source s

class Typeable l => IsSource l where
  source_downcast :: Typeable t => Source t -> Maybe (l t)
  source_downcast (Source l) = cast l
  {-# INLINE source_downcast #-}

data Source t =  forall l. (ToJSVal (l t), IsSource l) => Source (l t)



instance ToJSVal (Source t) where
  toJSVal (Source l) = toJSVal l

--
-- ImageWMS
--

newtype ImageWMS t = ImageWMS JSVal deriving (ToJSVal, Typeable)

instance IsSource ImageWMS

instance IsSourceConfig ImageWMSConfig where
  source ImageWMSConfig{..} = liftIO $ do
    jsUrl <- toJSVal _imageWMSConfig_url
    jsParams <- mapToJSVal _imageWMSConfig_params
    opts <- O.create
    O.setProp "url" jsUrl opts
    O.setProp "params" jsParams opts
    liftM (Source . ImageWMS) (mkSource "ImageWMS" opts)

mapToJSVal m = do
  o <- O.create
  forM_ (M.toList m) $ \(k,v) -> do
    O.setProp (toJSString k) (pToJSVal v) o
  return (jsval o)

data ImageWMSConfig t
  = ImageWMSConfig {
      _imageWMSConfig_url    :: String
    , _imageWMSConfig_params :: M.Map String String
    }

imageWMS :: String -> M.Map String String -> SourceConfig t
imageWMS url params = SourceConfig (ImageWMSConfig url params)

--
-- MapQuest
--

data MapQuestLayer = OpenStreetMap | Satellite | Hybrid

instance ToJSVal MapQuestLayer where
  toJSVal OpenStreetMap = return (jsval ("osm" :: JSString))
  toJSVal Satellite = return (jsval ("sat" :: JSString))
  toJSVal Hybrid = return (jsval ("hyb" :: JSString))

newtype MapQuest t = MapQuest JSVal deriving (ToJSVal, Typeable)

instance IsSource MapQuest

instance IsSourceConfig MapQuestConfig where
  source MapQuestConfig{..} = liftIO $ do
    opts <- O.create
    params <- O.create
    layer <- toJSVal _mapQuestConfig_layer
    O.setProp "layer" layer opts
    liftM (Source . MapQuest) (mkSource "MapQuest" opts)

data MapQuestConfig t
  = MapQuestConfig {
      _mapQuestConfig_layer :: MapQuestLayer
    }

mapQuest :: MapQuestLayer -> SourceConfig t
mapQuest layer = SourceConfig (MapQuestConfig layer)

foreign import javascript unsafe "new ol['source'][$1]($2)"
  mkSource :: JSString -> O.Object -> IO JSVal
