{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Reflex.OpenLayers.Source (
    Source (..)
  , MapQuestLayer (..)
  , Tiled
  , NotTiled
  , Raster
  , Vector

  , imageWMS
  , tileWMS
  , mapQuest
  , osm

  , mkSource
) where

import Reflex
import Reflex.Dom
import Reflex.OpenLayers.Util

import Data.Default(Default)
import Data.Typeable (Typeable, cast)
import qualified Data.Map as M
import Control.Monad (liftM, forM_, when)
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Types (JSVal, JSString, jsval)
import GHCJS.DOM.Types
import GHCJS.Foreign.QQ

data TileK = Tiled | NotTiled
type Tiled = 'Tiled
type NotTiled = 'NotTiled

data SourceK = Raster | Vector
type Raster = 'Raster
type Vector = 'Vector

data MapQuestLayer
  = OpenStreetMap
  | Satellite
  | Hybrid
  deriving (Show, Read, Enum, Bounded, Eq)

instance PToJSVal MapQuestLayer where
  pToJSVal OpenStreetMap = jsval ("osm" :: JSString)
  pToJSVal Satellite     = jsval ("sat" :: JSString)
  pToJSVal Hybrid        = jsval ("hyb" :: JSString)

instance PFromJSVal MapQuestLayer where
  pFromJSVal s =
    case (pFromJSVal s :: JSString) of
      "sat" -> Satellite
      "osm" -> OpenStreetMap
      "hyb" -> Hybrid
      _     -> error "pFromJSVal(MapQuestLayer): Unexpected layer name"

instance Default MapQuestLayer where def = Satellite


data Source (r::SourceK) (k::TileK) t where
  ImageWMS :: {
      _imageWmsUrl    :: String
    , _imageWmsParams :: M.Map String String
    } -> Source Raster NotTiled t
  TileWMS :: {
      _tileWmsUrl    :: String
    , _tileWmsParams :: M.Map String String
    } -> Source Raster Tiled t
  MapQuest :: {
      _layer  :: MapQuestLayer
    } -> Source Raster Tiled t
  OSM :: Source Raster Tiled t
deriving instance Show (Source r k t)


instance SyncJS (Source r k t) t where
  syncJS jsObj newHS | fastEq jsObj newHS = return Nothing
  syncJS jsObj newHS = do
    setStableName jsObj newHS
    case newHS of
      ImageWMS{ _imageWmsUrl=url, _imageWmsParams=params} -> liftIO $ do
        when ([jsu'|$r=`jsObj.getUrl();|] /= url)
          [jsu_|`jsObj.setUrl(`url);|]
        when ([jsu'|$r=`jsObj.getParams();|] /= params)
          [jsu_|`jsObj.updateParams(`params);|]
        return Nothing

      TileWMS{ _tileWmsUrl=url, _tileWmsParams=params} -> liftIO $ do
        when ([jsu'|$r=`jsObj.getUrl();|] /= url)
          [jsu_|`jsObj.setUrl(`url);|]
        when ([jsu'|$r=`jsObj.getParams();|] /= params)
          [jsu_|`jsObj.updateParams(`params);|]
        return Nothing

      MapQuest{_layer}
        | [jsu'|$r=`jsObj.getLayer()!==`_layer;|] ->
          liftM Just (mkSource newHS)
        | otherwise                               -> return Nothing

      OSM -> return Nothing


imageWMS
  :: Reflex t
  => String -> (M.Map String String) -> Source Raster NotTiled t
imageWMS = ImageWMS

tileWMS
  :: Reflex t
  => String -> (M.Map String String) -> Source Raster Tiled t
tileWMS = TileWMS


mapQuest :: Reflex t => MapQuestLayer -> Source Raster Tiled t
mapQuest = MapQuest

osm :: Reflex t => Source Raster Tiled t
osm = OSM

mkSource :: MonadWidget t m => Source r k t -> m JSVal
mkSource s = liftIO $ do
  r <- case s of
    ImageWMS{_imageWmsUrl, _imageWmsParams} -> do
      [jsu|$r=new ol.source.ImageWMS(
        {url:`_imageWmsUrl, params:`_imageWmsParams});|]
    TileWMS{_tileWmsUrl, _tileWmsParams} -> do
      [jsu|$r=new ol.source.TileWMS(
        {url:`_tileWmsUrl, params:`_tileWmsParams});|]
    MapQuest{_layer} ->
      [jsu|$r=new ol.source.MapQuest({layer:`_layer});|]
    OSM{} ->
      [jsu|$r=new ol.source.OSM({});|]
  setStableName r s
  return r
