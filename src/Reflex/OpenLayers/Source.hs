{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.OpenLayers.Source (
    Source (..)
  , MapQuestLayer (..)
  , Tile
  , Image
  , Raster
  , Vector
  , RasterOperation (..)
  , Pixel
  , red
  , green
  , blue
  , alpha

  , imageWMS
  , tileWMS
  , mapQuest
  , osm
  , raster
  , anySource

  , mkSource -- internal
) where

import Reflex
import Reflex.Dom
import Reflex.OpenLayers.Util

import Data.Word (Word8)
import Data.Default(Default)
import Data.Typeable (Typeable, cast)
import qualified Data.Map as M
import Control.Monad (liftM, forM_, when)
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Marshal (toJSVal, fromJSVal)
import GHCJS.Types (JSVal, JSString, jsval)
import GHCJS.DOM.Types
import GHCJS.Foreign.QQ
import GHCJS.Foreign.Callback

data TileK = TileK | ImageK
type Tile = 'TileK
type Image = 'ImageK

data SourceK = RasterK | VectorK
type Raster = 'RasterK
type Vector = 'VectorK

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

class RasterOperation f s | f->s, s->f where
  applyOp       :: f -> JSVal -> IO JSVal
  packSources   :: s -> IO JSVal
  operationType :: f -> String

data Source (r::SourceK) (k::TileK) t where
  ImageWMS :: {
      _imageWmsUrl    :: String
    , _imageWmsParams :: M.Map String String
    } -> Source Raster Image t

  TileWMS :: {
      _tileWmsUrl    :: String
    , _tileWmsParams :: M.Map String String
    } -> Source Raster Tile t

  MapQuest :: {
      _mapQuestLayer  :: MapQuestLayer
    } -> Source Raster Tile t

  OSM :: Source Raster Tile t

  Raster :: RasterOperation o s => {
      _rasterOperation :: o
    , _rasterSources   :: s
    } -> Source Raster Image t

newtype AnySource = AnySource (IO JSVal)

anySource :: Source r k t -> AnySource
anySource s = AnySource (mkSource s)

newtype Pixel = Pixel JSVal deriving (PFromJSVal, PToJSVal)
red, green, blue, alpha :: Lens' Pixel Word8
red = lens (\p->[jsu'|$r=`p[0];|]) (\p v-> [jsu'|`p[0]=`v; $r=`p;|])
green = lens (\p->[jsu'|$r=`p[0];|]) (\p v-> [jsu'|`p[0]=`v; $r=`p;|])
blue = lens (\p->[jsu'|$r=`p[0];|]) (\p v-> [jsu'|`p[0]=`v; $r=`p;|])
alpha = lens (\p->[jsu'|$r=`p[0];|]) (\p v-> [jsu'|`p[0]=`v; $r=`p;|])



instance RasterOperation (Pixel -> Pixel) AnySource where
  applyOp f i = return (pToJSVal (f [jsu'|$r=`i[0];|]))
  packSources (AnySource s) = s >>= \s2 -> [jsu|$r=[`s2];|]
  operationType _ = "pixel"



instance SyncJS (Source r k t) t where
  syncJS jsObj newHS | fastEq jsObj newHS = return Nothing
  syncJS jsObj newHS = do
    setStableName jsObj newHS
    case newHS of
      ImageWMS{ _imageWmsUrl=url, _imageWmsParams=params} -> liftIO $ do
        when ([jsu'|$r=`jsObj.get('url');|] /= url)
          [jsu_|`jsObj.setUrl(`url);|]
        when ([jsu'|$r=`jsObj.getParams();|] /= params)
          [jsu_|`jsObj.updateParams(`params);|]
        return Nothing

      TileWMS{ _tileWmsUrl=url, _tileWmsParams=params} -> liftIO $ do
        when ([jsu'|$r=`jsObj.get('url');|] /= url)
          [jsu_|`jsObj.setUrl(`url);|]
        when ([jsu'|$r=`jsObj.getParams();|] /= params)
          [jsu_|`jsObj.updateParams(`params);|]
        return Nothing

      MapQuest{_mapQuestLayer}
        | [jsu'|$r=`jsObj.getLayer()!==`_mapQuestLayer;|] ->
          liftM Just (mkSource newHS)
        | otherwise                               -> return Nothing

      OSM -> return Nothing

      Raster {} -> liftM Just (mkSource newHS)



imageWMS
  :: String -> M.Map String String -> Source Raster Image t
imageWMS = ImageWMS

tileWMS
  :: String -> M.Map String String -> Source Raster Tile t
tileWMS = TileWMS


mapQuest :: MapQuestLayer -> Source Raster Tile t
mapQuest = MapQuest

osm :: Source Raster Tile t
osm = OSM

raster :: RasterOperation o s => o -> s -> Source Raster Image t
raster op = Raster op

mkSource :: MonadIO m => Source r k t -> m JSVal
mkSource s = liftIO $ do
  r <- case s of
    ImageWMS{_imageWmsUrl, _imageWmsParams} ->
      [jsu|
        $r=new ol.source.ImageWMS(
          {url:`_imageWmsUrl, params:`_imageWmsParams});|]

    TileWMS{_tileWmsUrl, _tileWmsParams} ->
      [jsu|
        $r=new ol.source.TileWMS(
          {url:`_tileWmsUrl, params:`_tileWmsParams});|]

    MapQuest{_mapQuestLayer} ->
      [jsu|$r=new ol.source.MapQuest({layer:`_mapQuestLayer});|]

    OSM{} -> [jsu|$r=new ol.source.OSM({});|]

    Raster{_rasterSources, _rasterOperation} -> do
      sources <- packSources _rasterSources
      let typ = operationType _rasterOperation
      cb <- fmap jsval $ syncCallback1' (applyOp _rasterOperation)
      -- TODO Release callback on gc
      [jsu|$r=new ol.source.Raster({ sources:`sources
                                   , operation:`cb
                                   , threads: 0
                                   , operationType: `typ});|]
  setStableName r s
  return r

makeFields ''Pixel
