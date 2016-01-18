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
  , vectorSource
  , anySource

  , featureCollection
  , featureCollection2

  , mkSource -- internal
) where

import Reflex
import Reflex.Dom
import Reflex.OpenLayers.Util
import Reflex.OpenLayers.Event
import Reflex.OpenLayers.Collection

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Proxy
import Data.Word (Word8)
import Data.Default(Default)
import Data.Typeable (Typeable, cast)
import qualified Data.Map as M
import Control.Monad (liftM, forM_, when)
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Marshal (ToJSVal(toJSVal), FromJSVal(fromJSVal))
import GHCJS.Types (JSVal, IsJSVal, JSString, jsval)
import GHCJS.DOM.Types hiding (Event)
import GHCJS.Foreign.QQ
import GHCJS.Foreign.Callback
import Sigym4.Geometry.Types hiding (Raster, Pixel)
import Sigym4.Geometry.Json (
    ToFeatureProperties
  , FromFeatureProperties
  , toJSON_crs
  )

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
  packSources   :: MonadWidget t m => s -> m JSVal
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

  VectorSource :: {
      _vectorFeatures  :: Collection t key (FeatureT g v s a)
    } -> Source Vector Image t

newtype JSSource = JSSource JSVal
  deriving (PToJSVal, PFromJSVal, ToJSVal, FromJSVal)
instance IsJSVal JSSource

anySource :: MonadWidget t m => Source r k t -> m JSSource
anySource = fmap JSSource . mkSource

newtype Pixel = Pixel JSVal deriving (PFromJSVal, PToJSVal)
red, green, blue, alpha :: Lens' Pixel Word8
red = lens (\p->[jsu'|$r=`p[0];|]) (\p v-> [jsu'|$r=`p;$r[0]=`v;|])
green = lens (\p->[jsu'|$r=`p[1];|]) (\p v-> [jsu'|$r=`p;$r[1]=`v;|])
blue = lens (\p->[jsu'|$r=`p[2];|]) (\p v-> [jsu'|$r=`p;$r[2]=`v;|])
alpha = lens (\p->[jsu'|$r=`p[3];|]) (\p v-> [jsu'|$r=`p;$r[3]=`v;|])



instance RasterOperation (Pixel -> Pixel) JSSource where
  applyOp f i = return (pToJSVal (f [jsu'|$r=`i[0];|]))
  packSources (JSSource s) = liftIO [jsu|$r=[`s];|]
  operationType _ = "pixel"




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

vectorSource
  :: Collection t key (FeatureT g v srid a) -> Source Vector Image t
vectorSource = VectorSource

mkSource :: MonadWidget t m => Source r k t -> m JSVal
mkSource s = do
  case s of
    ImageWMS{_imageWmsUrl, _imageWmsParams} ->
      liftIO [jsu|
        $r=new ol.source.ImageWMS(
          {url:`_imageWmsUrl, params:`_imageWmsParams});|]

    TileWMS{_tileWmsUrl, _tileWmsParams} ->
      liftIO [jsu|
        $r=new ol.source.TileWMS(
          {url:`_tileWmsUrl, params:`_tileWmsParams});|]

    MapQuest{_mapQuestLayer} ->
      liftIO [jsu|$r=new ol.source.MapQuest({layer:`_mapQuestLayer});|]

    OSM{} -> liftIO [jsu|$r=new ol.source.OSM();|]

    Raster{_rasterSources, _rasterOperation} -> do
      sources <- packSources _rasterSources
      liftIO $ do
        let typ = operationType _rasterOperation
        cb <- fmap jsval $ syncCallback1' (applyOp _rasterOperation)
        -- TODO Release callback on gc
        [jsu|$r=new ol.source.Raster({ sources:`sources
                                     , operation:`cb
                                     , threads: 0
                                     , operationType: `typ});|]

    VectorSource{_vectorFeatures} -> do
      liftIO [jsu|$r=new ol.source.Vector({features:`_vectorFeatures});|]

featureCollection
  :: ( MonadWidget t m, Ord k, Enum k, KnownNat srid
     , FromFeatureProperties d, ToFeatureProperties d
     , FromJSON (g v srid), ToJSON (g v srid)
     )
  => Dynamic t (M.Map k (FeatureT g v srid d))
  -> m (Collection t k (FeatureT g v srid d))
featureCollection = collectionWith toJSVal_feature fromJSVal_feature

featureCollection2
  :: ( MonadWidget t m, Ord k, Enum k, KnownNat srid
     , VectorSpace v , FromFeatureProperties d, ToFeatureProperties d
     , FromJSON (g v srid), ToJSON (g v srid)
     )
  => M.Map k (FeatureT g v srid d)
  -> Event t (M.Map k (Maybe (FeatureT g v srid d)))
  -> m (Collection t k (FeatureT g v srid d))
featureCollection2 =
  collectionWith2 toJSVal_feature fromJSVal_feature $ \(h,f) -> do
    geom :: JSVal <- liftIO [jsu|$r=`f.getGeometry();|]
    wrapOLEvent_ "change" geom $ do
      geom' <- [jsu|$r=`f.getGeometry();|]
      newGeom <- maybe (fail "could not read geometry") return
                  =<< fromJSVal_geometry geom'
      return $ h & geometry .~ newGeom

toJSVal_feature
  :: forall g v srid d.
     ( KnownNat srid
     , ToJSON (g v srid)
     , ToFeatureProperties d
     )
  => FeatureT g v srid d -> IO JSVal
toJSVal_feature f = do
  jsF <- toJSVal (toJSON_crs (Proxy::Proxy srid) f)
  [js|$r=(new ol.format.GeoJSON()).readFeature(`jsF);|]

fromJSVal_feature
  :: ( FromFeatureProperties d , FromJSON (g v srid))
  => JSVal -> IO (Maybe (FeatureT g v srid d))
fromJSVal_feature j = do
  geoJ <- [js|$r=(new ol.format.GeoJSON()).writeFeatureObject(`j);|]
  val <- fromJSVal geoJ
  return (val >>= parseMaybe parseJSON)

fromJSVal_geometry
  :: (VectorSpace v, KnownNat srid, FromJSON (g v srid))
  => JSVal -> IO (Maybe (g v srid))
fromJSVal_geometry j = do
  geoJ <- [js|$r=(new ol.format.GeoJSON()).writeGeometryObject(`j);|]
  val <- fromJSVal geoJ
  return (val >>= parseMaybe parseJSON)

makeFields ''Pixel
