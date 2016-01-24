{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.OpenLayers.Projection (
    Projection(..)
  , toJSVal_projection

  , SphericalMercator
  , sphericalMercator
  , CRS84
  , crs84

, module SpatialReference
) where

import SpatialReference

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Default (Default(def))
import Data.Proxy
import GHCJS.Marshal (ToJSVal(toJSVal), FromJSVal(fromJSVal))
import GHCJS.Foreign.QQ
import GHCJS.Types

newtype Projection = Projection {unProj :: Crs}
  deriving (Eq, Show, Ord, ToJSON, FromJSON)

instance Default Projection where def = sphericalMercator

type SphericalMercator = Named "urn:ogc:def:crs:EPSG::3857"
type CRS84             = Epsg 4326
sphericalMercator, crs84 :: Projection
sphericalMercator = Projection (reflectCrs (Proxy :: Proxy SphericalMercator))
crs84 = Projection (reflectCrs (Proxy :: Proxy CRS84))

toJSVal_projection :: MonadIO m => Projection -> m JSVal
toJSVal_projection crs = do
  olProj <- liftIO $ do
    jVal <- toJSVal (toJSON crs)
    [js|$r=(new ol.format.GeoJSON()).readProjection({crs:`jVal});|]
  if isUndefined olProj
      then fail ("toJSVal_projection: openlayers did not recognize "
                 ++ show crs)
      else return olProj

instance FromJSVal Projection where
  fromJSVal jVal = do
    let olProj = [jsu'|$r=ol.proj.get(`jVal);|]
    return $ if isUndefined olProj
      then Nothing
      else (Just (Projection (namedCrs [jsu'|$r=`olProj.getCode()|])))
