{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.OpenLayers.Projection (Projection(..)) where

import Sigym4.Geometry

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import GHCJS.Marshal (ToJSVal(toJSVal), FromJSVal(fromJSVal))
import GHCJS.Foreign.QQ
import GHCJS.Types

newtype Projection = Projection {unProj :: Crs}
  deriving (Eq, Show, Ord, ToJSON, FromJSON)

instance ToJSVal Projection where
  toJSVal crs = do
    jVal <- toJSVal (toJSON crs)
    [js|$r=(new ol.format.GeoJSON()).readProjection({crs:`jVal});|]

instance FromJSVal Projection where
  fromJSVal jVal = do
    let olProj = [jsu'|$r=ol.proj.get(`jVal);|]
    return $ if isUndefined olProj
      then Nothing
      else (Just (Projection (namedCrs [jsu'|`olProj.getCode()|])))
