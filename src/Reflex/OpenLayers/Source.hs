{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Reflex.OpenLayers.Source (
    Source (..)
  , MapQuestLayer (..)

  , imageWMS
  , tileWMS
  , mapQuest
  , osm

  , mkSource
  , HasUrl (..)
  , HasParams (..)
  , HasMapQuestLayer (..)
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

data Source t
  = ImageWMS {
      _sourceUrl    :: String
    , _sourceParams :: M.Map String String
    }
  | TileWMS {
      _sourceUrl    :: String
    , _sourceParams :: M.Map String String
    }
  | MapQuest {
      _sourceMapQuestLayer  :: MapQuestLayer
    }
  | OSM
  deriving Show
makeFields ''Source

instance SyncJS (Source t) t where
  syncJS jsObj newHS | fastEq jsObj newHS = return Nothing
  syncJS jsObj newHS = do
    setStableName jsObj newHS
    case newHS of
      MapQuest{}
        | Just l <- newHS^?mapQuestLayer, [jsu'|$r=`jsObj.getLayer()!==`l;|] ->
          liftM Just (mkSource newHS)

      _ | Just u <- newHS^?url, [jsu'|$r=`jsObj.getUrl();|] /= u -> do
          liftIO [jsu_|`jsObj.setUrl(`u);|]
          return Nothing

      _ | Just p <- newHS^?params, [jsu'|$r=`jsObj.getParams();|] /= p -> do
          liftIO [jsu_|`jsObj.updateParams(`p);|]
          return Nothing

      _ -> return Nothing



imageWMS
  :: Reflex t
  => String -> (M.Map String String) -> Source t
imageWMS = ImageWMS

tileWMS
  :: Reflex t
  => String -> (M.Map String String) -> Source t
tileWMS = TileWMS

mapQuest :: Reflex t => MapQuestLayer -> Source t
mapQuest = MapQuest

osm :: Reflex t => Source t
osm = OSM

mkSource :: MonadWidget t m => Source t -> m JSVal
mkSource s = liftIO $ do
  r <- case s of
    ImageWMS{_sourceUrl=u, _sourceParams=p} ->
      [jsu|$r=new ol.source.ImageWMS({url:`u, params:`p});|]

    TileWMS{_sourceUrl=u, _sourceParams=p} ->
      [jsu|$r=new ol.source.TileWMS({url:`u, params:`p});|]

    MapQuest{_sourceMapQuestLayer} ->
      [jsu|$r=new ol.source.MapQuest({layer:`_sourceMapQuestLayer});|]

    OSM -> [jsu|$r=new ol.source.OSM({});|]

  setStableName r s
  return r
