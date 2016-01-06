{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Reflex.OpenLayers.Source (
    Source (..)
  , MapQuestLayer (..)
  , imageWMS
  , mapQuest
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
      _url    :: String
    , _params :: M.Map String String
    }
  | MapQuest {
      _layer  :: MapQuestLayer
    }
  deriving Show
makeLenses ''Source

instance SyncJS (Source t) t where
  syncJS jsObj newHS | fastEq jsObj newHS = return Nothing
  syncJS jsObj newHS@ImageWMS{_url, _params} = liftIO $ do
    setStableName jsObj newHS
    when ([jsu'|$r=`jsObj.getUrl();|] /= _url)
      [jsu_|`jsObj.setUrl(`_url);|]
    when ([jsu'|$r=`jsObj.getParams();|] /= _params)
      [jsu_|`jsObj.updateParams(`_params);|]
    return Nothing

  syncJS jsObj newHS@MapQuest{_layer}
    | [jsu'|$r=`jsObj.getLayer()!==`_layer;|] = liftM Just (mkSource newHS)
    | otherwise                               = return Nothing


imageWMS
  :: Reflex t
  => String -> (M.Map String String) -> Source t
imageWMS = ImageWMS

mapQuest :: Reflex t => MapQuestLayer -> Source t
mapQuest = MapQuest

mkSource :: MonadWidget t m => Source t -> m JSVal
mkSource s = liftIO $ do
  r <- case s of
    ImageWMS{_url, _params} -> do
      [jsu|$r=new ol.source.ImageWMS({url:`_url, params:`_params});|]
    MapQuest{_layer} ->
      [jsu|$r=new ol.source.MapQuest({layer:`_layer});|]
  setStableName r s
  return r
