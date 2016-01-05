{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Reflex.OpenLayers.Source (
    Source (..)
  , MapQuestLayer (..)
  , imageWMS
  , mapQuest
  , mkSource
) where

import Reflex
import Reflex.Dom
import Reflex.OpenLayers.Util (dynInitialize)

import Data.Default(Default)
import Data.Typeable (Typeable, cast)
import qualified Data.Map as M
import Control.Monad (liftM, forM_)
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

instance Default MapQuestLayer where def = Satellite

data Source t
  = ImageWMS {
      _url    :: String
    , _params :: M.Map String String
    }
  | MapQuest {
      _layer  :: MapQuestLayer
    }
makeLenses ''Source

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
  return r
