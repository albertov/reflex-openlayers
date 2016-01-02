{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Reflex.OpenLayers (
    Map
  , MapConfig
  , ViewConfig
  , View
  , HasAttributes(..)
  , HasMapView (..)
  , HasResolution (..)
  , HasSetResolution (..)
  , HasCenter (..)
  , HasSetCenter (..)
  , HasRotation (..)
  , HasSetRotation (..)
  , map
  , css
) where

import Reflex.OpenLayers.Layer
import Reflex.OpenLayers.Util

import Control.Lens (lens, (^.))
import Control.Monad (liftM, (>=>))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Default (Default)
import Data.Typeable (Typeable)
import Data.FileEmbed (embedFile)
import qualified Data.Map as M
import Data.Monoid

import qualified JavaScript.Object as O
import GHCJS.DOM.HTMLDivElement (castToHTMLDivElement)
import GHCJS.DOM.Element (toElement)
import GHCJS.DOM.Types (unElement)
import GHCJS.Types (JSVal)
import GHCJS.Marshal (ToJSVal(toJSVal))
import Reflex.Dom

import Prelude hiding (map)

-- Property lenses

declareProperties [
    "mapView"
  , "resolution"
  , "setResolution"
  , "center"
  , "setCenter"
  , "rotation"
  , "setRotation"
  ]

--
-- View
--

type Coordinates = (Double, Double)
type Resolution  = Double
type Rotation    = Double

data ViewConfig t
  = ViewConfig {
      _viewConfig_center        :: Coordinates
    , _viewConfig_resolution    :: Resolution
    , _viewConfig_rotation      :: Rotation
    , _viewConfig_setResolution :: Event t Resolution
    , _viewConfig_setCenter     :: Event t Coordinates
    , _viewConfig_setRotation   :: Event t Rotation
    }
hasProperties ''ViewConfig [
    "resolution"
  , "setResolution"
  , "center"
  , "setCenter"
  , "rotation"
  , "setRotation"
  ]

instance Reflex t => Default (ViewConfig t) where
  def = ViewConfig {
      _viewConfig_center        = (0,0)
    , _viewConfig_resolution    = 1
    , _viewConfig_rotation      = 0
    , _viewConfig_setCenter     = never
    , _viewConfig_setResolution = never
    , _viewConfig_setRotation   = never
    }

data View t
  = View {
      _view_center     :: Dynamic t (Maybe Coordinates)
    , _view_resolution :: Dynamic t (Maybe Resolution)
    , _view_rotation   :: Dynamic t Rotation
    , _view_jsval      :: JSVal
    }
hasProperties ''View [
    "resolution"
  , "center"
  , "rotation"
  ]

view :: MonadWidget t m => ViewConfig t -> m (View t)
view cfg = do
  jsView <- liftIO newView
  dynResolution <- wrapObservableProp "resolution"
               Just jsView (cfg^.resolution) (cfg^.setResolution)
  dynCenter <- wrapObservableProp "center"
                Just jsView (cfg^.center) (cfg^.setCenter)
  dynRotation <- wrapObservableProp "rotation"
               id jsView (cfg^.rotation) (cfg^.setRotation)

  return View {
      _view_center      = dynCenter
    , _view_resolution  = dynResolution
    , _view_rotation    = dynRotation
    , _view_jsval       = jsView
    }

--
-- ol.View
--
foreign import javascript unsafe "new ol['View']({})" newView :: IO JSVal


--
-- Map
--

data MapConfig t
  = MapConfig {
      _mapConfig_attributes :: Dynamic t (M.Map String String)
    , _mapConfig_mapView    :: ViewConfig t
    , _mapConfig_layers     :: Dynamic t [Layer t]
    }
hasProperties ''MapConfig [
    "mapView"
  , "layers"
  ]

instance HasAttributes (MapConfig t) where
  type Attrs (MapConfig t) = Dynamic t (M.Map String String)
  attributes = lens _mapConfig_attributes (\o v -> o {_mapConfig_attributes=v})

instance HasCenter (MapConfig t) (Coordinates) where
  center = mapView . center
instance HasSetCenter (MapConfig t) (Event t Coordinates) where
  setCenter = mapView . setCenter
instance HasResolution (MapConfig t) Resolution where
  resolution = mapView . resolution
instance HasSetResolution (MapConfig t) (Event t Resolution) where
  setResolution = mapView . setResolution
instance HasRotation (MapConfig t) (Rotation) where
  rotation = mapView . rotation
instance HasSetRotation (MapConfig t) (Event t Rotation) where
  setRotation = mapView . setRotation

instance Reflex t => Default (MapConfig t) where
  def = MapConfig {
        _mapConfig_attributes  = constDyn mempty
      , _mapConfig_mapView     = def
      , _mapConfig_layers      = constDyn []
    }

data Map t
  = Map {
      _map_jsval   :: JSVal
    , _map_mapView :: View t
    }
hasProperties ''Map [
    "mapView"
  ]


instance HasCenter (Map t) (Dynamic t (Maybe Coordinates)) where
  center = mapView . center
instance HasResolution (Map t) (Dynamic t (Maybe Resolution)) where
  resolution = mapView . resolution
instance HasRotation (Map t) (Dynamic t Rotation) where
  rotation = mapView . rotation


map :: (Typeable t, MonadWidget t m) => MapConfig t -> m (Map t)
map cfg = do
  element <- liftM castToHTMLDivElement $
               buildEmptyElement "div" (cfg^.attributes)
  let jsTarget = unElement (toElement element)
  v <- view (cfg^.mapView)
  jsMap <- liftIO $ do
    opts <- O.create
    O.setProp "view" (_view_jsval v) opts
    olMap opts
  initProp (const (liftIO . (toJSVal >=> olMap_setLayers jsMap)))
           layers jsMap cfg
  postBuild <- getPostBuild
  performEvent_ $
    fmap (const (liftIO $ olMap_setTarget jsMap jsTarget)) postBuild
  return Map {
        _map_mapView = v
      , _map_jsval   = jsMap
    }


--
-- ol.Map
--
foreign import javascript unsafe "new ol['Map']($1)"
  olMap :: O.Object -> IO JSVal

foreign import javascript unsafe "$1['setTarget']($2)"
  olMap_setTarget :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1['getLayerGroup']()['setLayers'](new ol.Collection($2))"
  olMap_setLayers :: JSVal -> JSVal -> IO ()

--
-- CSS
--


css :: ByteString
css = $(embedFile "static/ol.css")
