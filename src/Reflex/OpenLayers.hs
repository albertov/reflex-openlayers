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
  , map
  , css
) where

import Reflex.OpenLayers.Event (on_)
import Reflex.OpenLayers.Layer
import Reflex.OpenLayers.Util (initProp, initGoogProp)

import Control.Lens (Lens', makeLenses, (^.))
import Control.Monad (liftM, (>=>))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Default (Default(def))
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import Data.FileEmbed (embedFile)
import qualified Data.Map as M
import Data.Monoid
import Data.Dependent.Sum (DSum (..))

import qualified JavaScript.Object as O
import GHCJS.DOM.HTMLDivElement (castToHTMLDivElement)
import GHCJS.DOM.Element (toElement)
import GHCJS.DOM.Types (unElement)
import GHCJS.Types (JSVal, JSString)
import GHCJS.Marshal (ToJSVal(toJSVal), fromJSVal)
import GHCJS.Marshal.Pure (pToJSVal, pFromJSVal)
import Reflex.Host.Class (newEventWithTrigger)
import Reflex.Dom
import Reflex.Dom.Widget.Input (HasAttributes(..))

import Prelude hiding (map)

-- Property lenses

class HasMapView l v | l->v where
  mapView :: Lens' l v

class HasResolution l v | l->v where
  resolution :: Lens' l v

class HasSetResolution l v | l->v where
  setResolution :: Lens' l v

class HasCenter l v | l->v where
  center :: Lens' l v

class HasSetCenter l v | l->v where
  setCenter :: Lens' l v

class HasRotation l v | l->v where
  rotation :: Lens' l v
class HasSetRotation l v | l->v where
  setRotation :: Lens' l v


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
makeLenses ''ViewConfig

instance HasCenter (ViewConfig t) (Coordinates) where
  center = viewConfig_center
instance HasResolution (ViewConfig t) Resolution where
  resolution = viewConfig_resolution
instance HasSetResolution (ViewConfig t) (Event t Resolution) where
  setResolution = viewConfig_setResolution
instance HasSetCenter (ViewConfig t) (Event t Coordinates) where
  setCenter = viewConfig_setCenter
instance HasRotation (ViewConfig t) (Rotation) where
  rotation = viewConfig_rotation
instance HasSetRotation (ViewConfig t) (Event t Rotation) where
  setRotation = viewConfig_setRotation

instance Reflex t => Default (ViewConfig t) where
  def = ViewConfig {
      _viewConfig_center      = (0,0)
    , _viewConfig_resolution  = 1
    , _viewConfig_rotation    = 0
    , _viewConfig_setCenter   = never
    , _viewConfig_setResolution     = never
    , _viewConfig_setRotation = never
    }

data View t
  = View {
      _view_center   :: Dynamic t (Maybe Coordinates)
    , _view_resolution     :: Dynamic t (Maybe Resolution)
    , _view_rotation :: Dynamic t Rotation
    , _view_jsval    :: JSVal
    }
makeLenses ''View

instance HasCenter (View t) (Dynamic t (Maybe Coordinates)) where
  center = view_center
instance HasResolution (View t) (Dynamic t (Maybe Resolution)) where
  resolution = view_resolution
instance HasRotation (View t) (Dynamic t Rotation) where
  rotation = view_rotation

view :: MonadWidget t m => ViewConfig t -> m (View t)
view cfg = do
  jsView <- liftIO newView
  dynResolution <- initGoogProp "resolution"
               Just jsView (cfg^.resolution) (cfg^.setResolution)
  dynCenter <- initGoogProp "center"
                Just jsView (cfg^.center) (cfg^.setCenter)
  dynRotation <- initGoogProp "rotation"
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
    , _mapConfig_view       :: ViewConfig t
    , _mapConfig_layers     :: Dynamic t [Layer t]
    }
makeLenses ''MapConfig

instance HasAttributes (MapConfig t) where
  type Attrs (MapConfig t) = Dynamic t (M.Map String String)
  attributes = mapConfig_attributes

instance HasMapView (MapConfig t) (ViewConfig t) where
  mapView = mapConfig_view

instance HasLayers (MapConfig t) (Dynamic t [Layer t]) where
  layers = mapConfig_layers

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
      , _mapConfig_view        = def
      , _mapConfig_layers      = constDyn []
    }

data Map t
  = Map {
      _map_jsval  :: JSVal
    , _map_view   :: View t
    , _map_layers :: Dynamic t [Layer t]
    }
makeLenses ''Map

instance HasMapView (Map t) (View t) where
  mapView = map_view

instance HasLayers (Map t) (Dynamic t [Layer t]) where
  layers = map_layers

instance HasCenter (Map t) (Dynamic t (Maybe Coordinates)) where
  center = mapView . center
instance HasResolution (Map t) (Dynamic t (Maybe Resolution)) where
  resolution = mapView . resolution
instance HasRotation (Map t) (Dynamic t Rotation) where
  rotation = mapView . rotation


map :: (Typeable t, MonadWidget t m) => MapConfig t -> m (Map t)
map cfg@MapConfig{..} = do
  element <- liftM castToHTMLDivElement $ buildEmptyElement "div" (cfg^.attributes)
  let jsTarget = unElement (toElement element)
  v <- view (cfg^.mapView)
  jsMap <- liftIO $ do
    opts <- O.create
    O.setProp "view" (_view_jsval v) opts
    olMap opts
  initProp (const (liftIO . (toJSVal >=> olMap_setLayers jsMap)))
           layers jsMap cfg
  postBuild <- getPostBuild
  performEvent_ $ fmap (const (liftIO $ olMap_setTarget jsMap jsTarget)) postBuild
  return Map {
        _map_view   = v
      , _map_jsval  = jsMap
      , _map_layers = undefined
    }


--
-- ol.Map
--
foreign import javascript unsafe "new ol['Map']($1)"
  olMap :: O.Object -> IO JSVal

foreign import javascript unsafe "$1['setTarget']($2)"
  olMap_setTarget :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1['setView']($2)"
  olMap_setView :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1['getLayerGroup']()['setLayers'](new ol.Collection($2))"
  olMap_setLayers :: JSVal -> JSVal -> IO ()

--
-- CSS
--


css :: ByteString
css = $(embedFile "static/ol.css")
