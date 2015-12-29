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
  , HasLayerGroup (..)
  , HasZoom (..)
  , HasSetZoom (..)
  , HasCenter (..)
  , HasRotation (..)
  , map
  , css
) where

import Reflex.OpenLayers.Event (on_)
import Reflex.OpenLayers.Layer

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

class HasLayerGroup l v | l->v where
  layerGroup :: Lens' l v

class HasZoom l v | l->v where
  zoom :: Lens' l v

class HasSetZoom l v | l->v where
  setZoom :: Lens' l v

class HasCenter l v | l->v where
  center :: Lens' l v

class HasRotation l v | l->v where
  rotation :: Lens' l v

--
-- View
--

type Coordinates = (Double, Double)
type Zoom        = Int
type Rotation    = Double

data ViewConfig t
  = ViewConfig {
      _viewConfig_center      :: Coordinates
    , _viewConfig_zoom        :: Zoom
    , _viewConfig_rotation    :: Rotation
    , _viewConfig_setZoom     :: Event t Zoom
    , _viewConfig_setCenter   :: Event t Coordinates
    , _viewConfig_setRotation :: Event t Rotation
    }
makeLenses ''ViewConfig

instance HasCenter (ViewConfig t) (Coordinates) where
  center = viewConfig_center
instance HasZoom (ViewConfig t) Zoom where
  zoom = viewConfig_zoom
instance HasSetZoom (ViewConfig t) (Event t Zoom) where
  setZoom = viewConfig_setZoom
instance HasRotation (ViewConfig t) (Rotation) where
  rotation = viewConfig_rotation

instance Reflex t => Default (ViewConfig t) where
  def = ViewConfig {
      _viewConfig_center      = (0,0)
    , _viewConfig_zoom        = 0
    , _viewConfig_rotation    = 0
    , _viewConfig_setCenter   = never
    , _viewConfig_setZoom     = never
    , _viewConfig_setRotation = never
    }

data View t
  = View {
      _view_center   :: Dynamic t (Maybe Coordinates)
    , _view_zoom     :: Dynamic t (Maybe Zoom)
    , _view_rotation :: Dynamic t Rotation
    , _view_jsval    :: JSVal
    }
makeLenses ''View

instance HasCenter (View t) (Dynamic t (Maybe Coordinates)) where
  center = view_center
instance HasZoom (View t) (Dynamic t (Maybe Zoom)) where
  zoom = view_zoom
instance HasRotation (View t) (Dynamic t Rotation) where
  rotation = view_rotation

view :: MonadWidget t m => ViewConfig t -> m (View t)
view cfg = do
  jsView <- liftIO $ do
    opts <- O.create
    let set k v = do {v' <- toJSVal v; O.setProp k v' opts}
        set :: ToJSVal a => JSString -> a -> IO ()
    set "center" (cfg^.center)
    set "zoom" (cfg^.zoom)
    set "rotation" (cfg^.rotation)
    olView opts

  postGui <- askPostGui
  runWithActions <- askRunWithActions

  performEvent_ $ fmap (liftIO . (toJSVal >=> olView_setZoom jsView)) (cfg^.setZoom)
  dynZoom <- holdDyn (Just (cfg^.zoom)) =<< (newEventWithTrigger $ \trig -> do
    unsubscribe <- liftIO $ on_ "change:resolution" jsView $ \_ -> do
      zoom <- olView_getZoom jsView
      postGui $ runWithActions [trig :=> pFromJSVal zoom]
    return (liftIO unsubscribe))

  dynCenter <- holdDyn (Just (cfg^.center)) =<< (newEventWithTrigger $ \trig -> do
    unsubscribe <- liftIO $ on_ "change:center" jsView $ \_ -> do
      v <- fromJSVal =<< olView_getCenter jsView
      postGui $ runWithActions [trig :=> v]
    return (liftIO unsubscribe))

  return View {
      _view_center   = dynCenter
    , _view_zoom     = dynZoom
    , _view_rotation = constDyn 0
    , _view_jsval    = jsView
    }

--
-- ol.View
--
foreign import javascript unsafe "new ol['View']($1)"
  olView :: O.Object -> IO JSVal

foreign import javascript unsafe "$1['setCenter']($2)"
  olView_setCenter :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1['getCenter']()"
  olView_getCenter :: JSVal -> IO JSVal

foreign import javascript unsafe "$1['setRotation']($2)"
  olView_setRotation :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1['setZoom']($2)"
  olView_setZoom :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1['getZoom']()"
  olView_getZoom :: JSVal -> IO JSVal

--
-- Map
--

data MapConfig t
  = MapConfig {
      _mapConfig_attributes    :: Dynamic t (M.Map String String)
    , _mapConfig_view          :: ViewConfig t
    , _mapConfig_layerGroup    :: GroupConfig t
    }
makeLenses ''MapConfig

instance HasAttributes (MapConfig t) where
  type Attrs (MapConfig t) = Dynamic t (M.Map String String)
  attributes = mapConfig_attributes

instance HasMapView (MapConfig t) (ViewConfig t) where
  mapView = mapConfig_view

instance HasLayerGroup (MapConfig t) (GroupConfig t) where
  layerGroup = mapConfig_layerGroup

instance Reflex t => Default (MapConfig t) where
  def = MapConfig {
        _mapConfig_attributes    = constDyn mempty
      , _mapConfig_view          = def
      , _mapConfig_layerGroup    = def
    }

data Map t
  = Map {
      _map_jsval      :: JSVal
    , _map_view       :: View t
    , _map_layerGroup :: Group t
    }
makeLenses ''Map

instance HasMapView (Map t) (View t) where
  mapView = map_view


map :: (Typeable t, MonadWidget t m) => MapConfig t -> m (Map t)
map cfg@MapConfig{..} = do
  element <- liftM castToHTMLDivElement $ buildEmptyElement "div" (cfg^.attributes)
  v <- view (cfg^.mapView)
  layers <- layer (cfg^.layerGroup)
  let jsTarget = unElement (toElement element)
  jsMap <- liftIO $ do
    opts <- O.create
    O.setProp "view" (_view_jsval v) opts
    jsLayers <- toJSVal layers
    O.setProp "layers" jsLayers opts
    olMap opts
  liftIO $ debugMap jsMap
  postBuild <- getPostBuild
  performEvent_ $ fmap (\_ -> liftIO $ olMap_setTarget jsMap jsTarget) postBuild
  return Map {
        _map_view       = v
      , _map_jsval      = jsMap
      , _map_layerGroup = fromJust (layer_downcast layers)
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


--
-- Util
--

foreign import javascript unsafe "window['_map']=$1"
  debugMap :: JSVal -> IO ()

--
-- CSS
--


css :: ByteString
css = $(embedFile "static/ol.css")
