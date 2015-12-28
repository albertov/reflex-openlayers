{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Reflex.OpenLayers (
    Map (..)
  , MapConfig (..)
  , ViewConfig (..)
  , View (..)
  , map
  , css
) where

import Reflex.OpenLayers.Layer

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Default (Default(def))
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import Data.FileEmbed (embedFile)
import qualified Data.Map as M
import Data.Monoid

import qualified JavaScript.Object as O
import GHCJS.DOM.HTMLDivElement (castToHTMLDivElement)
import GHCJS.DOM.Element (toElement)
import GHCJS.DOM.Types (unElement)
import GHCJS.Types (JSVal)
import GHCJS.Marshal (toJSVal)
import Reflex.Dom

import Prelude hiding (map)

--
-- Map
--

data MapConfig t
  = MapConfig {
      _mapConfig_attributes    :: Dynamic t (M.Map String String)
    , _mapConfig_view          :: ViewConfig t
    , _mapConfig_layerGroup    :: GroupConfig t
    , _mapConfig_setLayerGroup :: Event t (GroupConfig t)
    }

instance Reflex t => Default (MapConfig t) where
  def = MapConfig {
        _mapConfig_attributes    = constDyn mempty
      , _mapConfig_view          = def
      , _mapConfig_layerGroup    = def
      , _mapConfig_setLayerGroup = never
    }

data Map t
  = Map {
      _map_jsval      :: JSVal
    , _map_view       :: View t
    , _map_layerGroup :: Dynamic t (Group t)
    }

map :: (Typeable t, MonadWidget t m) => MapConfig t -> m (Map t)
map MapConfig{..} = do
  element <- liftM castToHTMLDivElement $ buildEmptyElement "div" _mapConfig_attributes
  v <- view _mapConfig_view
  layerGroup <- layer _mapConfig_layerGroup
  let jsTarget = unElement (toElement element)
  jsMap <- liftIO $ do
    opts <- O.create
    O.setProp "view" (_view_jsval v) opts
    jsLayers <- toJSVal layerGroup
    O.setProp "layers" jsLayers opts
    olMap opts
  liftIO $ debugMap jsMap
  dynLayers <- holdDyn (fromJust (layer_downcast layerGroup)) never
  postBuild <- getPostBuild
  performEvent_ $ fmap (\_ -> liftIO $ olMap_setTarget jsMap jsTarget) postBuild
  return Map {
        _map_view       = v
      , _map_jsval      = jsMap
      , _map_layerGroup = dynLayers
    }

foreign import javascript unsafe "window['_map']=$1"
  debugMap :: JSVal -> IO ()


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
    , _viewConfig_setCenter   :: Event t Coordinates
    , _viewConfig_setZoom     :: Event t Zoom
    , _viewConfig_setRotation :: Event t Rotation
    }

instance Reflex t => Default (ViewConfig t) where
  def = ViewConfig {
      _viewConfig_center      = (0,0)
    , _viewConfig_zoom        = 1
    , _viewConfig_rotation    = 0
    , _viewConfig_setCenter   = never
    , _viewConfig_setZoom     = never
    , _viewConfig_setRotation = never
    }

data View t
  = View {
      _view_center   :: Dynamic t Coordinates
    , _view_zoom     :: Dynamic t Zoom
    , _view_rotation :: Dynamic t Rotation
    , _view_jsval    :: JSVal
    }

view :: MonadWidget t m => ViewConfig t -> m (View t)
view ViewConfig {..} = do
  vVal <- liftIO $ do
    opts <- O.create
    let set k v = O.setProp k v opts
    set "center" =<< toJSVal _viewConfig_center
    set "zoom" =<< toJSVal _viewConfig_zoom
    set "rotation" =<< toJSVal _viewConfig_rotation
    olView opts
  return View {
      _view_center   = constDyn (0,0)
    , _view_zoom     = constDyn 0
    , _view_rotation = constDyn 0
    , _view_jsval    = vVal
    }

--
-- CSS
--


css :: ByteString
css = $(embedFile "static/ol.css")

--
-- FFI
--

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
-- ol.View
--
foreign import javascript unsafe "new ol['View']($1)"
  olView :: O.Object -> IO JSVal

foreign import javascript unsafe "$1['setCenter']($2)"
  olView_setCenter :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1['setRotation']($2)"
  olView_setRotation :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1['setZoom']($2)"
  olView_setZoom :: JSVal -> JSVal -> IO ()
