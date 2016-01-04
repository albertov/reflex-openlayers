{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Reflex.OpenLayers (
    Map
  , View
  , HasAttributes(..)
  , HasView (..)
  , HasZoom (..)
  , HasCenter (..)
  , HasRotation (..)
  , HasLayers (..)
  , Zoom (..)
  , map
  , css
) where

import Reflex.OpenLayers.Layer
import Reflex.OpenLayers.Util

import Control.Lens (lens, (^.))
import Control.Monad (liftM, (>=>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.Default (Default)
import Data.FileEmbed (embedFile)
import qualified Data.Map as M
import Data.Monoid

import GHCJS.DOM.HTMLDivElement (castToHTMLDivElement)
import GHCJS.DOM.Element (toElement)
import GHCJS.DOM.Types (unElement)
import GHCJS.Marshal (toJSVal)
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Types (JSVal)
import GHCJS.Foreign.QQ
import Reflex.Host.Class (newEventWithTrigger)
import Reflex.Dom

import Prelude hiding (map)

declareProperties [
    "view"
  , "zoom"
  , "center"
  , "rotation"
  , "layers"
  ]

--
-- View
--

type Coordinates = (Double, Double)

instance PToJSVal Coordinates where
  pToJSVal (x,y) = [jsu'|[`x, `y]|]

instance PFromJSVal Coordinates where
  pFromJSVal l = ([js'|`l[0]|], [js'|`l[1]|])

data Zoom
  = Resolution Double
  | Zoom       Int
  deriving (Show, Ord, Eq)
type Rotation    = Double

data View
  = View {
      _view_center   :: Coordinates
    , _view_zoom     :: Zoom
    , _view_rotation :: Rotation
    } deriving (Eq, Ord, Show)

hasProperties ''View [
    "zoom"
  , "center"
  , "rotation"
  ]

instance Default View where
  def = View {
      _view_center   = (0,0)
    , _view_zoom     = Zoom 0
    , _view_rotation = 0
    }

--
-- Map
--

data Map t
  = Map {
      _map_attributes :: Dynamic t (M.Map String String)
    , _map_view       :: Dynamic t View
    , _map_layers     :: Dynamic t [Layer t]
    }
hasProperties ''Map [
    "view"
  , "layers"
  ]

instance HasAttributes (Map t) where
  type Attrs (Map t) = Dynamic t (M.Map String String)
  attributes = lens _map_attributes (\o v -> o {_map_attributes=v})

instance Reflex t => Default (Map t) where
  def = Map {
        _map_attributes  = constDyn mempty
      , _map_view        = constDyn def
      , _map_layers      = constDyn mempty
    }

data MapEvent t
  = MapEvent {evMap :: JSVal}

map :: MonadWidget t m => Map t -> m (Event t (MapEvent t))
map cfg = do
  el <- liftM castToHTMLDivElement (buildEmptyElement "div" (cfg^.attributes))
  let target = unElement (toElement el)
  g <- mkLayer (group (cfg^.layers))
  m :: JSVal <- liftIO $ [jsu|$r = new ol.Map({layers:`g});|]
  eNewView <- dyn =<< mapDyn mkView (cfg^.view)
  addVoidAction $ ffor eNewView $ \newView ->
    liftIO $ [jsu_|`m.setView(`newView);|]
  getPostBuild >>=
    performEvent_ . fmap (const (liftIO ([js_|`m.setTarget(`target)|])))
  newEventWithTrigger $ \_ -> return (return ())

mkView :: MonadIO m => View -> m JSVal
mkView View{ _view_center   = c
           , _view_rotation = r
           , _view_zoom     = Zoom z
           } =
  liftIO [jsu|$r = new ol.View({center:`c, rotation:`r, zoom:`z});|]
mkView View{ _view_center   = c
           , _view_rotation = r
           , _view_zoom     = Resolution rs
           } =
  liftIO [jsu|$r = new ol.View({center:`c, rotation:`r, resolution:`rs});|]


-- CSS
--


css :: ByteString
css = $(embedFile "static/ol.css")
