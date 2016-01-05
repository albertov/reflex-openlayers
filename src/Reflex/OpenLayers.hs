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
  , MapWidget
  , View
  , HasAttributes(..)
  , HasView (..)
  , HasZoom (..)
  , HasCenter (..)
  , HasRotation (..)
  , HasLayers (..)
  , HasViewChanged (..)
  , ZoomResolution
  , These (..)
  , map
  , css
) where


import Reflex.OpenLayers.Layer
import Reflex.OpenLayers.Util
import Reflex.OpenLayers.Event

import Reflex.Host.Class (newEventWithTrigger, newEventWithTriggerRef)
import Reflex
import Reflex.Dom

import Control.Lens (lens, makeFields, (^.))
import Control.Monad (liftM, when, (>=>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.Default (Default)
import Data.Dependent.Sum (DSum (..))
import Data.FileEmbed (embedFile)
import Data.These
import qualified Data.Map as M
import Data.Monoid

import GHCJS.DOM.HTMLDivElement (castToHTMLDivElement)
import GHCJS.DOM.Element (toElement)
import GHCJS.DOM.Types (unElement)
import GHCJS.Marshal (toJSVal)
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Types (JSVal)
import GHCJS.Foreign.QQ

import Prelude hiding (map)

--
-- View
--

type Coordinates = (Double, Double)

instance PToJSVal Coordinates where
  pToJSVal (x,y) = [jsu'|[`x, `y]|]

instance PFromJSVal Coordinates where
  pFromJSVal l = ([js'|`l[0]|], [js'|`l[1]|])

type ZoomResolution = These Int Double
type Rotation    = Double

data View
  = View {
      _viewCenter   :: Coordinates
    , _viewZoom     :: ZoomResolution
    , _viewRotation :: Rotation
    } deriving (Eq, Ord, Show)

instance PFromJSVal View where
  pFromJSVal v = View [js'|$r=`v.getCenter();|]
                      (These [js'|$r=`v.getZoom();|]
                             [js'|$r=`v.getResolution();|])
                      [js'|$r=`v.getRotation();|]

makeFields ''View

instance Default View where
  def = View {
      _viewCenter   = (0,0)
    , _viewZoom     = This 0
    , _viewRotation = 0
    }

--
-- Map
--

data Map t
  = Map {
      _map_attributes :: Dynamic t (M.Map String String)
    , _mapView        :: Dynamic t View
    , _mapLayers      :: Dynamic t [Layer t]
    }
makeFields ''Map

instance HasAttributes (Map t) where
  type Attrs (Map t) = Dynamic t (M.Map String String)
  attributes = lens _map_attributes (\o v -> o {_map_attributes=v})

instance Reflex t => Default (Map t) where
  def = Map {
        _map_attributes  = constDyn mempty
      , _mapView         = constDyn def
      , _mapLayers       = constDyn mempty
    }

data MapEvent t
  = MapEvent {evMap :: JSVal}


data MapWidget t
  = MapWidget {
      _mapWidgetViewChanged :: Event t View
    }
makeFields ''MapWidget

map :: MonadWidget t m => Map t -> m (MapWidget t)
map cfg = do
  el <- liftM castToHTMLDivElement (buildEmptyElement "div" (cfg^.attributes))
  let target = unElement (toElement el)
  g <- mkLayer (group (cfg^.layers))
  v <- mkView def
  m :: JSVal <- liftIO $ [jsu|$r = new ol.Map({layers:`g, view:`v});|]
  (eUpdating, tUpdating) <- newEventWithTriggerRef
  isUpdating <- hold False eUpdating
  dynInitializeWith mkView (cfg^.view) $ \newView -> do
    runFrameWithTriggerRef tUpdating True
    liftIO $ [jsu_|h$updateView(`m, `newView);|]
    runFrameWithTriggerRef tUpdating False
  getPostBuild >>=
    performEvent_ . fmap (const (liftIO ([js_|`m.setTarget(`target)|])))

  postGui <- askPostGui
  runWithActions <- askRunWithActions
  eViewChanged <- newEventWithTrigger $ \trig -> do
    unsubscribe <- liftIO $ on_ "propertychange" v $ \(_::JSVal) -> do
      v' <- liftIO [jsu|$r=`m.getView()|]
      postGui $ runWithActions [trig :=> v']
    return (liftIO unsubscribe)

  return (MapWidget (gate (fmap not isUpdating) eViewChanged))

mkView :: MonadIO m => View -> m JSVal
mkView View{ _viewCenter   = c
           , _viewRotation = r
           , _viewZoom     = This z
           } =
  liftIO [jsu|$r = new ol.View({center:`c, rotation:`r, zoom:`z});|]
mkView View{ _viewCenter   = c
           , _viewRotation = r
           , _viewZoom     = That rs
           } =
  liftIO [jsu|$r = new ol.View({center:`c, rotation:`r, resolution:`rs});|]
mkView View{ _viewCenter   = c
           , _viewRotation = r
           , _viewZoom     = These z _
           } =
  liftIO [jsu|$r = new ol.View({center:`c, rotation:`r, zoom:`z});|]


-- CSS
--


css :: ByteString
css = $(embedFile "static/ol.css")
