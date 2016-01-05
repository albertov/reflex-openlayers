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
  , Coordinates (..)
  , HasAttributes(..)
  , HasX(..)
  , HasY(..)
  , HasView (..)
  , HasResolution (..)
  , HasCenter (..)
  , HasRotation (..)
  , HasLayers (..)
  , HasViewChanged (..)
  , olMap
  , olCss

  , module Reflex.OpenLayers.Layer
  , module Reflex.OpenLayers.Source
) where


import Reflex.OpenLayers.Layer
import Reflex.OpenLayers.Source
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

data Coordinates
  = Coordinates {
      coordinatesX :: !Double
    , coordinatesY :: !Double
    } deriving (Eq, Ord, Show)
makeFields ''Coordinates

instance PToJSVal Coordinates where
  pToJSVal (Coordinates x y) = [jsu'|[`x, `y]|]

instance PFromJSVal Coordinates where
  pFromJSVal l = Coordinates [js'|`l[0]|] [js'|`l[1]|]

type Rotation    = Double

data View
  = View {
      _viewCenter     :: Coordinates
    , _viewResolution :: Double
    , _viewRotation   :: Rotation
    } deriving (Eq, Ord, Show)

instance PFromJSVal View where
  pFromJSVal v = View [js'|$r=`v.getCenter();|]
                      [js'|$r=`v.getResolution();|]
                      [js'|$r=`v.getRotation();|]

makeFields ''View

instance Default View where
  def = View {
      _viewCenter     = Coordinates 0 0
    , _viewResolution = 100000
    , _viewRotation   = 0
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

olMap :: MonadWidget t m => Map t -> m (MapWidget t)
olMap cfg = do
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
mkView View{ _viewCenter     = c
           , _viewRotation   = r
           , _viewResolution = rs
           } =
  liftIO [jsu|$r = new ol.View({center:`c, rotation:`r, resolution:`rs});|]


-- CSS
--


olCss :: ByteString
olCss = $(embedFile "static/ol.css")
