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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

module Reflex.OpenLayers (
    Map
  , MapWidget
  , View
  , Property(..)
  , PropertyObj(..)
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
  , HasInitialValue (..)
  , olMap
  , olCss
  , property
  , updateProperty

  , module Reflex.OpenLayers.Layer
  , module Reflex.OpenLayers.Source
) where


import Reflex.OpenLayers.Layer
import Reflex.OpenLayers.Source
import Reflex.OpenLayers.Util
import Reflex.OpenLayers.Event

import Reflex.Host.Class (newEventWithTrigger)
import Reflex
import Reflex.Dom

import Control.Lens (lens, makeFields, (^.), (^?), (^?!))
import Control.Monad (liftM, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.Default (Default)
import Data.Dependent.Sum (DSum (..))
import Data.FileEmbed (embedFile)
import qualified Data.Map as M
import Data.Maybe (fromJust)

import GHCJS.DOM.HTMLDivElement (castToHTMLDivElement)
import GHCJS.DOM.Element (toElement)
import GHCJS.DOM.Types (unElement)
import GHCJS.Marshal (ToJSVal(toJSVal), FromJSVal(fromJSVal))
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Types (JSVal, IsJSVal)
import GHCJS.Foreign.QQ

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
  pToJSVal (Coordinates a b) = [jsu'|[`a, `b]|]

instance PFromJSVal Coordinates where
  pFromJSVal l = Coordinates [js'|`l[0]|] [js'|`l[1]|]

type Rotation    = Double

data View
  = View {
      _viewCenter     :: !Coordinates
    , _viewResolution :: !Double
    , _viewRotation   :: !Rotation
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
    , _mapLayers      :: Property t "layers" (LayerSet (Layer t))
    }
makeFields ''Map

newtype JSMap = JSMap JSVal
  deriving (PToJSVal, PFromJSVal, ToJSVal, FromJSVal)
instance IsJSVal JSMap

instance HasAttributes (Map t) where
  type Attrs (Map t) = Dynamic t (M.Map String String)
  attributes = lens _map_attributes (\o v -> o {_map_attributes=v})

instance Reflex t => Default (Map t) where
  def = Map {
        _map_attributes  = constDyn mempty
      , _mapView         = constDyn def
      , _mapLayers       = def
    }

data MapWidget t
  = MapWidget {
      _mapWidgetViewChanged :: Event t View
    , _mapWidgetLayers      :: PropertyObj t "layers" (LayerSet (JSLayer t))
    }
makeFields ''MapWidget

olMap :: MonadWidget t m => Map t -> m (MapWidget t)
olMap cfg = do
  target <- liftM (unElement . toElement . castToHTMLDivElement) $
              buildEmptyElement "div" (cfg^.attributes)

  v <- mkView def
  m :: JSMap <- liftIO $ [jsu|$r = new ol.Map({view:`v}); window._map=$r;|]
  getPostBuild >>=
    performEvent_ . fmap (const (liftIO ([js_|`m.setTarget(`target)|])))

  postGui <- askPostGui
  runWithActions <- askRunWithActions
  eViewChanged <- newEventWithTrigger $ \trig -> do
    unsubscribe <- liftIO $ on_ "propertychange" v $ \(_::JSVal) -> do
      v' <- liftIO [jsu|$r=`m.getView()|]
      postGui $ runWithActions [trig :=> v']
    return (liftIO unsubscribe)

  MapWidget <$> pure eViewChanged
            <*> initProperty m (cfg^?!layers)


mkView :: MonadIO m => View -> m JSVal
mkView View{ _viewCenter     = c
           , _viewRotation   = r
           , _viewResolution = rs
           } =
  liftIO [jsu|$r = new ol.View({center:`c, rotation:`r, resolution:`rs});|]


instance HasNamedProperty JSMap "layers" (LayerSet (Layer t)) (LayerSet (JSLayer t)) t
  where
    initProperty m (Property ls e) = do
      jsL <- mkLayer (group ls)
      liftIO [jsu_|`m.setLayerGroup(`jsL);|]
      PropertyObj <$> holdDyn (jsL^?!layers) never <*> pure (const (return ()))

-- CSS
--


olCss :: ByteString
olCss = $(embedFile "static/ol.css")
