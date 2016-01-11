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
  , Property
  , Coordinates (..)
  , HasAttributes(..)
  , HasX(..)
  , HasY(..)
  , HasView (..)
  , HasResolution (..)
  , HasCenter (..)
  , HasRotation (..)
  , HasLayers (..)
  , HasInitialValue (..)
  , HasSetValue (..)
  , olMap
  , olCss
  , property

  , module Reflex.OpenLayers.Layer -- FIXME
  , module Reflex.OpenLayers.Source --FIXME
) where


import Reflex.OpenLayers.Layer
import Reflex.OpenLayers.Source
import Reflex.OpenLayers.Util
import Reflex.OpenLayers.Event

import Reflex.Host.Class (newEventWithTrigger)
import Reflex
import Reflex.Dom

import Control.Lens (Lens', lens, to, makeFields, (^.), (^?), (^?!))
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
import GHCJS.Types (JSVal, IsJSVal, jsval)
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

data View t p
  = View {
      _viewCenter     :: !(p t Coordinates)
    , _viewResolution :: !(p t Double)
    , _viewRotation   :: !(p t Rotation)
    }
makeFields ''View

instance Reflex t => Default (View t Property)where
  def = View {
      _viewCenter     = property (Coordinates 0 0)
    , _viewResolution = property 100000
    , _viewRotation   = property 0
    }

--
-- Map
--

data Map t
  = Map {
      _map_attributes :: Dynamic t (M.Map String String)
    , _mapView        :: View t Property
    , _mapLayers      :: Dynamic t (LayerSet (Layer t Property))
    }
makeFields ''Map


instance HasCenter (Map t) (Property t Coordinates) where
  center = view . center
instance HasResolution (Map t) (Property t Double) where
  resolution = view . resolution
instance HasRotation (Map t) (Property t Rotation) where
  rotation = view . rotation

newtype JSMap = JSMap JSVal
  deriving (PToJSVal, PFromJSVal, ToJSVal, FromJSVal)
instance IsJSVal JSMap

instance HasAttributes (Map t) where
  type Attrs (Map t) = Dynamic t (M.Map String String)
  attributes = lens _map_attributes (\o v -> o {_map_attributes=v})

instance Reflex t => Default (Map t) where
  def = Map {
        _map_attributes  = constDyn mempty
      , _mapView         = def
      , _mapLayers       = constDyn def
    }

data MapWidget t
  = MapWidget {
      _mapWidgetView   :: View t Dynamic
    , _mapWidgetLayers :: Dynamic t (LayerSet (Layer t Dynamic))
    , _mapWidgetJsVal  :: JSMap
    }
makeFields ''MapWidget

instance PToJSVal (MapWidget t) where
  pToJSVal = jsval . _mapWidgetJsVal

instance ToJSVal (MapWidget t) where
  toJSVal = return . pToJSVal

instance HasCenter (MapWidget t) (Dynamic t Coordinates) where
  center = view . center
instance HasResolution (MapWidget t) (Dynamic t Double) where
  resolution = view . resolution
instance HasRotation (MapWidget t) (Dynamic t Rotation) where
  rotation = view . rotation

olMap :: MonadWidget t m => Map t -> m (MapWidget t)
olMap cfg = do
  target <- liftM (unElement . toElement . castToHTMLDivElement) $
              buildEmptyElement "div" (cfg^.attributes)

  (jv, v) <- mkView (cfg^.view)
  (jg, g) <- mkLayer (group (cfg^?!layers))
  m <- liftIO $ [jsu|$r = new ol.Map({view:`jv, layers:`jg}); window._map=$r;|]
  getPostBuild >>=
    performEvent_ . fmap (const (liftIO [js_|`m.setTarget(`target)|]))
  return $ MapWidget v (g^?!layers) m


mkView :: MonadWidget t m => View t Property -> m (JSVal, View t Dynamic)
mkView v = do
  j <- liftIO [jsu|$r = new ol.View({});|]
  jv <- View <$> initProperty "center"     j (v^.center)
             <*> initProperty "resolution" j (v^.resolution)
             <*> initProperty "rotation"   j (v^.rotation)
  return (j,jv)


-- CSS
--


olCss :: ByteString
olCss = $(embedFile "static/ol.css")
