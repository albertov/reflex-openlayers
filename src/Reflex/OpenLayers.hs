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
    MapConfig
  , Map
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
  , HasUpdateSize (..)
  , olMap
  , constProperty

  , module Reflex.OpenLayers.Layer -- FIXME
  , module Reflex.OpenLayers.Source --FIXME
  , module Reflex.OpenLayers.Collection
  , module Reflex.OpenLayers.Projection
) where


import Reflex.OpenLayers.Layer
import Reflex.OpenLayers.Source
import Reflex.OpenLayers.Collection
import Reflex.OpenLayers.Projection
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
      _viewCenter     = constProperty (Coordinates 0 0)
    , _viewResolution = constProperty 100000
    , _viewRotation   = constProperty 0
    }

--
-- MapConfig
--

data MapConfig t
  = MapConfig {
      _mapConfig_attributes :: Dynamic t (M.Map String String)
    , _mapConfigView        :: View t Property
    , _mapConfigLayers      :: Dynamic t (LayerSet (Layer t Property))
    , _mapConfigUpdateSize  :: Event t ()
    }
makeFields ''MapConfig


instance HasCenter (MapConfig t) (Property t Coordinates) where
  center = view . center
instance HasResolution (MapConfig t) (Property t Double) where
  resolution = view . resolution
instance HasRotation (MapConfig t) (Property t Rotation) where
  rotation = view . rotation

instance HasAttributes (MapConfig t) where
  type Attrs (MapConfig t) = Dynamic t (M.Map String String)
  attributes = lens _mapConfig_attributes (\o v -> o {_mapConfig_attributes=v})

instance Reflex t => Default (MapConfig t) where
  def = MapConfig {
        _mapConfig_attributes  = constDyn mempty
      , _mapConfigView         = def
      , _mapConfigLayers       = constDyn def
      , _mapConfigUpdateSize   = never
    }

data Map t
  = Map {
      _mapView   :: View t Dynamic
    , _mapLayers :: Dynamic t (LayerSet (Layer t Dynamic))
    , _mapJsVal  :: JSVal
    }
makeFields ''Map

instance PToJSVal (Map t) where
  pToJSVal = _mapJsVal

instance ToJSVal (Map t) where
  toJSVal = return . pToJSVal

instance HasCenter (Map t) (Dynamic t Coordinates) where
  center = view . center
instance HasResolution (Map t) (Dynamic t Double) where
  resolution = view . resolution
instance HasRotation (Map t) (Dynamic t Rotation) where
  rotation = view . rotation

olMap :: MonadWidget t m => MapConfig t -> m (Map t)
olMap cfg = do
  target <- liftM (unElement . toElement . castToHTMLDivElement) $
              buildEmptyElement "div" (cfg^.attributes)
  (jv, v) <- mkView (cfg^.view)
  (jg, g) <- mkLayer (group (cfg^?!layers))
  m <- liftIO $ [jsu|$r = new ol.Map({view:`jv, layers:`jg, target:`target});|]
  postBuild <- getPostBuild
  performEvent_ $
    fmap (const (liftIO [js_|`m.updateSize();|])) $
      leftmost [cfg^.updateSize, postBuild]
  return $ Map v (g^?!layers) m


mkView :: MonadWidget t m => View t Property -> m (JSVal, View t Dynamic)
mkView v = do
  j <- liftIO [jsu|$r = new ol.View();|]
  jv <- View <$> initProperty "center"     j (v^.center)
             <*> initProperty "resolution" j (v^.resolution)
             <*> initProperty "rotation"   j (v^.rotation)
  return (j,jv)
