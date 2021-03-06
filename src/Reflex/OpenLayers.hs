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
  , HasAttributes(..)
  , HasView (..)
  , HasResolution (..)
  , HasCenter (..)
  , HasRotation (..)
  , HasLayers (..)
  , HasInitialValue (..)
  , HasSetValue (..)
  , HasUpdateSize (..)
  , HasInteractions (..)
  , HasZoomBounds (..)
  , olMap
  , constProperty
  , Center (..)
  , HasX (..)
  , HasY (..)
  , mkCenter
  , module Reflex.OpenLayers.Layer -- FIXME
  , module Reflex.OpenLayers.Source --FIXME
  , module Reflex.OpenLayers.Collection
  , module Reflex.OpenLayers.Projection

  -- Re-exports
) where


import Reflex.OpenLayers.Layer
import Reflex.OpenLayers.Source
import Reflex.OpenLayers.Collection
import Reflex.OpenLayers.Projection
import Reflex.OpenLayers.Util

import Reflex
import Reflex.Dom

import Control.Lens (Lens', lens, to, makeFields, (^.), (^?), (^?!))
import Control.Monad
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Default (Default)
import Data.Proxy
import qualified Data.Map as M

import GHCJS.DOM.HTMLDivElement (castToHTMLDivElement)
import GHCJS.DOM.Element (toElement)
import GHCJS.DOM.Types (unElement)
import GHCJS.Marshal (ToJSVal(toJSVal), FromJSVal(fromJSVal))
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Types (JSVal, IsJSVal, jsval)
import GHCJS.Foreign.QQ

import Sigym4.Geometry



--
-- View
--
class HasPoint o a | o -> a where
  point :: Lens' o a

class HasX o a | o -> a where
  x :: Lens' o a

class HasY o a | o -> a where
  y :: Lens' o a

newtype Center crs = Center { unCenter :: Point V2 crs}
  deriving (Eq, Ord, Show)

mkCenter :: Projection -> Double -> Double -> WithSomeCrs Center
mkCenter (Projection crs) x y =
  reifyCrs crs $ \(Proxy :: Proxy crs) ->
    WithSomeCrs $ Center (Point (V2 x y) :: Point V2 crs)

instance PToJSVal (Center crs) where
  pToJSVal (Center (Point (V2 a b))) = [jsu'|[`a, `b]|]

instance PFromJSVal (Center crs)  where
  pFromJSVal l = Center $ Point $ V2 [js'|`l[0]|] [js'|`l[1]|]

instance Default (Center crs) where
  def = Center $ Point 0

instance HasPoint (Center crs) (Point V2 crs) where
  point = lens unCenter (const Center)

instance HasX (Center crs) Double where
  x = point.vertex._x

instance HasY (Center crs) Double where
  y = point.vertex._y



instance Default (WithSomeCrs Center) where
  def = WithSomeCrs (def :: Center SphericalMercator)

instance HasPoint (WithSomeCrs Center) (WithSomeCrs (Point V2)) where
  point = lens (\(WithSomeCrs (Center p)) -> WithSomeCrs p)
               (\_ (WithSomeCrs p) -> WithSomeCrs (Center p))

instance HasX (WithSomeCrs Center) Double where
  x = point.vertex._x

instance HasY (WithSomeCrs Center) Double where
  y = point.vertex._y



type Rotation    = Double

data View t p
  = View {
      _viewCenter     :: !(p t (WithSomeCrs Center))
    , _viewResolution :: !(p t Double)
    , _viewRotation   :: !(p t Rotation)
    , _viewZoomBounds :: Maybe (Int,Int) -- ^ Used instead of min/maxZoom since
                                         --   both need to be specified
    }
makeFields ''View


instance Reflex t => Default (View t Property) where
  def = View {
      _viewCenter     = constProperty def
    , _viewResolution = constProperty 100000
    , _viewRotation   = constProperty 0
    , _viewZoomBounds = Nothing
    }

--
-- MapConfig
--

data MapConfig t
  = MapConfig {
      _mapConfig_attributes :: Dynamic t (M.Map String String)
    , _mapConfigView        :: View t Property
    , _mapConfigLayers      :: Dynamic t (LayerSet (Layer t Property))
    , _mapConfigInteractions :: M.Map String Bool
    , _mapConfigUpdateSize  :: Event t ()
    }
makeFields ''MapConfig


instance HasCenter (MapConfig t) (Property t (WithSomeCrs Center)) where
  center = view . center
instance HasResolution (MapConfig t) (Property t Double) where
  resolution = view . resolution
instance HasRotation (MapConfig t) (Property t Rotation) where
  rotation = view . rotation
instance HasZoomBounds (MapConfig t) (Maybe (Int,Int)) where
  zoomBounds = view . zoomBounds

instance HasAttributes (MapConfig t) where
  type Attrs (MapConfig t) = Dynamic t (M.Map String String)
  attributes = lens _mapConfig_attributes (\o v -> o {_mapConfig_attributes=v})

instance Reflex t => Default (MapConfig t) where
  def = MapConfig {
        _mapConfig_attributes  = constDyn mempty
      , _mapConfigView         = def
      , _mapConfigLayers       = constDyn def
      , _mapConfigUpdateSize   = never
      , _mapConfigInteractions = mempty
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

instance HasCenter (Map t) (Dynamic t (WithSomeCrs Center)) where
  center = view . center
instance HasResolution (Map t) (Dynamic t Double) where
  resolution = view . resolution
instance HasRotation (Map t) (Dynamic t Rotation) where
  rotation = view . rotation


olMap
  :: MonadWidget t m => MapConfig t -> m (Map t)
olMap cfg = do
  target <- liftM (unElement . toElement . castToHTMLDivElement) $
              buildEmptyElement "div" (cfg^.attributes)
  (jv, v) <- mkView (cfg^.view)
  (jg, g) <- mkLayer (group (cfg^?!layers))
  let ics = cfg^.interactions
  m <- liftIO $ [jsu|
    $r = new ol.Map({
        view:`jv
      , layers:`jg
      , target:`target
      , interactions : ol.interaction.defaults(`ics)
      });|]
  postBuild <- getPostBuild
  performEvent_ $
    fmap (const (liftIO [js_|`m.updateSize();|])) $
      leftmost [cfg^.updateSize, postBuild]
  return $ Map v (g^?!layers) m


mkView
  :: forall t m. MonadWidget t m
  => View t Property -> m (JSVal, View t Dynamic)
mkView v = do
  let crs = getCrs (v^.center.initialValue)
  jsProj <- toJSVal_projection (Projection crs)
  let build :: WithSomeCrs Center -> m JSVal
      build wc@(WithSomeCrs c) = do
        jsSrc <- toJSVal_projection (Projection (getCrs wc))
        liftIO [js|$r=ol.proj.transform(`c, `jsSrc, `jsProj);|]
      unBuild c = reifyCrs crs $ \(Proxy :: Proxy crs) ->
        return $ WithSomeCrs (pFromJSVal c :: Center crs)
      minz = v^.zoomBounds.to (fmap fst)
      maxz = v^.zoomBounds.to (fmap snd)
  j <- liftIO [jsu|
    $r = new ol.View(
      { projection: `jsProj
      , minZoom: `minz || undefined
      , maxZoom: `maxz || undefined
      });
    |]
  _viewCenter <- initPropertyWith (Just unBuild) build "center" j (v^.center)
  _viewResolution <- initProperty "resolution" j (v^.resolution)
  _viewRotation <- initProperty "rotation"   j (v^.rotation)
  return (j, View { _viewZoomBounds=v^.zoomBounds
                  , ..
                  })
