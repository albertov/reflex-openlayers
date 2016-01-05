{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE NamedFieldPuns #-}

module Reflex.OpenLayers.Layer (
    Layer (..)
  , Extent (..)
  , Opacity
  , image
  , tile
  , group
  , mkLayer

  -- Properties
  , HasOpacity(..)
  , HasVisible(..)
  , HasExtent(..)
  , HasMinResolution(..)
  , HasMaxResolution(..)
  , HasZIndex(..)
) where

import Reflex.OpenLayers.Source (Source, mkSource)
import Reflex.OpenLayers.Util

import Reflex
import Reflex.Dom

import qualified JavaScript.Object as O
import Data.Default (Default(def))
import Data.Typeable (Typeable, cast)
import Control.Lens
import Control.Monad (when, liftM)
import Control.Monad.IO.Class (liftIO)
import GHCJS.Marshal (toJSVal)
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Types
import GHCJS.Foreign (isNull)
import GHCJS.Foreign.QQ
import GHCJS.DOM.Types (toJSString)
import System.Mem.StableName

type Opacity = Double

data Extent
  = Extent { _xMin :: !Double
           , _yMin :: !Double
           , _xMax :: !Double
           , _yMax :: !Double
           } deriving (Show, Eq)

instance PToJSVal Extent where
  pToJSVal (Extent x0 y0 x1 y1) = [jsu'|[`x0, `y0, `x1, `y1]|]
instance PFromJSVal Extent where
  pFromJSVal l = Extent [js'|`l[0]|] [js'|`l[1]|] [js'|`l[2]|] [js'|`l[3]|]


--
-- Layer
--

data LayerBase t
  = LayerBase {
      _layerBaseOpacity       :: Dynamic t Opacity
    , _layerBaseVisible       :: Dynamic t Bool
    , _layerBaseZIndex        :: Dynamic t Int
    , _layerBaseExtent        :: Dynamic t (Maybe Extent)
    , _layerBaseMinResolution :: Dynamic t (Maybe Double)
    , _layerBaseMaxResolution :: Dynamic t (Maybe Double)
    }
makeFields ''LayerBase

instance Reflex t => Default (LayerBase t) where
  def = LayerBase {
       _layerBaseOpacity          = constDyn 1
     , _layerBaseVisible          = constDyn True
     , _layerBaseZIndex           = constDyn 0
     , _layerBaseExtent           = constDyn Nothing
     , _layerBaseMinResolution    = constDyn Nothing
     , _layerBaseMaxResolution    = constDyn Nothing
     }

data Layer t
  = Image { _layerBase :: LayerBase t
          , _source    :: Dynamic t (Source t)
          }
  | Tile  { _layerBase :: LayerBase t
          , _source    :: Dynamic t (Source t)
          }
  | Group { _layerBase :: LayerBase t
          , _layers    :: Dynamic t [Layer t]
          }
makeLenses ''Layer

instance HasOpacity (Layer t) (Dynamic t Opacity) where
  opacity = layerBase . opacity
instance HasVisible (Layer t) (Dynamic t Bool) where
  visible = layerBase . visible
instance HasZIndex (Layer t) (Dynamic t Int) where
  zIndex = layerBase . zIndex
instance HasExtent (Layer t) (Dynamic t (Maybe Extent)) where
  extent = layerBase . extent
instance HasMinResolution (Layer t) (Dynamic t (Maybe Double)) where
  minResolution = layerBase . minResolution
instance HasMaxResolution (Layer t) (Dynamic t (Maybe Double)) where
  maxResolution = layerBase . maxResolution

image :: Reflex t => Dynamic t (Source t) -> Layer t
image = Image def

tile :: Reflex t => Dynamic t (Source t) -> Layer t
tile = Tile def

group :: Reflex t => Dynamic t [Layer t] -> Layer t
group = Group def

mkLayer :: MonadWidget t m => Layer t -> m JSVal
mkLayer l = do
  r <- case l of
    Image{_source} -> do
      r <- liftIO [jsu|$r=new ol.layer.Image({});|]
      dynInitializeWith mkSource _source $ \newSource ->
        liftIO $ [jsu_|`r.setSource(`newSource);|]
      return r
    Tile{_source} -> do
      r <- liftIO [jsu|$r=new ol.layer.Tile({});|]
      dynInitializeWith mkSource _source $ \newSource ->
        liftIO $ [jsu_|`r.setSource(`newSource);|]
      return r
    Group{_layers} -> do
      r <- liftIO [jsu|$r=new ol.layer.Group({});|]
      dynInitializeWith (mapM mkLayer) _layers $ \newLayers -> liftIO $ do
        ls <- toJSVal newLayers
        [jsu_|h$updateGroupLayers(`r, `ls);|]
      return r
  liftIO $ do
    hash <- liftM hashStableName (makeStableName l)
    [jsu_|`r["h$hash"]=`hash;|]
  wrapDynObservableProp "opacity" r (l^.opacity)
  wrapDynObservableProp "visible" r (l^.visible)
  wrapDynObservableProp "zIndex" r (l^.zIndex)
  wrapDynObservableProp "extent" r (l^.extent)
  wrapDynObservableProp "minResolution" r (l^.minResolution)
  wrapDynObservableProp "maxResolution" r (l^.maxResolution)
  return r
