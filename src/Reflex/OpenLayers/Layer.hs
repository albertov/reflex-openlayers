{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE NamedFieldPuns #-}

module Reflex.OpenLayers.Layer (
    Layer
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
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import GHCJS.Marshal (toJSVal)
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Types
import GHCJS.Foreign (isNull)
import GHCJS.Foreign.QQ
import GHCJS.DOM.Types (toJSString)
import Language.Haskell.TH (reify)

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
declareProperties [
    "opacity"
  , "visible"
  , "extent"
  , "zIndex"
  , "minResolution"
  , "maxResolution"
  ]

data LayerBase t
  = LayerBase {
      _layerBase_opacity       :: Dynamic t Opacity
    , _layerBase_visible       :: Dynamic t Bool
    , _layerBase_zIndex        :: Dynamic t Int
    , _layerBase_extent        :: Dynamic t (Maybe Extent)
    , _layerBase_minResolution :: Dynamic t (Maybe Double)
    , _layerBase_maxResolution :: Dynamic t (Maybe Double)
    }
hasProperties ''LayerBase [
    "opacity"
  , "visible"
  , "zIndex"
  , "extent"
  , "minResolution"
  , "maxResolution"
  ]
instance Reflex t => Default (LayerBase t) where
  def = LayerBase {
       _layerBase_opacity          = constDyn 1
     , _layerBase_visible          = constDyn True
     , _layerBase_zIndex           = constDyn 0
     , _layerBase_extent           = constDyn Nothing
     , _layerBase_minResolution    = constDyn Nothing
     , _layerBase_maxResolution    = constDyn Nothing
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
mkLayer l =
  case l of
    Image{_source} -> do
      r <- liftIO [jsu|$r=new ol.layer.Image({});|]
      eNewSource <- dyn =<< mapDyn mkSource _source
      addVoidAction $ ffor eNewSource $ \newSource ->
        liftIO $ [jsu_|`r.setSource(`newSource);|]
      return r
    Tile{_source} -> do
      r <- liftIO [jsu|$r=new ol.layer.Tile({});|]
      eNewSource <- dyn =<< mapDyn mkSource _source
      addVoidAction $ ffor eNewSource $ \newSource ->
        liftIO $ [jsu_|`r.setSource(`newSource);|]
      return r
    Group{_layers} -> do
      -- FIXME: Don't recreate unchanged layers in order not to lose features
      r <- liftIO [jsu|$r=new ol.layer.Group({});|]
      eNewLayers <- dyn =<< mapDyn (mapM mkLayer) _layers
      addVoidAction $ ffor eNewLayers $ \newLayers -> liftIO $ do
        ls <- toJSVal newLayers
        [jsu_|`r.setLayers(new ol.Collection(`ls));|]
      return r
