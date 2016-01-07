{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Reflex.OpenLayers.Layer (
    Layer (..)
  , JSLayer
  , Extent (..)
  , Opacity
  , LayerSet
  , image
  , tile
  , group
  , mkLayer
  , fromList
  , pushLayer

  -- Properties
  , HasOpacity(..)
  , HasVisible(..)
  , HasExtent(..)
  , HasMinResolution(..)
  , HasMaxResolution(..)
  , HasZIndex(..)
  , HasLayers(..)
  , HasTileSource(..)
  , HasImageSource(..)
) where


import Reflex.OpenLayers.Source
import Reflex.OpenLayers.Util

import Reflex
import Reflex.Dom

import Data.Default (Default(def))
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (fromMaybe)
import Control.Lens
import Control.Monad (when, liftM, forM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHCJS.Marshal (ToJSVal(toJSVal), FromJSVal(fromJSVal))
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Types
import GHCJS.Foreign.QQ

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

data LayerBase t p = LayerBase {
      _layerBaseOpacity       :: p t "opacity" Opacity
    , _layerBaseVisible       :: p t "visible" Bool
    , _layerBaseZIndex        :: p t "zIndex" Int
    , _layerBaseExtent        :: p t "extent" (Maybe Extent)
    , _layerBaseMinResolution :: p t "minResolution" (Maybe Double)
    , _layerBaseMaxResolution :: p t "maxResolution" (Maybe Double)
    }
makeFields ''LayerBase

instance Reflex t => Default (LayerBase t Property) where
  def = LayerBase {
       _layerBaseOpacity          = property 1
     , _layerBaseVisible          = property True
     , _layerBaseZIndex           = property 0
     , _layerBaseExtent           = property Nothing
     , _layerBaseMinResolution    = property Nothing
     , _layerBaseMaxResolution    = property Nothing
     }

type LayerSet = M.Map Int

fromList :: [a] -> LayerSet a
fromList = M.fromList . zip [0..]

pushLayer :: a -> LayerSet a -> LayerSet a
pushLayer v m = case M.maxViewWithKey m of
  Nothing          -> M.singleton (toEnum 0) v
  Just ((k, _), _) -> M.insert (succ k) v m

data JSLayer t
  = JSImage { _jSLayerBase   :: LayerBase t PropertyObj
            , _jSLayerImageSource :: Source Raster Image t
            , _jSLayerVal  :: JSVal
            }
  | JSTile  { _jSLayerBase   :: LayerBase t PropertyObj
            , _jSLayerTileSource :: Source Raster Tile t
            , _jSLayerVal  :: JSVal
            }
  | JSGroup { _jSLayerBase   :: LayerBase t PropertyObj
            , _jSLayerLayers :: LayerSet (JSLayer t)
            , _jSLayerVal  :: JSVal
            }
makeFields ''JSLayer

instance PToJSVal (JSLayer t) where pToJSVal = _jSLayerVal
instance ToJSVal  (JSLayer t) where toJSVal = return . _jSLayerVal


data Layer t
  = Image { _layerBase   :: LayerBase t Property
          , _layerImageSource :: Source Raster Image t
          }
  | Tile  { _layerBase   :: LayerBase t Property
          , _layerTileSource :: Source Raster Tile t
          }
  | Group { _layerBase   :: LayerBase t Property
          , _layerLayers :: LayerSet (Layer t)
          }
makeFields ''Layer

instance HasOpacity (Layer t) (Property t "opacity" Opacity) where
  opacity = base . opacity
instance HasVisible (Layer t) (Property t "visible" Bool) where
  visible = base . visible
instance HasZIndex (Layer t) (Property t "zIndex" Int) where
  zIndex = base . zIndex
instance HasExtent (Layer t) (Property t "extent" (Maybe Extent)) where
  extent = base . extent
instance HasMinResolution (Layer t) (Property t "minResolution" (Maybe Double)) where
  minResolution = base . minResolution
instance HasMaxResolution (Layer t) (Property t "maxResolution" (Maybe Double)) where
  maxResolution = base . maxResolution


instance HasOpacity (JSLayer t) (PropertyObj t "opacity" Opacity) where
  opacity = base . opacity
instance HasVisible (JSLayer t) (PropertyObj t "visible" Bool) where
  visible = base . visible
instance HasZIndex (JSLayer t) (PropertyObj t "zIndex" Int) where
  zIndex = base . zIndex
instance HasExtent (JSLayer t) (PropertyObj t "extent" (Maybe Extent)) where
  extent = base . extent
instance HasMinResolution (JSLayer t) (PropertyObj t "minResolution" (Maybe Double)) where
  minResolution = base . minResolution
instance HasMaxResolution (JSLayer t) (PropertyObj t "maxResolution" (Maybe Double)) where
  maxResolution = base . maxResolution


instance HasNamedProperty (JSLayer t) "opacity" Opacity Opacity t
instance HasNamedProperty (JSLayer t) "visible" Bool Bool t
instance HasNamedProperty (JSLayer t) "zIndex" Int Int t
instance HasNamedProperty (JSLayer t) "extent" (Maybe Extent) (Maybe Extent) t
instance HasNamedProperty (JSLayer t) "maxResolution" (Maybe Double) (Maybe Double) t
instance HasNamedProperty (JSLayer t) "minResolution" (Maybe Double) (Maybe Double) t

image :: Reflex t => Source Raster Image t -> Layer t
image = Image def

tile :: Reflex t => Source Raster Tile t -> Layer t
tile = Tile def

group :: Reflex t => LayerSet (Layer t) -> Layer t
group = Group def

mkLayer
  :: MonadWidget t m
  => (Int, Layer t)
  -> m (JSLayer t)
mkLayer (key,l) =
  case l of
    Image{_layerImageSource} -> do
      s <- mkSource _layerImageSource
      j <- liftIO [jsu|$r=new ol.layer.Image({source:`s});|]
      let r = JSImage undefined _layerImageSource j
      b <- updateBase r (l^.base)
      return $ r {_jSLayerBase=b}
    Tile{_layerTileSource} -> do
      s <- mkSource _layerTileSource
      j <- liftIO [jsu|$r=new ol.layer.Tile({source: `s});|]
      let r = JSTile undefined _layerTileSource j
      b <- updateBase r (l^.base)
      return $ r {_jSLayerBase=b}
    Group{_layerLayers} -> do
      ls <- forM (M.toAscList _layerLayers) $ \(ix, l) -> do
        jsL <- mkLayer (ix,l)
        return (ix, jsL)
      jsLs <- liftIO (toJSVal (map snd ls))
      j <- liftIO [jsu|$r=new ol.layer.Group({layers:`jsLs});|]
      let r = JSGroup undefined (M.fromList ls) j
      b <- updateBase r (l^.base)
      return $ r {_jSLayerBase=b}

updateBase r b =
  LayerBase <$> initProperty r (b^.opacity)
            <*> initProperty r (b^.visible)
            <*> initProperty r (b^.zIndex)
            <*> initProperty r (b^.extent)
            <*> initProperty r (b^.minResolution)
            <*> initProperty r (b^.maxResolution)
