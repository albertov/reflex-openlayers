{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Reflex.OpenLayers.Layer (
    Layer (..)
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
import Reflex.OpenLayers.Collection
import Reflex.OpenLayers.Util

import Reflex
import Reflex.Dom

import Data.Default (Default)
import qualified Data.Map as M
import Control.Lens
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHCJS.Marshal (ToJSVal, FromJSVal)
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
  pFromJSVal l = Extent [js'|$r=`l[0]|] [js'|$r=`l[1]|]
                        [js'|$r=`l[2]|] [js'|$r=`l[3]|]


--
-- Layer
--

data LayerBase t p = LayerBase {
      _layerBaseOpacity       :: (p t Opacity)
    , _layerBaseVisible       :: (p t Bool)
    , _layerBaseZIndex        :: (p t Int)
    , _layerBaseExtent        :: (p t (Maybe Extent))
    , _layerBaseMinResolution :: (p t (Maybe Double))
    , _layerBaseMaxResolution :: (p t (Maybe Double))
    }
makeFields ''LayerBase

instance Reflex t => Default (LayerBase t Property) where
  def = LayerBase {
       _layerBaseOpacity          = constProperty 1
     , _layerBaseVisible          = constProperty True
     , _layerBaseZIndex           = constProperty 0
     , _layerBaseExtent           = constProperty Nothing
     , _layerBaseMinResolution    = constProperty Nothing
     , _layerBaseMaxResolution    = constProperty Nothing
     }

type LayerSet = M.Map Int

fromList :: [a] -> LayerSet a
fromList = M.fromList . zip [0..]

pushLayer :: a -> LayerSet a -> LayerSet a
pushLayer v m = case M.maxViewWithKey m of
  Nothing          -> M.singleton (toEnum 0) v
  Just ((k, _), _) -> M.insert (succ k) v m


data Layer t p
  = Image { _layerBase   :: LayerBase t p
          , _layerImageSource :: p t (Source Raster Image t)
          }
  | Tile  { _layerBase   :: LayerBase t p
          , _layerTileSource :: p t (Source Raster Tile t)
          }
  | Group { _layerBase   :: LayerBase t p
          , _layerLayers :: Dynamic t (LayerSet (Layer t p))
          }
makeFields ''Layer

instance HasOpacity (Layer t p) (p t Opacity) where
  opacity = base . opacity
instance HasVisible (Layer t p) (p t Bool) where
  visible = base . visible
instance HasZIndex (Layer t p) (p t Int) where
  zIndex = base . zIndex
instance HasExtent (Layer t p) (p t (Maybe Extent)) where
  extent = base . extent
instance HasMinResolution (Layer t p) (p t (Maybe Double)) where
  minResolution = base . minResolution
instance HasMaxResolution (Layer t p) (p t (Maybe Double)) where
  maxResolution = base . maxResolution


image :: Reflex t => Source Raster Image t -> Layer t Property
image = Image def . constProperty

tile :: Reflex t => Source Raster Tile t -> Layer t Property
tile = Tile def . constProperty

group
  :: Reflex t
  => Dynamic t (LayerSet (Layer t Property)) -> Layer t Property
group = Group def

mkLayer :: MonadWidget t m => Layer t Property -> m (JSVal, Layer t Dynamic)
mkLayer lyr = do
  case lyr of
    Image{_layerImageSource} -> do
      j <- liftIO [jsu|$r=new ol.layer.Image();|]
      dynSource <-
        initPropertyWith Nothing mkSource "source" j _layerImageSource
      b <- updateBase j (lyr^.base)
      return (j, Image b dynSource)
    Tile{_layerTileSource} -> do
      j <- liftIO [jsu|$r=new ol.layer.Tile();|]
      dynSource <-
        initPropertyWith Nothing mkSource "source" j _layerTileSource
      b <- updateBase j (lyr^.base)
      return (j, Tile b dynSource)
    Group{_layerLayers} -> do
      dynMkLayers <- list _layerLayers (\dL -> sample (current dL) >>= mkLayer)
      dynLayers <- mapDyn (M.map snd) dynMkLayers
      c <- mkCollection =<< mapDyn (M.map fst) dynMkLayers
      j <- liftIO [jsu|$r=new ol.layer.Group({layers:`c});|]
      b <- updateBase j (lyr^.base)
      return (j, Group b dynLayers)
  where
    updateBase r b =
      LayerBase <$> initProperty "opacity" r (b^.opacity)
                <*> initProperty "visible" r (b^.visible)
                <*> initProperty "zIndex" r (b^.zIndex)
                <*> initProperty "extent" r (b^.extent)
                <*> initProperty "minResolution" r (b^.minResolution)
                <*> initProperty "maxResolution" r (b^.maxResolution)
