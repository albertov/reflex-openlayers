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

data LayerBase t p
  = LayerBase {
      _layerBaseOpacity       :: p t "opacity" Opacity
    , _layerBaseVisible       :: p t "visible" Bool
    , _layerBaseZIndex        :: p t "zIndex" Int
    , _layerBaseExtent        :: p t "extent" (Maybe Extent)
    , _layerBaseMinResolution :: p t "minResolution" (Maybe Double)
    , _layerBaseMaxResolution :: p t "maxResolution" (Maybe Double)
    }
makeFields ''LayerBase
makeLenses ''LayerBase

instance Reflex t => Default (LayerBase t Property) where
  def = LayerBase {
       _layerBaseOpacity          = property 1
     , _layerBaseVisible          = property True
     , _layerBaseZIndex           = property 0
     , _layerBaseExtent           = property Nothing
     , _layerBaseMinResolution    = property Nothing
     , _layerBaseMaxResolution    = property Nothing
     }

type LayerSet t p = M.Map Int (Layer t p)

fromList :: [Layer t p] -> LayerSet t p
fromList = M.fromList . zip [0..]

pushLayer :: Layer t p -> LayerSet t p -> LayerSet t p
pushLayer v m = case M.maxViewWithKey m of
  Nothing          -> M.singleton (toEnum 0) v
  Just ((k, _), _) -> M.insert (succ k) v m

newtype JSLayer = JSLayer JSVal
  deriving (PToJSVal, PFromJSVal, ToJSVal, FromJSVal)

instance IsJSVal JSLayer

data Layer t p
  = Image { _layerBase   :: LayerBase t p
          , _layerImageSource :: Source Raster Image t
          }
  | Tile  { _layerBase   :: LayerBase t p
          , _layerTileSource :: Source Raster Tile t
          }
  | Group { _layerBase   :: LayerBase t p
          , _layerLayers :: LayerSet t p
          }
makeFields ''Layer

instance HasOpacity (Layer t p) (p t "opacity" Opacity) where
  opacity = base . opacity
instance HasVisible (Layer t p) (p t "visible" Bool) where
  visible = base . visible
instance HasZIndex (Layer t p) (p t "zIndex" Int) where
  zIndex = base . zIndex
instance HasExtent (Layer t p) (p t "extent" (Maybe Extent)) where
  extent = base . extent
instance HasMinResolution (Layer t p) (p t "minResolution" (Maybe Double)) where
  minResolution = base . minResolution
instance HasMaxResolution (Layer t p) (p t "maxResolution" (Maybe Double)) where
  maxResolution = base . maxResolution

instance HasNamedProperty JSLayer "opacity" Opacity Opacity t
instance HasNamedProperty JSLayer "visible" Bool Bool t
instance HasNamedProperty JSLayer "zIndex" Int Int t
instance HasNamedProperty JSLayer "extent" (Maybe Extent) (Maybe Extent) t
instance HasNamedProperty JSLayer "maxResolution" (Maybe Double) (Maybe Double) t
instance HasNamedProperty JSLayer "minResolution" (Maybe Double) (Maybe Double) t

image :: Reflex t => Source Raster Image t -> Layer t Property
image = Image def

tile :: Reflex t => Source Raster Tile t -> Layer t Property
tile = Tile def

group :: Reflex t => LayerSet t Property -> Layer t Property
group = Group def

mkLayer
  :: MonadWidget t m
  => (Int, Layer t Property)
  -> m (JSLayer, Layer t PropertyObj)
mkLayer (key,l) =
  case l of
    Image{_layerImageSource} -> do
      s <- mkSource _layerImageSource
      r <- liftIO [jsu|$r=new ol.layer.Image({source:`s});|]
      b <- updateBase r (l^.base)
      return (r, Image b _layerImageSource)
    Tile{_layerTileSource} -> do
      s <- mkSource _layerTileSource
      r <- liftIO [jsu|$r=new ol.layer.Tile({source: `s});|]
      b <- updateBase r (l^.base)
      return (r, Tile b _layerTileSource)
    Group{_layerLayers} -> do
      (js,lg) <- fmap unzip $ forM (M.toAscList _layerLayers) $ \(ix, l) -> do
        (jsL,lp) <- mkLayer (ix,l)
        return (jsL, (ix,lp))
      ls <- liftIO (toJSVal js)
      r <- liftIO [jsu|$r=new ol.layer.Group({layers:`ls});|]
      b <- updateBase r (l^.base)
      return (r, Group b (M.fromList lg))

updateBase r b =
  LayerBase <$> initProperty r (b^.opacity)
            <*> initProperty r (b^.visible)
            <*> initProperty r (b^.zIndex)
            <*> initProperty r (b^.extent)
            <*> initProperty r (b^.minResolution)
            <*> initProperty r (b^.maxResolution)


setPropIfNotNull :: (MonadIO m, PToJSVal a) => String -> JSVal -> a -> m ()
setPropIfNotNull n v a = liftIO [jsu_|if(`a!==null){`v['set'](`n, `a)};|]
