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
      _layerBaseOpacity       :: p t Opacity
    , _layerBaseVisible       :: p t Bool
    , _layerBaseZIndex        :: p t Int
    , _layerBaseExtent        :: p t (Maybe Extent)
    , _layerBaseMinResolution :: p t (Maybe Double)
    , _layerBaseMaxResolution :: p t (Maybe Double)
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
  = JSImage { _jSLayerBase   :: LayerBase t Dynamic
            , _jSLayerVal  :: JSVal
            }
  | JSTile  { _jSLayerBase   :: LayerBase t Dynamic
            , _jSLayerVal  :: JSVal
            }
  | JSGroup { _jSLayerBase   :: LayerBase t Dynamic
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
          , _layerLayers :: Dynamic t (LayerSet (JSLayer t))
          }
makeFields ''Layer

instance HasOpacity (Layer t) (Property t Opacity) where
  opacity = base . opacity
instance HasVisible (Layer t) (Property t Bool) where
  visible = base . visible
instance HasZIndex (Layer t) (Property t Int) where
  zIndex = base . zIndex
instance HasExtent (Layer t) (Property t (Maybe Extent)) where
  extent = base . extent
instance HasMinResolution (Layer t) (Property t (Maybe Double)) where
  minResolution = base . minResolution
instance HasMaxResolution (Layer t) (Property t (Maybe Double)) where
  maxResolution = base . maxResolution


instance HasOpacity (JSLayer t) (Dynamic t Opacity) where
  opacity = base . opacity
instance HasVisible (JSLayer t) (Dynamic t Bool) where
  visible = base . visible
instance HasZIndex (JSLayer t) (Dynamic t Int) where
  zIndex = base . zIndex
instance HasExtent (JSLayer t) (Dynamic t (Maybe Extent)) where
  extent = base . extent
instance HasMinResolution (JSLayer t) (Dynamic t (Maybe Double)) where
  minResolution = base . minResolution
instance HasMaxResolution (JSLayer t) (Dynamic t (Maybe Double)) where
  maxResolution = base . maxResolution



image :: Reflex t => Source Raster Image t -> Layer t
image = Image def

tile :: Reflex t => Source Raster Tile t -> Layer t
tile = Tile def

group :: Reflex t => Dynamic t (LayerSet (JSLayer t)) -> Layer t
group = Group def

mkLayer :: MonadWidget t m => Layer t -> m (JSLayer t)
mkLayer l = do
  liftIO $ putStrLn "mkLayer"
  case l of
    Image{_layerImageSource} -> do
      s <- mkSource _layerImageSource
      j <- liftIO [jsu|$r=new ol.layer.Image({source:`s});|]
      b <- updateBase j (l^.base)
      return $ JSImage b j
    Tile{_layerTileSource} -> do
      s <- mkSource _layerTileSource
      j <- liftIO [jsu|$r=new ol.layer.Tile({source: `s});|]
      b <- updateBase j (l^.base)
      return $ JSTile b j
    Group{_layerLayers} -> do
      j <- liftIO [jsu|$r=new ol.layer.Group({});|]
      dynInitialize _layerLayers $ \ls -> liftIO $ do
        jsLs <- liftIO (toJSVal (M.elems ls))
        [jsu_|`j.setLayers(new ol.Collection(`jsLs));|]
      b <- updateBase j (l^.base)
      return $ JSGroup b j
  where
    updateBase r b =
      LayerBase <$> initProperty "opacity" r (b^.opacity)
                <*> initProperty "visible" r (b^.visible)
                <*> initProperty "zIndex" r (b^.zIndex)
                <*> initProperty "extent" r (b^.extent)
                <*> initProperty "minResolution" r (b^.minResolution)
                <*> initProperty "maxResolution" r (b^.maxResolution)
