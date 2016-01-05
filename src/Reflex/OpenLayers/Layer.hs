{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , HasSource(..)
  , HasLayers(..)
) where


import Reflex.OpenLayers.Source (Source, mkSource)
import Reflex.OpenLayers.Util

import Reflex
import Reflex.Dom

import Data.Default (Default(def))
import qualified Data.Map as M
import Data.Monoid
import Control.Lens
import Control.Monad (when, liftM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHCJS.Marshal (toJSVal)
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

data LayerBase
  = LayerBase {
      _layerBaseOpacity       :: Opacity
    , _layerBaseVisible       :: Bool
    , _layerBaseZIndex        :: Int
    , _layerBaseExtent        :: (Maybe Extent)
    , _layerBaseMinResolution :: (Maybe Double)
    , _layerBaseMaxResolution :: (Maybe Double)
    }
makeFields ''LayerBase

instance Default LayerBase where
  def = LayerBase {
       _layerBaseOpacity          = 1
     , _layerBaseVisible          = True
     , _layerBaseZIndex           = 0
     , _layerBaseExtent           = Nothing
     , _layerBaseMinResolution    = Nothing
     , _layerBaseMaxResolution    = Nothing
     }

type LayerSet t = M.Map Int (Layer t)

fromList :: [Layer t] -> LayerSet t
fromList = M.fromList . zip [0..]

pushLayer :: Layer t -> LayerSet t -> LayerSet t
pushLayer v m = case M.maxViewWithKey m of
  Nothing          -> M.singleton (toEnum 0) v
  Just ((k, _), _) -> M.insert (succ k) v m


data Layer t
  = Image { _layerBase   :: LayerBase
          , _layerSource :: Source t
          }
  | Tile  { _layerBase   :: LayerBase
          , _layerSource :: Source t
          }
  | Group { _layerBase   :: LayerBase
          , _layerLayers :: Dynamic t (LayerSet t)
          }
makeFields ''Layer

instance HasOpacity (Layer t) (Opacity) where
  opacity = base . opacity
instance HasVisible (Layer t) (Bool) where
  visible = base . visible
instance HasZIndex (Layer t) (Int) where
  zIndex = base . zIndex
instance HasExtent (Layer t) ((Maybe Extent)) where
  extent = base . extent
instance HasMinResolution (Layer t) ((Maybe Double)) where
  minResolution = base . minResolution
instance HasMaxResolution (Layer t) ((Maybe Double)) where
  maxResolution = base . maxResolution

image :: Reflex t => (Source t) -> Layer t
image = Image def

tile :: Reflex t => (Source t) -> Layer t
tile = Tile def

group :: Reflex t => Dynamic t (LayerSet t) -> Layer t
group = Group def

mkLayer :: MonadWidget t m => (Int, Layer t) -> m JSVal
mkLayer (key,l) = do
  r <- case l of
    Image{_layerSource} -> do
      s <- mkSource _layerSource
      liftIO [jsu|$r=new ol.layer.Image({source:`s});|]
    Tile{_layerSource} -> do
      s <- mkSource _layerSource
      liftIO [jsu|$r=new ol.layer.Tile({source: `s});|]
    Group{_layerLayers} -> do
      r <- liftIO [jsu|$r=new ol.layer.Group({});|]
      dynInitializeWith (mapM mkLayer . M.toAscList) _layerLayers $
        \newLayers -> liftIO $ do
          ls <- toJSVal newLayers
          [jsu_|h$updateGroupLayers(`r, `ls);|]
      return r
  setProp "opacity" r (l^.opacity)
  setProp "visible" r (l^.visible)
  setProp "zIndex" r (l^.zIndex)
  setProp "extent" r (l^.extent)
  setProp "minResolution" r (l^.minResolution)
  setProp "maxResolution" r (l^.maxResolution)
  liftIO $ [jsu_|`r['h$key']=`key|]
  return r

setProp :: (MonadIO m, PToJSVal a) => String -> JSVal -> a -> m ()
setProp n v a = liftIO [jsu_|if(`a!==null){`v['set'](`n, `a)};|]
