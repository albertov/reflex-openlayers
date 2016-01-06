{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
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
  deriving Show
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

instance SyncJS LayerBase t where
  syncJS jsObj newHS = do
    syncEqProp jsObj "opacity"       (newHS^.opacity)
    syncEqProp jsObj "visible"       (newHS^.visible)
    syncEqProp jsObj "zIndex"        (newHS^.zIndex)
    syncEqProp jsObj "extent"        (newHS^.extent)
    syncEqProp jsObj "minResolution" (newHS^.minResolution)
    syncEqProp jsObj "maxResolution" (newHS^.maxResolution)
    return Nothing


type LayerSet t = M.Map Int (Layer t)

fromList :: [Layer t] -> LayerSet t
fromList = M.fromList . zip [0..]

pushLayer :: Layer t -> LayerSet t -> LayerSet t
pushLayer v m = case M.maxViewWithKey m of
  Nothing          -> M.singleton (toEnum 0) v
  Just ((k, _), _) -> M.insert (succ k) v m


data Layer t
  = Image { _layerBase   :: LayerBase
          , _layerImageSource :: Source Raster NotTiled t
          }
  | Tile  { _layerBase   :: LayerBase
          , _layerTileSource :: Source Raster Tiled t
          }
  | Group { _layerBase   :: LayerBase
          , _layerLayers :: LayerSet t
          }
  deriving Show
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

image :: Reflex t => Source Raster NotTiled t -> Layer t
image = Image def

tile :: Reflex t => Source Raster Tiled t -> Layer t
tile = Tile def

group :: Reflex t => LayerSet t -> Layer t
group = Group def

mkLayer :: MonadWidget t m => (Int, Layer t) -> m JSVal
mkLayer (key,l) = do
  r <- case l of
    Image{_layerImageSource} -> do
      s <- mkSource _layerImageSource
      liftIO [jsu|$r=new ol.layer.Image({source:`s});|]
    Tile{_layerTileSource} -> do
      s <- mkSource _layerTileSource
      liftIO [jsu|$r=new ol.layer.Tile({source: `s});|]
    Group{_layerLayers} -> do
      ls <- liftIO . toJSVal =<< mapM mkLayer (M.toAscList _layerLayers)
      liftIO [jsu|$r=new ol.layer.Group({layers:`ls});|]
  setPropIfNotNull "opacity" r (l^.opacity)
  setPropIfNotNull "visible" r (l^.visible)
  setPropIfNotNull "zIndex" r (l^.zIndex)
  setPropIfNotNull "extent" r (l^.extent)
  setPropIfNotNull "minResolution" r (l^.minResolution)
  setPropIfNotNull "maxResolution" r (l^.maxResolution)
  liftIO $ [jsu_|`r['ol$key']=`key|]
  setStableName r l
  return r

setPropIfNotNull :: (MonadIO m, PToJSVal a) => String -> JSVal -> a -> m ()
setPropIfNotNull n v a = liftIO [jsu_|if(`a!==null){`v['set'](`n, `a)};|]

instance SyncJS (Layer t) t where
  syncJS jsObj newHS | fastEq jsObj newHS = return Nothing
  syncJS jsObj newHS = do
    setStableName jsObj newHS
    syncJS_ jsObj (newHS^.base)
    case newHS of
      Image{_layerImageSource} -> updateSource _layerImageSource
      Tile{_layerTileSource} -> updateSource _layerTileSource
      Group{_layerLayers} -> do
        newLs <- forM (M.toAscList _layerLayers) $ \(key, l) -> do
          case [jsu'|$r=`keyMap[`key];|] of
            Just jsL -> syncJS_ jsL l >> return jsL
            Nothing  -> mkLayer (key, l)
        liftIO $ do
          jsNewLs <- toJSVal newLs
          [jsu_|`jsObj.setLayers(new ol.Collection(`jsNewLs));|]
        return Nothing

    where
      updateSource :: MonadWidget t m => Source r k t -> m (Maybe JSVal)
      updateSource newSource = do
        mNewVal <- syncJS [jsu'|$r=`jsObj.getSource();|] newSource
        case mNewVal of
          Just newVal -> liftIO [jsu_|`jsObj.setSource(`newVal);|]
          Nothing -> return ()
        return Nothing

      keyMap = fromMaybe (error "syncJS(Layer(Group)): missing key on js obj")
                         (mkKeyMap [js'|$r=`jsObj.getLayers().getArray();|])
      mkKeyMap :: JSVal -> Maybe JSVal
      mkKeyMap arr = [jsu'|
        $r = {};
        for (var i=0; i<`arr.length; i++) {
          var o=`arr[i], h=o["ol$key"];
          if (typeof h != "undefined") {
            $r[h] = o;
          } else {
            $r=null;
            break;
          }
        }|]
