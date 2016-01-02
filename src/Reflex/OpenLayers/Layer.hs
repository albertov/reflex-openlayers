{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Reflex.OpenLayers.Layer (
    IsLayer (..)
  , Layer

  , IsLayerConfig (..)
  , LayerConfig

  , Extent (..)
  , Opacity

  , ImageConfig (..)
  , Image (..)
  , image

  , TileConfig (..)
  , Tile (..)
  , tile

  , GroupConfig (..)
  , Group (..)

  -- Properties
  , HasOpacity(..)
  , HasSource(..)
  , HasVisible(..)
  , HasLayers (..)
  , HasExtent(..)
  , HasMinResolution(..)
  , HasMaxResolution(..)
) where

import Reflex.OpenLayers.Source (
    SourceConfig
  , Source
  )
import qualified Reflex.OpenLayers.Source as S
import Reflex.OpenLayers.Util

import Reflex
import Reflex.Dom

import qualified JavaScript.Object as O
import Data.Default (Default(def))
import Data.Typeable (Typeable, cast)
import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import GHCJS.Marshal (ToJSVal(toJSVal))
import GHCJS.Types
import GHCJS.Foreign (isNull)
import GHCJS.DOM.Types (toJSString)
import Language.Haskell.TH (reify)

type Opacity = Double

data Extent
  = Extent { _xMin :: !Double
           , _yMin :: !Double
           , _xMax :: !Double
           , _yMax :: !Double
           } deriving Show

instance ToJSVal Extent where
  toJSVal Extent{..} = toJSVal (_xMin, _yMin, _xMax, _yMax)



foreign import javascript unsafe "new ol['layer'][$1]($2)"
  mkLayer :: JSString -> O.Object -> IO JSVal

--
-- Layer
--
declareProperties [
    "opacity"
  , "source"
  , "visible"
  , "extent"
  , "layers"
  , "minResolution"
  , "maxResolution"
  ]

class ( HasVisible (l t) (Dynamic t Bool)
      , HasOpacity (l t) (Dynamic t Opacity)
      , HasExtent (l t) (Maybe Extent)
      , HasMinResolution (l t) (Maybe Double)
      , HasMaxResolution (l t) (Maybe Double)
      ) => IsLayerConfig l t where
  layer :: MonadWidget t m => l t -> m (Layer t)

-- TODO: Remove once extent and minmax resolutions are dynamic
layerOptions :: IsLayerConfig l t => l t -> IO O.Object
layerOptions cfg = do
  opts <- O.create
  let set n v = when (not (isNull v)) (O.setProp n v opts)
  toJSVal (cfg^.extent)  >>= set "extent"
  toJSVal (cfg^.minResolution) >>= set "minResolution"
  toJSVal (cfg^.maxResolution) >>= set "maxResolution"
  return opts

data LayerConfig t =
  forall l. IsLayerConfig l t => LayerConfig (l t)

instance IsLayerConfig LayerConfig t where
  layer (LayerConfig l) = layer l

instance HasOpacity (LayerConfig t) (Dynamic t Opacity) where
  opacity =
    lens (\(LayerConfig l) -> l^.opacity)
         (\(LayerConfig l) o -> LayerConfig (l & opacity.~o))

instance HasVisible (LayerConfig t) (Dynamic t Bool) where
  visible =
    lens (\(LayerConfig l) -> l^.visible)
         (\(LayerConfig l) o -> LayerConfig (l & visible.~o))
instance HasExtent (LayerConfig t) (Maybe Extent) where
  extent =
    lens (\(LayerConfig l) -> l^.extent)
         (\(LayerConfig l) o -> LayerConfig (l & extent.~o))
instance HasMinResolution (LayerConfig t) (Maybe Double) where
  minResolution =
    lens (\(LayerConfig l) -> l^.minResolution)
         (\(LayerConfig l) o -> LayerConfig (l & minResolution.~o))
instance HasMaxResolution (LayerConfig t) (Maybe Double) where
  maxResolution =
    lens (\(LayerConfig l) -> l^.maxResolution)
         (\(LayerConfig l) o -> LayerConfig (l & maxResolution.~o))

class ( Typeable l, ToJSVal (l t)
      ) => IsLayer l t where
  layer_downcast :: Typeable t => Layer t -> Maybe (l t)
  layer_downcast (Layer l) = cast l
  {-# INLINE layer_downcast #-}

data Layer t =  forall l. IsLayer l t => Layer (l t)

instance IsLayer Layer t where
  layer_downcast = Just


instance ToJSVal (Layer t) where
  toJSVal (Layer l) = toJSVal l

--
-- Image
--

newtype Image t = Image JSVal deriving (Typeable, ToJSVal)
instance IsLayer Image t

data ImageConfig t
  = ImageConfig {
       _imageConfig_source           :: Dynamic t (SourceConfig t)
     , _imageConfig_opacity          :: Dynamic t Opacity
     , _imageConfig_visible          :: Dynamic t Bool
     , _imageConfig_extent           :: Maybe Extent
     , _imageConfig_minResolution    :: Maybe Double
     , _imageConfig_maxResolution    :: Maybe Double
     }

hasProperties ''ImageConfig [
    "opacity"
  , "source"
  , "visible"
  , "extent"
  , "minResolution"
  , "maxResolution"
  ]


image :: Reflex t => Dynamic t (SourceConfig t) -> LayerConfig t
image src = LayerConfig $ ImageConfig {
       _imageConfig_source           = src
     , _imageConfig_opacity          = constDyn 1
     , _imageConfig_visible          = constDyn True
     , _imageConfig_extent           = Nothing
     , _imageConfig_minResolution    = Nothing
     , _imageConfig_maxResolution    = Nothing
     }


initSource
  :: ( S.IsSourceConfig l, MonadWidget t m
     , HasSource s (Dynamic t (l t1))
     ) => JSVal -> s -> m ()
initSource = initOLPropWith "setSource" source (S.source)

initVisible
  :: ( MonadWidget t m
     , HasVisible s (Dynamic t Bool)
     ) => JSVal -> s -> m ()
initVisible = initOLProp "setVisible" visible

initOpacity
  :: ( MonadWidget t m
     , HasOpacity s (Dynamic t Opacity)
     ) => JSVal -> s -> m ()
initOpacity = initOLProp "setOpacity" opacity

initLayerCommon jsVal cfg = do
  initVisible jsVal cfg
  initOpacity jsVal cfg

instance IsLayerConfig ImageConfig t where
  layer cfg@ImageConfig{..} = do
    imgVal <- liftIO $ do
      imgOpts <- layerOptions cfg
      mkLayer "Image" imgOpts
    initLayerCommon imgVal cfg
    initSource imgVal cfg
    return (Layer (Image imgVal))



--
-- Tile
--

newtype Tile t = Tile JSVal deriving (Typeable, ToJSVal)
instance IsLayer Tile t

data TileConfig t
  = TileConfig {
       _tileConfig_source           :: Dynamic t (SourceConfig t)
     , _tileConfig_opacity          :: Dynamic t Opacity
     , _tileConfig_visible          :: Dynamic t Bool
     , _tileConfig_extent           :: Maybe Extent
     , _tileConfig_minResolution    :: Maybe Double
     , _tileConfig_maxResolution    :: Maybe Double
     }
hasProperties ''TileConfig [
    "opacity"
  , "source"
  , "visible"
  , "extent"
  , "minResolution"
  , "maxResolution"
  ]

tile :: Reflex t => Dynamic t (SourceConfig t) -> LayerConfig t
tile src = LayerConfig $ TileConfig {
       _tileConfig_source           = src
     , _tileConfig_opacity          = constDyn 1
     , _tileConfig_visible          = constDyn True
     , _tileConfig_extent           = Nothing
     , _tileConfig_minResolution    = Nothing
     , _tileConfig_maxResolution    = Nothing
     }

instance IsLayerConfig TileConfig t where
  layer cfg@TileConfig{..} = do
    imgVal <- liftIO $ do
      imgOpts <- layerOptions cfg
      mkLayer "Tile" imgOpts
    initLayerCommon imgVal cfg
    initSource imgVal cfg
    return (Layer (Tile imgVal))


--
-- Group
--

newtype Group t = Group JSVal deriving (Typeable, ToJSVal)
instance IsLayer Group t

data GroupConfig t
  = GroupConfig {
       _groupConfig_layers           :: [LayerConfig t]
     , _groupConfig_opacity          :: Dynamic t Opacity
     , _groupConfig_visible          :: Dynamic t Bool
     , _groupConfig_extent           :: Maybe Extent
     , _groupConfig_minResolution    :: Maybe Double
     , _groupConfig_maxResolution    :: Maybe Double
     }
hasProperties ''GroupConfig [
    "opacity"
  , "layers"
  , "visible"
  , "extent"
  , "minResolution"
  , "maxResolution"
  ]

instance Reflex t => Default (GroupConfig t) where
  def = GroupConfig {
       _groupConfig_layers           = []
     , _groupConfig_opacity          = constDyn 1
     , _groupConfig_visible          = constDyn True
     , _groupConfig_extent           = Nothing
     , _groupConfig_minResolution    = Nothing
     , _groupConfig_maxResolution    = Nothing
     }

instance IsLayerConfig GroupConfig t where
  layer cfg  = do
    layers <- mapM (\(LayerConfig l) -> layer l) (cfg^.layers)
    jsVal <- liftIO $ do
      opts <- layerOptions cfg
      jsLayers <- toJSVal layers
      O.setProp "layers" jsLayers opts
      mkLayer "Group" opts
    initLayerCommon jsVal cfg
    return (Layer (Group jsVal))
