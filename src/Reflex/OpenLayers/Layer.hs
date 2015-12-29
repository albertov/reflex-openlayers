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
  , HasVisibility(..)
  , HasLayers (..)
  , HasExtent(..)
  , HasResolutionRange(..)
) where

import Reflex.OpenLayers.Source (
    SourceConfig
  , Source
  )
import qualified Reflex.OpenLayers.Source as S
import Reflex.OpenLayers.Util (olSetter)

import Reflex
import Reflex.Dom

import qualified JavaScript.Object as O
import Data.Default (Default(def))
import Data.Typeable (Typeable, cast)
import Control.Lens
import Control.Monad (liftM, when, (>=>))
import Control.Monad.IO.Class (liftIO)
import GHCJS.Marshal (ToJSVal(toJSVal))
import GHCJS.Types
import GHCJS.Foreign (isNull)
import GHCJS.DOM.Types (toJSString)

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

data Visibility = Visible | Hidden deriving Show

isVisible :: Visibility -> Bool
isVisible Visible = True
isVisible Hidden = False

--
-- Layer
--
class HasOpacity o a | o->a where opacity :: Lens' o a
class HasSource o a | o->a where source :: Lens' o a
class HasVisibility o a | o->a where visible :: Lens' o a
class HasExtent o a | o->a where extent :: Lens' o a
class HasLayers o a | o->a where layers :: Lens' o a
class HasResolutionRange o a | o->a where
  minResolution :: Lens' o a
  maxResolution :: Lens' o a

class ( HasVisibility (l t) Visibility
      , HasOpacity (l t) Opacity
      , HasExtent (l t) (Maybe Extent)
      , HasResolutionRange (l t) (Maybe Double)
      ) => IsLayerConfig l t where
  layer :: MonadWidget t m => l t -> m (Layer t)

layerOptions :: IsLayerConfig l t => l t -> IO O.Object
layerOptions cfg = do
  opts <- O.create
  let set n v = when (not (isNull v)) (O.setProp n v opts)
  toJSVal (views visible isVisible cfg) >>= set "visible"
  toJSVal (cfg^.opacity) >>= set "opacity"
  toJSVal (cfg^.extent)  >>= set "extent"
  toJSVal (cfg^.minResolution) >>= set "minResolution"
  toJSVal (cfg^.maxResolution) >>= set "maxResolution"
  return opts

data LayerConfig t =
  forall l. IsLayerConfig l t => LayerConfig (l t)

instance IsLayerConfig LayerConfig t where
  layer (LayerConfig l) = layer l

instance HasOpacity (LayerConfig t) Opacity where
  opacity =
    lens (\(LayerConfig l) -> l^.opacity)
         (\(LayerConfig l) o -> LayerConfig (l & opacity.~o))

instance HasVisibility (LayerConfig t) Visibility where
  visible =
    lens (\(LayerConfig l) -> l^.visible)
         (\(LayerConfig l) o -> LayerConfig (l & visible.~o))
instance HasExtent (LayerConfig t) (Maybe Extent) where
  extent =
    lens (\(LayerConfig l) -> l^.extent)
         (\(LayerConfig l) o -> LayerConfig (l & extent.~o))
instance HasResolutionRange (LayerConfig t) (Maybe Double) where
  minResolution =
    lens (\(LayerConfig l) -> l^.minResolution)
         (\(LayerConfig l) o -> LayerConfig (l & minResolution.~o))
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
     , _imageConfig_opacity          :: Opacity
     , _imageConfig_visible          :: Visibility
     , _imageConfig_extent           :: Maybe Extent
     , _imageConfig_minResolution    :: Maybe Double
     , _imageConfig_maxResolution    :: Maybe Double
     , _imageConfig_setOpacity       :: Event t Opacity
     , _imageConfig_setVisible       :: Event t Bool
     , _imageConfig_setExtent        :: Event t (Maybe Extent)
     , _imageConfig_setMinResolution :: Event t (Maybe Double)
     , _imageConfig_setMaxResolution :: Event t (Maybe Double)
     }

makeLenses ''ImageConfig

image :: Reflex t => Dynamic t (SourceConfig t) -> LayerConfig t
image src = LayerConfig $ ImageConfig {
       _imageConfig_source           = src
     , _imageConfig_opacity          = 1
     , _imageConfig_visible          = Visible
     , _imageConfig_extent           = Nothing
     , _imageConfig_minResolution    = Nothing
     , _imageConfig_maxResolution    = Nothing
     , _imageConfig_setOpacity       = never
     , _imageConfig_setVisible       = never
     , _imageConfig_setExtent        = never
     , _imageConfig_setMinResolution = never
     , _imageConfig_setMaxResolution = never
     }

initSource imgVal cfg = do
  schedulePostBuild $ do
    src <- S.source =<< sample (current (cfg^.source))
    olSetter "setSource" imgVal src
  performEvent_ $
    fmap (S.source >=> olSetter "setSource" imgVal)
    (updated (cfg^.source))

instance IsLayerConfig ImageConfig t where
  layer cfg@ImageConfig{..} = do
    imgVal <- liftIO $ do
      imgOpts <- layerOptions cfg
      mkLayer "Image" imgOpts
    initSource imgVal cfg
    return (Layer (Image imgVal))

instance HasOpacity (ImageConfig t) Opacity where
  opacity = imageConfig_opacity
instance HasSource (ImageConfig t) (Dynamic t (SourceConfig t)) where
  source = imageConfig_source
instance HasVisibility (ImageConfig t) Visibility where
  visible = imageConfig_visible
instance HasExtent (ImageConfig t) (Maybe Extent) where
  extent = imageConfig_extent
instance HasResolutionRange (ImageConfig t) (Maybe Double) where
  minResolution = imageConfig_minResolution
  maxResolution = imageConfig_maxResolution



--
-- Tile
--

newtype Tile t = Tile JSVal deriving (Typeable, ToJSVal)
instance IsLayer Tile t

data TileConfig t
  = TileConfig {
       _tileConfig_source           :: Dynamic t (SourceConfig t)
     , _tileConfig_opacity          :: Opacity
     , _tileConfig_visible          :: Visibility
     , _tileConfig_extent           :: Maybe Extent
     , _tileConfig_minResolution    :: Maybe Double
     , _tileConfig_maxResolution    :: Maybe Double
     , _tileConfig_setOpacity       :: Event t Opacity
     , _tileConfig_setVisible       :: Event t Bool
     , _tileConfig_setExtent        :: Event t (Maybe Extent)
     , _tileConfig_setMinResolution :: Event t (Maybe Double)
     , _tileConfig_setMaxResolution :: Event t (Maybe Double)
     }
makeLenses ''TileConfig

makeLenses ''Tile

tile :: Reflex t => Dynamic t (SourceConfig t) -> LayerConfig t
tile src = LayerConfig $ TileConfig {
       _tileConfig_source           = src
     , _tileConfig_opacity          = 1
     , _tileConfig_visible          = Visible
     , _tileConfig_extent           = Nothing
     , _tileConfig_minResolution    = Nothing
     , _tileConfig_maxResolution    = Nothing
     , _tileConfig_setOpacity       = never
     , _tileConfig_setVisible       = never
     , _tileConfig_setExtent        = never
     , _tileConfig_setMinResolution = never
     , _tileConfig_setMaxResolution = never
     }

instance IsLayerConfig TileConfig t where
  layer cfg@TileConfig{..} = do
    imgVal <- liftIO $ do
      imgOpts <- layerOptions cfg
      mkLayer "Tile" imgOpts
    initSource imgVal cfg
    return (Layer (Tile imgVal))

instance HasOpacity (TileConfig t) Opacity where
  opacity = tileConfig_opacity
instance HasSource (TileConfig t) (Dynamic t (SourceConfig t)) where
  source = tileConfig_source
instance HasVisibility (TileConfig t) Visibility where
  visible = tileConfig_visible
instance HasExtent (TileConfig t) (Maybe Extent) where
  extent = tileConfig_extent
instance HasResolutionRange (TileConfig t) (Maybe Double) where
  minResolution = tileConfig_minResolution
  maxResolution = tileConfig_maxResolution

--
-- Group
--

newtype Group t = Group JSVal deriving (Typeable, ToJSVal)
instance IsLayer Group t

data GroupConfig t
  = GroupConfig {
       _groupConfig_layers           :: [LayerConfig t]
     , _groupConfig_opacity          :: Opacity
     , _groupConfig_visible          :: Visibility
     , _groupConfig_extent           :: Maybe Extent
     , _groupConfig_minResolution    :: Maybe Double
     , _groupConfig_maxResolution    :: Maybe Double
     , _groupConfig_setLayers        :: Event t [LayerConfig t]
     , _groupConfig_setOpacity       :: Event t Opacity
     , _groupConfig_setVisible       :: Event t Bool
     , _groupConfig_setExtent        :: Event t (Maybe Extent)
     , _groupConfig_setMinResolution :: Event t (Maybe Double)
     , _groupConfig_setMaxResolution :: Event t (Maybe Double)
     }
makeLenses ''GroupConfig

instance Reflex t => Default (GroupConfig t) where
  def = GroupConfig {
       _groupConfig_layers           = []
     , _groupConfig_opacity          = 1
     , _groupConfig_visible          = Visible
     , _groupConfig_extent           = Nothing
     , _groupConfig_minResolution    = Nothing
     , _groupConfig_maxResolution    = Nothing
     , _groupConfig_setLayers        = never
     , _groupConfig_setOpacity       = never
     , _groupConfig_setVisible       = never
     , _groupConfig_setExtent        = never
     , _groupConfig_setMinResolution = never
     , _groupConfig_setMaxResolution = never
     }

instance IsLayerConfig GroupConfig t where
  layer cfg  = do
    layers <- mapM (\(LayerConfig l) -> layer l) (cfg^.layers)
    jsVal <- liftIO $ do
      opts <- layerOptions cfg
      jsLayers <- toJSVal layers
      O.setProp "layers" jsLayers opts
      mkLayer "Group" opts
    return (Layer (Group jsVal))

instance HasOpacity (GroupConfig t) Opacity where
  opacity = groupConfig_opacity
instance HasVisibility (GroupConfig t) Visibility where
  visible = groupConfig_visible
instance HasExtent (GroupConfig t) (Maybe Extent) where
  extent = groupConfig_extent
instance HasResolutionRange (GroupConfig t) (Maybe Double) where
  minResolution = groupConfig_minResolution
  maxResolution = groupConfig_maxResolution

instance HasLayers (GroupConfig t) [LayerConfig t] where
  layers = groupConfig_layers
