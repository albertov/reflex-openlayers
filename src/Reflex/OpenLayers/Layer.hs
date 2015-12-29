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

module Reflex.OpenLayers.Layer (
    IsLayer (..)
  , Layer

  , IsLayerConfig (..)
  , LayerConfig

  , Extent (..)
  , Opacity

  , ImageConfig (..)
  , Image (..)
  , imageConfig

  , TileConfig (..)
  , Tile (..)
  , tileConfig

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

import Reflex.Dom (
    Reflex
  , MonadWidget
  , Dynamic
  , Event
  , never
  , holdDyn
  )

import qualified JavaScript.Object as O
import Data.Default (Default(def))
import Data.Typeable (Typeable, cast)
import Control.Lens
import Control.Monad (liftM, when)
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
class HasOpacity l o | l->o where
  opacity :: Lens' (l t) o

class HasSource l s | l->s where
  source :: Lens' (l t) (s t)

class HasVisibility l v | l->v  where
  visible :: Lens' (l t) v

class HasExtent l e | l->e where
  extent :: Lens' (l t) e

class HasResolutionRange l r | l->r where
  minResolution :: Lens' (l t) r
  maxResolution :: Lens' (l t) r

class HasLayers o a | o->a where
  layers :: Lens' o a

class ( HasVisibility l Visibility
      , HasOpacity l Opacity
      , HasExtent l (Maybe Extent)
      , HasResolutionRange l (Maybe Double)
      ) => IsLayerConfig l where
  layer :: MonadWidget t m => l t -> m (Layer t)

layerOptions :: IsLayerConfig l => l t -> IO O.Object
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
  forall l. IsLayerConfig l => LayerConfig (l t)

instance IsLayerConfig LayerConfig where
  layer (LayerConfig l) = layer l

instance HasOpacity LayerConfig Opacity where
  opacity =
    lens (\(LayerConfig l) -> l^.opacity)
         (\(LayerConfig l) o -> LayerConfig (l & opacity.~o))
instance HasVisibility LayerConfig Visibility where
  visible =
    lens (\(LayerConfig l) -> l^.visible)
         (\(LayerConfig l) o -> LayerConfig (l & visible.~o))
instance HasExtent LayerConfig (Maybe Extent) where
  extent =
    lens (\(LayerConfig l) -> l^.extent)
         (\(LayerConfig l) o -> LayerConfig (l & extent.~o))
instance HasResolutionRange LayerConfig (Maybe Double) where
  minResolution =
    lens (\(LayerConfig l) -> l^.minResolution)
         (\(LayerConfig l) o -> LayerConfig (l & minResolution.~o))
  maxResolution =
    lens (\(LayerConfig l) -> l^.maxResolution)
         (\(LayerConfig l) o -> LayerConfig (l & maxResolution.~o))

class ( Typeable l
      ) => IsLayer l where
  layer_downcast :: Typeable t => Layer t -> Maybe (l t)
  layer_downcast (Layer l) = cast l
  {-# INLINE layer_downcast #-}

  layer_visible       :: Reflex t => l t -> Dynamic t Visibility
  layer_extent        :: Reflex t => l t -> Dynamic t (Maybe Extent)
  layer_minResolution :: Reflex t => l t -> Dynamic t (Maybe Double)
  layer_maxResolution :: Reflex t => l t -> Dynamic t (Maybe Double)

data Layer t =  forall l. (ToJSVal (l t), IsLayer l) => Layer (l t)

instance IsLayer Layer where
  layer_downcast = Just


instance ToJSVal (Layer t) where
  toJSVal (Layer l) = toJSVal l

--
-- Image
--

data Image t
  = Image {
      _image_jsval         :: JSVal
    , _image_source        :: Dynamic t (Source t)
    , _image_opacity       :: Dynamic t Opacity
    , _image_visible       :: Dynamic t Visibility
    , _image_extent        :: Dynamic t (Maybe Extent)
    , _image_minResolution :: Dynamic t (Maybe Double)
    , _image_maxResolution :: Dynamic t (Maybe Double)
    } deriving (Typeable)


instance ToJSVal (Image t) where
  toJSVal = return . _image_jsval

instance IsLayer Image where
  layer_visible       = _image_visible
  layer_extent        = _image_extent
  layer_minResolution = _image_minResolution
  layer_maxResolution = _image_maxResolution

data ImageConfig t
  = ImageConfig {
       _imageConfig_source           :: SourceConfig t
     , _imageConfig_opacity          :: Opacity
     , _imageConfig_visible          :: Visibility
     , _imageConfig_extent           :: Maybe Extent
     , _imageConfig_minResolution    :: Maybe Double
     , _imageConfig_maxResolution    :: Maybe Double
     , _imageConfig_setSource        :: Event t (SourceConfig t)
     , _imageConfig_setOpacity       :: Event t Opacity
     , _imageConfig_setVisible       :: Event t Bool
     , _imageConfig_setExtent        :: Event t (Maybe Extent)
     , _imageConfig_setMinResolution :: Event t (Maybe Double)
     , _imageConfig_setMaxResolution :: Event t (Maybe Double)
     }

makeLenses ''Image
makeLenses ''ImageConfig

imageConfig :: Reflex t => SourceConfig t -> LayerConfig t
imageConfig src = LayerConfig $ ImageConfig {
       _imageConfig_source           = src
     , _imageConfig_opacity          = 1
     , _imageConfig_visible          = Visible
     , _imageConfig_extent           = Nothing
     , _imageConfig_minResolution    = Nothing
     , _imageConfig_maxResolution    = Nothing
     , _imageConfig_setSource        = never
     , _imageConfig_setOpacity       = never
     , _imageConfig_setVisible       = never
     , _imageConfig_setExtent        = never
     , _imageConfig_setMinResolution = never
     , _imageConfig_setMaxResolution = never
     }

instance IsLayerConfig ImageConfig where
  layer cfg@ImageConfig{..} = do
    src <- S.source (cfg^.source)
    imgVal <- liftIO $ do
      imgOpts <- layerOptions cfg
      jsSrc <- toJSVal src
      O.setProp "source" jsSrc imgOpts
      mkLayer "Image" imgOpts
    dynSrc <- holdDyn src never
    dynOpacity <- holdDyn _imageConfig_opacity never
    dynVisible <- holdDyn _imageConfig_visible never
    dynExtent <- holdDyn _imageConfig_extent never
    dynMinResolution <- holdDyn _imageConfig_minResolution never
    dynMaxResolution <- holdDyn _imageConfig_maxResolution never
    return (Layer (Image {
               _image_jsval         = imgVal
             , _image_source        = dynSrc
             , _image_opacity       = dynOpacity
             , _image_visible       = dynVisible
             , _image_extent        = dynExtent
             , _image_minResolution = dynMinResolution
             , _image_maxResolution = dynMaxResolution
           }))

instance HasOpacity ImageConfig Opacity where
  opacity = imageConfig_opacity
instance HasSource ImageConfig SourceConfig where
  source = imageConfig_source
instance HasVisibility ImageConfig Visibility where
  visible = imageConfig_visible
instance HasExtent ImageConfig (Maybe Extent) where
  extent = imageConfig_extent
instance HasResolutionRange ImageConfig (Maybe Double) where
  minResolution = imageConfig_minResolution
  maxResolution = imageConfig_maxResolution

--
-- Tile
--

data Tile t
  = Tile {
      _tile_jsval         :: JSVal
    , _tile_source        :: Dynamic t (Source t)
    , _tile_opacity       :: Dynamic t Opacity
    , _tile_visible       :: Dynamic t Visibility
    , _tile_extent        :: Dynamic t (Maybe Extent)
    , _tile_minResolution :: Dynamic t (Maybe Double)
    , _tile_maxResolution :: Dynamic t (Maybe Double)
    } deriving (Typeable)


instance ToJSVal (Tile t) where
  toJSVal = return . _tile_jsval

instance IsLayer Tile where
  layer_visible       = _tile_visible
  layer_extent        = _tile_extent
  layer_minResolution = _tile_minResolution
  layer_maxResolution = _tile_maxResolution

data TileConfig t
  = TileConfig {
       _tileConfig_source           :: SourceConfig t
     , _tileConfig_opacity          :: Opacity
     , _tileConfig_visible          :: Visibility
     , _tileConfig_extent           :: Maybe Extent
     , _tileConfig_minResolution    :: Maybe Double
     , _tileConfig_maxResolution    :: Maybe Double
     , _tileConfig_setSource        :: Event t (SourceConfig t)
     , _tileConfig_setOpacity       :: Event t Opacity
     , _tileConfig_setVisible       :: Event t Bool
     , _tileConfig_setExtent        :: Event t (Maybe Extent)
     , _tileConfig_setMinResolution :: Event t (Maybe Double)
     , _tileConfig_setMaxResolution :: Event t (Maybe Double)
     }

makeLenses ''Tile
makeLenses ''TileConfig

tileConfig :: Reflex t => SourceConfig t -> LayerConfig t
tileConfig src = LayerConfig $ TileConfig {
       _tileConfig_source           = src
     , _tileConfig_opacity          = 1
     , _tileConfig_visible          = Visible
     , _tileConfig_extent           = Nothing
     , _tileConfig_minResolution    = Nothing
     , _tileConfig_maxResolution    = Nothing
     , _tileConfig_setSource        = never
     , _tileConfig_setOpacity       = never
     , _tileConfig_setVisible       = never
     , _tileConfig_setExtent        = never
     , _tileConfig_setMinResolution = never
     , _tileConfig_setMaxResolution = never
     }

instance IsLayerConfig TileConfig where
  layer cfg@TileConfig{..} = do
    src <- S.source (cfg^.source)
    imgVal <- liftIO $ do
      imgOpts <- layerOptions cfg
      jsSrc <- toJSVal src
      O.setProp "source" jsSrc imgOpts
      mkLayer "Tile" imgOpts
    dynSrc <- holdDyn src never
    dynOpacity <- holdDyn _tileConfig_opacity never
    dynVisible <- holdDyn _tileConfig_visible never
    dynExtent <- holdDyn _tileConfig_extent never
    dynMinResolution <- holdDyn _tileConfig_minResolution never
    dynMaxResolution <- holdDyn _tileConfig_maxResolution never
    return (Layer (Tile {
               _tile_jsval         = imgVal
             , _tile_source        = dynSrc
             , _tile_opacity       = dynOpacity
             , _tile_visible       = dynVisible
             , _tile_extent        = dynExtent
             , _tile_minResolution = dynMinResolution
             , _tile_maxResolution = dynMaxResolution
           }))

instance HasOpacity TileConfig Opacity where
  opacity = tileConfig_opacity
instance HasSource TileConfig SourceConfig where
  source = tileConfig_source
instance HasVisibility TileConfig Visibility where
  visible = tileConfig_visible
instance HasExtent TileConfig (Maybe Extent) where
  extent = tileConfig_extent
instance HasResolutionRange TileConfig (Maybe Double) where
  minResolution = tileConfig_minResolution
  maxResolution = tileConfig_maxResolution

--
-- Group
--

data Group t
  = Group {
      _group_jsval         :: JSVal
    , _group_layers        :: Dynamic t [Layer t]
    , _group_opacity       :: Dynamic t Opacity
    , _group_visible       :: Dynamic t Visibility
    , _group_extent        :: Dynamic t (Maybe Extent)
    , _group_minResolution :: Dynamic t (Maybe Double)
    , _group_maxResolution :: Dynamic t (Maybe Double)
    } deriving (Typeable)
makeLenses ''Group


instance ToJSVal (Group t) where
  toJSVal = return . _group_jsval

instance IsLayer Group where
  layer_visible       = _group_visible
  layer_extent        = _group_extent
  layer_minResolution = _group_minResolution
  layer_maxResolution = _group_maxResolution

instance HasLayers (Group t) (Dynamic t [Layer t]) where
  layers = group_layers

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

instance IsLayerConfig GroupConfig where
  layer cfg@GroupConfig{..} = do
    layers <- mapM (\(LayerConfig l) -> layer l) _groupConfig_layers
    jsVal <- liftIO $ do
      opts <- layerOptions cfg
      jsLayers <- toJSVal layers
      O.setProp "layers" jsLayers opts
      mkLayer "Group" opts
    dynLayers <- holdDyn layers never
    dynOpacity <- holdDyn _groupConfig_opacity never
    dynVisible <- holdDyn _groupConfig_visible never
    dynExtent <- holdDyn _groupConfig_extent never
    dynMinResolution <- holdDyn _groupConfig_minResolution never
    dynMaxResolution <- holdDyn _groupConfig_maxResolution never
    return (Layer (Group {
               _group_jsval         = jsVal
             , _group_layers        = dynLayers
             , _group_opacity       = dynOpacity
             , _group_visible       = dynVisible
             , _group_extent        = dynExtent
             , _group_minResolution = dynMinResolution
             , _group_maxResolution = dynMaxResolution
           }))

instance HasOpacity GroupConfig Opacity where
  opacity = groupConfig_opacity
instance HasVisibility GroupConfig Visibility where
  visible = groupConfig_visible
instance HasExtent GroupConfig (Maybe Extent) where
  extent = groupConfig_extent
instance HasResolutionRange GroupConfig (Maybe Double) where
  minResolution = groupConfig_minResolution
  maxResolution = groupConfig_maxResolution

instance HasLayers (GroupConfig t) [LayerConfig t] where
  layers = groupConfig_layers
