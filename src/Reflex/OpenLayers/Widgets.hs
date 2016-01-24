{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Reflex.OpenLayers.Widgets (
    layerWidget
  , layerListWidget
) where

import Reflex.OpenLayers.Projection
import Reflex.OpenLayers.Util
import Reflex.OpenLayers

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Common

import Control.Monad
import Control.Lens ((^.), (^?), (^?!), (%~))
import qualified Data.Map as M
import Data.List (foldl')
import Data.Monoid ((<>))
import Safe (readMay)

layerListWidget
  :: MonadWidget t m
  => Dynamic t (M.Map Int (Layer t Property))
  -> m (Dynamic t (M.Map Int (Layer t Property)))
layerListWidget layerMap = mdo
  initialLayers <- sample (current layerMap)
  remover <- mapDyn (mergeWith (.) . map fst . M.elems) layerList
  dynLayerMap <- foldDyn ($) initialLayers $
    leftmost [ switchPromptlyDyn remover
             , fmap (\n -> const n) (updated layerMap)
             ]
  layerList <- el "ul" $ listWithKey dynLayerMap layerWidget
  mapDyn (M.map snd) layerList


layerWidget
  :: MonadWidget t m
  => Int
  -> Dynamic t (Layer t Property)
  -> m ( Event t (LayerSet (Layer t Property) -> LayerSet (Layer t Property))
       , Layer t Property)
layerWidget key layer = el "li" $ do
  curLayer <- sample (current layer)

  eVisible <- el "label" $ do
    input <- checkbox (curLayer^.visible.initialValue) $ def
      & setValue .~ curLayer^.visible.setValue
    text "Visible?"
    return $ visible.setValue %~ leftmost . (:[_checkbox_change input])

  eOpacity <- el "label" $ do
    text "Opacity"
    input <- htmlTextInput "number" $ def
      & widgetConfig_initialValue .~ show (curLayer^.opacity.initialValue)
      & setValue .~ fmap show (curLayer^.opacity.setValue)
      & attributes .~ constDyn ("step" =: "0.05")
    let eChange = fmapMaybe readMay (change input)
    return $ opacity.setValue %~ leftmost . (:[eChange])

  eZIndex <- el "label" $ do
    text "ZIndex"
    input <- htmlTextInput "number" $ def
      & widgetConfig_initialValue .~ show (curLayer^.zIndex.initialValue)
      & setValue .~ fmap show (curLayer^.zIndex.setValue)
    let eChange = fmapMaybe readMay (change input)
    return $ zIndex.setValue %~ leftmost . (:[eChange])

  eSource <- case curLayer of
    Image{} -> do
      eChange <- sourceWidget (curLayer^?!imageSource)
      return $ imageSource.setValue %~ leftmost . (:[eChange])
    Tile{} -> do
      eChange <- sourceWidget (curLayer^?!tileSource)
      return $ tileSource.setValue %~ leftmost . (:[eChange])
    _ -> return id

  eGroupLayers <- case curLayer of
    Group{} -> do
      dynLayers <- layerListWidget (curLayer^?!layers)
      return $ layers .~ dynLayers
    _ -> return id

  eDelete <- button "delete"

  return ( fmap (const (M.delete key)) eDelete
         , foldl' (&) curLayer [
                eVisible
              , eOpacity
              , eZIndex
              , eSource
              , eGroupLayers
              ]
         )

sourceWidget
  :: MonadWidget t m
  => Property t (WithSomeCrs (Source r k t))
  -> m (Event t (WithSomeCrs (Source r k t)))
sourceWidget = propertyWidget $ \case
  WithSomeCrs (s@ImageWMS{_imageWmsUrl}) ->
    el "label" $ do
      text "URL"
      input <- htmlTextInput "url" $ def
        & widgetConfig_initialValue .~ _imageWmsUrl
        & attributes .~ (constDyn ("size" =: "60"))
      return $
        fmap (\v -> s {_imageWmsUrl=v} `asCrsOf` s) (blurOrEnter input)
  WithSomeCrs (s@TileWMS{_tileWmsUrl}) ->
    el "label" $ do
      text "URL"
      input <- htmlTextInput "url" $ def
        & widgetConfig_initialValue .~ _tileWmsUrl
        & attributes .~ (constDyn ("size" =: "60"))
      return $ fmap (\v -> s {_tileWmsUrl=v} `asCrsOf` s) (blurOrEnter input)
  WithSomeCrs (s@MapQuest{_mapQuestLayer}) ->
    el "label" $ do
      text "Layer"
      input <- htmlDropdownStatic [minBound..maxBound] show id $ def
        & widgetConfig_initialValue .~ _mapQuestLayer
      return $ fmap (\v -> s {_mapQuestLayer=v} `asCrsOf` s) (change input)
  _ -> return never

asCrsOf :: KnownCrs crs => a crs -> a crs -> WithSomeCrs a
asCrsOf a _ = WithSomeCrs a
