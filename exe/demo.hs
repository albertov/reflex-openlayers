{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
module Main (main) where

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Common
import Reflex.Dom.Contrib.Widgets.BoundedList (mkHiding)
import Reflex.OpenLayers

import Control.Monad
import Control.Lens ((^.), (^?), (^?!), over, views)
import qualified Data.Map as M
import Data.List (foldl')
import Safe (readMay)

main :: IO ()
main = mainWidgetWithCss olCss $ mdo
  remover <- mapDyn (mergeWith (.) . map fst . M.elems) layerList
  dynLayerMap <- foldDyn ($) initialLayers (switchPromptlyDyn remover)
  dynLayers <- mapDyn (M.map snd) layerList

  mapWidget <- olMap $ def
    & view      .~ dynView
    & layers    .~ dynLayers

  layerList <- el "ul" $ listWithKey dynLayerMap layerWidget


  dynView <- dtdd "view" $ mdo
    dynView <- foldDyn ($) initialView $ mergeWith(.) [
            fmap const (mapWidget^.viewChanged)
          , rotationChange
          , resolutionChange
          , centerChange
          ]
    rotationChange <- dtdd "rotation" $ do
      curView <- sample (current dynView)
      input <- htmlTextInput "number" $ def
        & widgetConfig_initialValue .~ show (curView^.rotation)
        & attributes .~ constDyn ("step" =: "0.05")
        & setValue   .~
            fmap (show . (^.rotation)) (mapWidget^.viewChanged)
      let eVal = fmapMaybe readMay (change input)
      return $ fmap (rotation .~) eVal

    resolutionChange <- dtdd "resolution" $ do
      curView <- sample (current dynView)
      input <- htmlTextInput "number" $ def
        & widgetConfig_initialValue .~ show (curView^.resolution)
        & attributes .~ constDyn ("step" =: "1000")
        & setValue   .~
            fmap (show . (^.resolution)) (mapWidget^.viewChanged)
      let eVal = fmapMaybe readMay (change input)
      return $ fmap (resolution .~) eVal

    centerChange <- dtdd "center" $ mdo
      let showCenter c = show (c^.x) ++ ", " ++ show (c^.y)
      dynText =<< mapDyn (views center showCenter) dynView
      el "br" blank
      let mover = do
            curView <- current dynView
            pixels <- current (value input)
            let d = (curView^.resolution) * (maybe 0 fromIntegral pixels)
            return $ \dir ->
              case dir of
                North -> over (center . y) (+d)
                South -> over (center . y) (subtract d)
                East  -> over (center . x) (+d)
                West  -> over (center . x) (subtract d)
      north <- liftM (fmap (const North)) (button "north")
      south <- liftM (fmap (const South)) (button "south")
      east <- liftM (fmap (const East)) (button "east")
      west <- liftM (fmap (const West)) (button "west")
      input <- intWidget $ def & widgetConfig_initialValue .~ Just 10
      return $ attachWith ($) mover $ leftmost [north, south, east, west]
    return dynView
  return ()

initialLayers = fromList
  [ tile $ mapQuest Satellite
  , tile $
      tileWMS
        "http://demo.boundlessgeo.com/geoserver/wms"
        ("LAYERS" =: "topp:states")
  ]

initialView = def & center .~ Coordinates (-10997148) 4569099

layerWidget
  :: MonadWidget t m
  => Int
  -> Dynamic t (Layer t)
  -> m (Event t (LayerSet (Layer t) -> LayerSet (Layer t)), Layer t)
layerWidget key layer = el "li" $ do
  curLayer <- sample (current layer)

  eVisible <- el "label" $ do
    input <- checkbox (curLayer^.visible.initialValue) $ def
      & setValue .~ curLayer^.visible.setValue
    text "Visible?"
    return $ visible.setValue .~ (_checkbox_change input)

  eOpacity <- el "label" $ do
    text "Opacity"
    input <- htmlTextInput "number" $ def
      & widgetConfig_initialValue .~ show (curLayer^.opacity.initialValue)
      & setValue .~ fmap show (curLayer^.opacity.setValue)
      & attributes .~ constDyn ("step" =: "0.05")
    let eChange = fmapMaybe readMay (change input)
    return $ opacity.setValue .~ eChange

  eZIndex <- el "label" $ do
    text "ZIndex"
    input <- htmlTextInput "number" $ def
      & widgetConfig_initialValue .~ show (curLayer^.zIndex.initialValue)
      & setValue .~ fmap show (curLayer^.zIndex.setValue)
    let eChange = fmapMaybe readMay (change input)
    return $ zIndex.setValue .~ eChange

{-
  changeSource <- case curLayer of
    Image{} -> do
      eSource <- sourceWidget =<< mapDyn (^?imageSource) layer
      return $ fmap (\v -> Just . (imageSource .~ v)) eSource
    Tile{} -> do
      eSource <- sourceWidget =<< mapDyn (^?tileSource) layer
      return $ fmap (\v -> Just . (tileSource .~ v)) eSource
    _ -> return never
-}
  eDelete <- button "delete"
  return ( fmap (const (M.delete key)) eDelete
         , foldl' (&) curLayer [
                eVisible
              , eOpacity
              , eZIndex
              ]
         )

sourceWidget
  :: MonadWidget t m
  => Dynamic t (Maybe (Source r k t))
  -> m (Event t (Source r k t))
sourceWidget val = do
  curVal <- sample (current val)
  case curVal of
    Just (s@ImageWMS{_imageWmsUrl}) -> do
      el "label" $ do
        text "URL"
        input <- htmlTextInput "url" $ def
          & widgetConfig_initialValue .~ _imageWmsUrl
        return $ fmap (\v -> s {_imageWmsUrl=v}) (blurOrEnter input)
    Just (s@TileWMS{_tileWmsUrl}) -> do
      el "label" $ do
        text "URL"
        input <- htmlTextInput "url" $ def
          & widgetConfig_initialValue .~ _tileWmsUrl
        return $ fmap (\v -> s {_tileWmsUrl=v}) (blurOrEnter input)
    Just (s@MapQuest{_mapQuestLayer}) -> do
      el "label" $ do
        text "Layer"
        input <- htmlDropdownStatic [minBound..maxBound] show id $ def
          & widgetConfig_initialValue .~ _mapQuestLayer
        return $ fmap (\v -> s {_mapQuestLayer=v}) (change input)
    _ -> return never

data Direction = North | South | East | West
