{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
module Main (main) where

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Common
import Reflex.Dom.Contrib.Widgets.BoundedList (mkHiding)
import Reflex.OpenLayers

import Control.Monad
import Control.Lens ((^.), (^?), over, views)
import qualified Data.Map as M
import Data.List (foldl')
import Safe (readMay)

main :: IO ()
main = mainWidgetWithCss olCss $ mdo
  mapWidget <- olMap $ def
    & view  .~ dynView
    & layers  .~ dynLayers

  dynLayers <- dtdd "layers" $ mdo
    dynLayerMap <- foldDyn ($) initialLayers $ switch $ current itemChangeEvent
    events <- el "ul" $ list dynLayerMap layerWidget
    let combineItemChanges
          = fmap ( foldl' (.) id)
          . mergeList
          . map (\(k, v) -> fmap (flip M.update k) v)
          . M.toList
    itemChangeEvent <- mapDyn combineItemChanges events
    return dynLayerMap

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
  , image $ raster (\p -> p & red .~ 0) (anySource osm)
  ]

initialView = def & center .~ Coordinates (-10997148) 4569099

layerWidget
  :: MonadWidget t m
  => Dynamic t (Layer t)
  -> m (Event t (Layer t -> Maybe (Layer t)))
layerWidget layer = el "li" $ do

  dynVisible <- mapDyn (^.visible) layer
  eVisible <- el "label" $ do
    e <- checkboxView (constDyn mempty) dynVisible
    text "Visible?"
    return e

  dynOpacity <- mapDyn (^.opacity) layer
  curOpacity <- sample (current dynOpacity)
  opacityInput <- el "label" $ do
    text "Opacity"
    htmlTextInput "number" $ def
      & widgetConfig_initialValue .~ show curOpacity
      & attributes .~ constDyn ("step" =: "0.05")
  let eOpacity = fmapMaybe readMay (change opacityInput)

  dynZIndex <- mapDyn (^.zIndex) layer
  curZIndex <- sample (current dynZIndex)
  zIndexInput <- el "label" $ do
    text "zIndex"
    htmlTextInput "number" $ def
      & widgetConfig_initialValue .~ show curZIndex
  let eZindex = fmapMaybe readMay (change zIndexInput)

  curLayer <- sample (current layer)
  changeSource <- case curLayer of
    Image{} -> do
      eSource <- sourceWidget =<< mapDyn (^?imageSource) layer
      return $ fmap (\v -> Just . (imageSource .~ v)) eSource
    Tile{} -> do
      eSource <- sourceWidget =<< mapDyn (^?tileSource) layer
      return $ fmap (\v -> Just . (tileSource .~ v)) eSource
    _ -> return never


  eDelete <- button "delete"

  return $ mergeWith (>=>) [
      fmap (\v -> Just . (visible .~ v)) eVisible
    , fmap (\v -> Just . (opacity .~ v)) eOpacity
    , fmap (\v -> Just . (zIndex  .~ v)) eZindex
    , changeSource
    , fmap (\_ _ -> Nothing) eDelete
    ]

sourceWidget
  :: MonadWidget t m
  => Dynamic t (Maybe (Source r k))
  -> m (Event t (Source r k))
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
