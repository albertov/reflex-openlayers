{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
module Main (main) where

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Common
import Reflex.Dom.Contrib.Widgets.BoundedList (mkHiding)
import Reflex.OpenLayers

import Control.Monad
import Control.Lens ((^.), (^?), (^?!), (%~))
import qualified Data.Map as M
import Data.List (foldl')
import Safe (readMay)

main :: IO ()
main = mainWidgetWithCss olCss $ mdo
  remover <- mapDyn (mergeWith (.) . map fst . M.elems) layerList
  dynLayerMap <- foldDyn ($) initialLayers (switchPromptlyDyn remover)
  dynLayers <- mapDyn (M.map snd) layerList

  mapWidget <- olMap $ def
    & center.initialValue .~ Coordinates (-10997148) 4569099
    & center.setValue     .~ eCenter
    & resolution.setValue .~ eResolution
    & rotation.setValue   .~ eRotation
    & layers              .~ dynLayers

  layerList <- el "ul" $ listWithKey dynLayerMap layerWidget


  (eCenter, eResolution, eRotation) <- dtdd "view" $ do
    eRotation <- dtdd "rotation" $ do
      curValue <- sample (current (mapWidget^.rotation))
      input <- htmlTextInput "number" $ def
        & widgetConfig_initialValue .~ show curValue
        & attributes .~ constDyn ("step" =: "0.05")
        & setValue   .~ fmap show (updated (mapWidget^.rotation))
      return $ fmapMaybe readMay (change input)

    eResolution <- dtdd "resolution" $ do
      curValue <- sample (current (mapWidget^.resolution))
      input <- htmlTextInput "number" $ def
        & widgetConfig_initialValue .~ show curValue
        & attributes .~ constDyn ("step" =: "1000")
        & setValue   .~ fmap show (updated (mapWidget^.resolution))
      return $ fmapMaybe readMay (change input)

    eCenter <- dtdd "center" $ mdo
      let showCenter c = show (c^.x) ++ ", " ++ show (c^.y)
      dynText =<< mapDyn showCenter (mapWidget^.center)
      el "br" blank
      let mover = do
            pixels <- current (value pixelInput)
            curResolution <- current (mapWidget^.resolution)
            curCenter <- current (mapWidget^.center)
            let delta = curResolution * (maybe 0 fromIntegral pixels)
            return $ \dir ->
              case dir of
                North -> curCenter & y %~ (+ delta)
                South -> curCenter & y %~ (subtract delta)
                East  -> curCenter & x %~ (+ delta)
                West  -> curCenter & x %~ (subtract delta)
      north <- liftM (fmap (const North)) (button "north")
      south <- liftM (fmap (const South)) (button "south")
      east <- liftM (fmap (const East)) (button "east")
      west <- liftM (fmap (const West)) (button "west")
      pixelInput <- intWidget $ def & widgetConfig_initialValue .~ Just 10
      return $ attachWith ($) mover $ leftmost [north, south, east, west]
    return (eCenter, eResolution, eRotation)
  return ()

initialLayers = fromList
  [ tile $ constDyn $ mapQuest Satellite
  , tile $ constDyn $
      tileWMS
        "http://demo.boundlessgeo.com/geoserver/wms"
        ("LAYERS" =: "topp:states")
  ]


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

  eSource <- case curLayer of
    Image{} -> do
      curSource <- sample (current (curLayer^?!imageSource))
      eSource <- sourceWidget curSource
      dynSource <- holdDyn curSource eSource
      return $ imageSource .~ dynSource
    Tile{} -> do
      curSource <- sample (current (curLayer^?!tileSource))
      eSource <- sourceWidget curSource
      dynSource <- holdDyn curSource eSource
      return $ tileSource .~ dynSource
    _ -> return id
  eDelete <- button "delete"
  return ( fmap (const (M.delete key)) eDelete
         , foldl' (&) curLayer [
                eVisible
              , eOpacity
              , eZIndex
              , eSource
              ]
         )

sourceWidget
  :: MonadWidget t m
  => Source r k t
  -> m (Event t (Source r k t))
sourceWidget curVal =
  case curVal of
    s@ImageWMS{_imageWmsUrl} -> do
      el "label" $ do
        text "URL"
        input <- htmlTextInput "url" $ def
          & widgetConfig_initialValue .~ _imageWmsUrl
        return $ fmap (\v -> s {_imageWmsUrl=v}) (blurOrEnter input)
    s@TileWMS{_tileWmsUrl} -> do
      el "label" $ do
        text "URL"
        input <- htmlTextInput "url" $ def
          & widgetConfig_initialValue .~ _tileWmsUrl
        return $ fmap (\v -> s {_tileWmsUrl=v}) (blurOrEnter input)
    s@MapQuest{_mapQuestLayer} -> do
      el "label" $ do
        text "Layer"
        input <- htmlDropdownStatic [minBound..maxBound] show id $ def
          & widgetConfig_initialValue .~ _mapQuestLayer
        return $ fmap (\v -> s {_mapQuestLayer=v}) (change input)
    _ -> return never

data Direction = North | South | East | West
