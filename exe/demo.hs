{-# LANGUAGE RecursiveDo #-}
module Main (main) where

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Common
import Reflex.OpenLayers

import Control.Monad
import Control.Lens ((^.), over, views)
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
    events <- el "ul" $ do
      listWithKey dynLayerMap layerWidget
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
  , image $
      imageWMS
        "http://demo.boundlessgeo.com/geoserver/wms"
        ("LAYERS" =: "topp:states")
  ]

initialView = def & center .~ Coordinates (-10997148) 4569099

layerWidget
  :: MonadWidget t m
  => Int
  -> Dynamic t (Layer t)
  -> m (Event t (Layer t -> Maybe (Layer t)))
layerWidget ix layer = el "li" $ do

  dynVisible <- mapDyn (^.visible) layer
  eVisible <- checkboxView (constDyn mempty) dynVisible

  dynOpacity <- mapDyn (^.opacity) layer
  curOpacity <- sample (current dynOpacity)
  opacityInput <- htmlTextInput "number" $ def
    & widgetConfig_initialValue .~ show curOpacity
    & attributes .~ constDyn ("step" =: "0.05")
  let eOpacity = fmapMaybe readMay (change opacityInput)

  eDelete <- button "delete"

  return $ mergeWith (>=>) [
      fmap (\v l -> Just (l & visible .~ v)) eVisible
    , fmap (\v l -> Just (l & opacity .~ v)) eOpacity
    , fmap (\_ _ -> Nothing) eDelete
    ]

data Direction = North | South | East | West
