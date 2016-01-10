{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
module Main (main) where

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Common
import Reflex.Dom.Contrib.Widgets.BoundedList (mkHiding)
import Reflex.OpenLayers

import Data.Dependent.Sum (DSum (..))
import qualified Data.Dependent.Map as DMap

import Control.Monad
import Control.Lens ((^.), (^?), (^?!), over, views)
import qualified Data.Map as M
import Data.List (foldl')
import Safe (readMay)

data Item

{-
main :: IO ()
main = mainWidget $ el "ul" $ mdo
  items <- fmap M.fromList $ forM (M.toList initialConfigs) $ \(i,c) -> do
    set <- switchPromptly never (fmap (change . (M.! i)) $ updated litems)
    w <- htmlTextInput "text" $ c
      & widgetConfig_setValue .~ set
    return (i,w)

  litems <- list (constDyn items) itemWidget
  return ()
  where
    initialConfigs = M.fromList [
          (1, def $ widgetConfig_initialValue .~ "foo")
        , (2, def $ widgetConfig_initialValue .~ "bar")
        ]

itemWidget :: MonadWidget t m => Dynamic t (HtmlWidget t String) -> m (HtmlWidget t String)
itemWidget s = do
  curItem <- sample (current s)
  curValue <- sample (current (value curItem))
  htmlTextInput "text" $ def
    & widgetConfig_initialValue .~ curValue
    & widgetConfig_setValue     .~ change curItem
-}


main :: IO ()
main = mainWidgetWithCss olCss $ mdo
  iLayers <- fmap M.fromList $ forM (M.toList initialLayers) $ \(i,l) -> do
    setOpacity <- switchPromptly (l^.opacity.setValue) $
      fmap (flip select Opacity . snd) $
      fmapMaybe (M.lookup i) (updated layerList)
    jsL <- mkLayer (l & opacity.setValue .~ setOpacity)
    return (i, jsL)

  remover <- mapDyn ( mergeWith (.)
                    . map (\(k,(e,_)) -> fmap (const (M.delete k)) e)
                    . M.toList
                    ) layerList
  dynLayerMap <- foldDyn ($) iLayers (switchPromptlyDyn remover)

  mapWidget <- olMap $ def
    & view      .~ dynView
    & layers    .~ dynLayerMap

  layerList <- el "ul" $ list dynLayerMap layerWidget


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
  => Dynamic t (JSLayer t)
  -> m (Event t (), EventSelector t LayerProperty)
layerWidget layer = el "li" $ do
  curLayer <- sample (current layer)

  eVisible <- el "label" $ do
    let dynValue = curLayer^.visible
    curValue <- sample (current dynValue)
    input <- checkbox curValue $ def
      & setValue .~ updated dynValue
    text "Visible?"
    return $ fmap (DMap.singleton Visible) $ _checkbox_change input

  eOpacity <- el "label" $ do
    text "Opacity"
    let dynValue = curLayer^.opacity
    curValue <- sample (current dynValue)
    input <- htmlTextInput "number" $ def
      & widgetConfig_initialValue .~ show curValue
      & setValue .~ fmap show (updated dynValue)
      & attributes .~ constDyn ("step" =: "0.05")
    return $ fmap (DMap.singleton Opacity) $ fmapMaybe readMay (change input)

  eZIndex <- el "label" $ do
    text "ZIndex"
    let dynValue = curLayer^.zIndex
    curValue <- sample (current dynValue)
    input <- htmlTextInput "number" $ def
      & widgetConfig_initialValue .~ show curValue
      & setValue .~ fmap show (updated dynValue)
    return $ fmap (DMap.singleton ZIndex) $ fmapMaybe readMay (change input)

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
  return (eDelete, fan (mergeWith DMap.union [eVisible, eOpacity, eZIndex]))

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
