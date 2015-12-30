{-# LANGUAGE RecursiveDo #-}
module Main (main) where

import Control.Arrow
import Control.Monad
import Control.Lens ((^.))
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Reflex.Dom
import qualified Reflex.OpenLayers as OL
import Reflex.OpenLayers.Source as OL
import Reflex.OpenLayers.Layer as OL
import Safe (readMay)

main :: IO ()
main = mainWidgetWithCss OL.css $ mdo
  mapWidget <- OL.map $ def
    & OL.resolution    .~ 100000
    & OL.setResolution .~ fmapMaybe readMay (updated (value resolutionInput))
    & OL.center  .~ (-10997148, 4569099)
    & OL.setCenter  .~ eCenter
    & OL.layers  .~ dynLayers
  dynLayers <- holdDyn initialLayers $
                 tag (fmap reverse (current dynLayers)) reverseButton
  initialLayers <- mapM layer [
        OL.tile dynSrc
      , (OL.image $ constDyn $
          OL.imageWMS "http://demo.boundlessgeo.com/geoserver/wms"
          ("LAYERS" =: "topp:states"))
        & OL.opacity .~ dynOpacity
        & OL.visible .~ value layerBox
      ]
  dynSrc <- holdDyn (OL.mapQuest Satellite) never
  dynOpacity <- mapDyn (fromMaybe 0 . readMay) (value opacityInput)
  let eCenter  = push performMove eMove
      delta = 1000
      curCenter = sample (current (mapWidget^.OL.center))
      performMove North = liftM (fmap (second (+delta))) curCenter
      performMove South = liftM (fmap (second (subtract delta))) curCenter
      performMove East = liftM (fmap (first (+delta))) curCenter
      performMove West = liftM (fmap (first (subtract delta))) curCenter
  resolutionInput <- dtdd "resolution" $ do
    let dynResolution = mapWidget^.OL.resolution
    curResolution <- sample (current dynResolution)
    resolutionInput <- textInput $ def
      & textInputConfig_initialValue .~ show (fromMaybe 0 curResolution)
      & setValue   .~ fmap (show . fromMaybe (-1))  (updated dynResolution)
      & attributes .~ constDyn ("type" =: "number")
    display dynResolution
    return resolutionInput
  eMove <- dtdd "center" $ do
    display (mapWidget^.OL.center)
    goNorth <- liftM (fmap (const North)) (button "north")
    return goNorth
  (reverseButton,layerBox, opacityInput) <- dtdd "layers" $ do
    l <- checkbox True def
    o <- textInput $ def
      & textInputConfig_initialValue .~ "1"
      & attributes .~ constDyn ("type" =: "number")
    b <- button "reverse"
    return (b,l, o)
  return ()

data Direction = North | South | East | West
