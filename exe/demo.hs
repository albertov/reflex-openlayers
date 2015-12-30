{-# LANGUAGE RecursiveDo #-}
module Main (main) where

import Control.Lens ((^.))
import Data.Default (def)
import Data.Maybe (fromMaybe, fromJust)
import Reflex.Dom
import qualified Reflex.OpenLayers as OL
import Reflex.OpenLayers.Source as OL
import Reflex.OpenLayers.Layer as OL
import Safe (readMay)

main :: IO ()
main = mainWidgetWithCss OL.css $ mdo
  mapWidget <- OL.map $ def
    & OL.zoom    .~ 4
    & OL.setZoom .~ fmapMaybe readMay (updated (value zoomInput))
    & OL.center  .~ (-10997148, 4569099)
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

  zoomInput <- dtdd "zoom" $ do
    let dynZoom = mapWidget ^. (OL.mapView . OL.zoom)
    curZoom <- sample (current dynZoom)
    zoomInput <- textInput $ def
      & textInputConfig_initialValue .~ show (fromMaybe 0 curZoom)
      & setValue   .~ fmap (show . fromJust)  (updated dynZoom)
      & attributes .~ constDyn ("type" =: "number")
    display dynZoom
    return zoomInput
  dtdd "center" $ display (mapWidget ^. (OL.mapView . OL.center))
  (reverseButton,layerBox, opacityInput) <- dtdd "layers" $ do
    l <- checkbox True def
    o <- textInput $ def
      & textInputConfig_initialValue .~ "1"
      & attributes .~ constDyn ("type" =: "number")
    b <- button "reverse"
    return (b,l, o)
  return ()
