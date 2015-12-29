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
    & OL.mapView .~ (def
      & OL.zoom   .~ 4
      & OL.setZoom .~ fmapMaybe readMay (updated (value zoomInput))
      & OL.center .~ (-10997148, 4569099)
      )
    & OL.layerGroup .~ (def
      & OL.layers .~ [
          OL.tile dynSrc
        , (OL.image $ constDyn $
            OL.imageWMS "http://demo.boundlessgeo.com/geoserver/wms"
            ("LAYERS" =: "topp:states"))
          & OL.opacity .~ 0.5
        ]
      )

  dynSrc <- holdDyn (OL.mapQuest Satellite) never
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
  return ()
