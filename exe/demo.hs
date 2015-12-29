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
main = mainWidgetWithCss OL.css $ do
  rec m <- OL.map $ def
        & OL.mapView .~ (def
          & OL.zoom   .~ 4
          & OL.setZoom .~ fmapMaybe readMay (updated (value zoomInput))
          & OL.center .~ (-10997148, 4569099)
          )
        & OL.layerGroup .~ (def
          & OL.layers .~ [
              OL.tileConfig $
                OL.mapQuest Satellite
            , OL.imageConfig $
                OL.imageWMS "http://demo.boundlessgeo.com/geoserver/wms"
                ("LAYERS" =: "topp:states")
            ]
          )
      zoomInput <- dtdd "zoom" $ do
        let dynZoom = m ^. (OL.mapView . OL.zoom)
        curZoom <- sample (current dynZoom)
        zoomInput <- textInput $ def
          & textInputConfig_initialValue .~ show (fromMaybe 0 curZoom)
          & setValue   .~ fmap (show . fromJust)  (updated dynZoom)
          & attributes .~ constDyn ("type" =: "number")
        display dynZoom
        return zoomInput
  dtdd "center" $ display (m ^. (OL.mapView . OL.center))
  return ()
