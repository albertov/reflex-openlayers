module Main (main) where

import Data.Default (def)
import Reflex.Dom
import qualified Reflex.OpenLayers as OL
import Reflex.OpenLayers.Source
import Reflex.OpenLayers.Layer

main :: IO ()
main = mainWidgetWithCss OL.css $ do
  m <- OL.map def {
      OL._mapConfig_view = def {
          OL._viewConfig_zoom = 4
        , OL._viewConfig_center = (-10997148, 4569099)
        }
    , OL._mapConfig_layerGroup = def {
        _groupConfig_layers = [
            tileConfig $
              mapQuest Satellite
          , imageConfig $
              imageWMS "http://demo.boundlessgeo.com/geoserver/wms"
              ("LAYERS" =: "topp:states")
          ]
        }
    }
  return ()
