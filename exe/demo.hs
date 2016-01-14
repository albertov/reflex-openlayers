{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Main (main) where

import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Common
import Reflex.OpenLayers
import Reflex.OpenLayers.Widgets

import Control.Monad
import Control.Lens ((^.), (^?), (^?!), (%~))
import qualified Data.Map as M
import Data.Monoid ((<>))
import Safe (readMay)

main :: IO ()
main = mainWidgetWithCss olCss $ mdo
  mapWidget <- olMap $ def
    & center.initialValue .~ Coordinates (-10997148) 4569099
    & center.setValue     .~ eCenter
    & resolution.setValue .~ eResolution
    & rotation.setValue   .~ eRotation
    & layers              .~ dynLayers
    & attributes          .~ constDyn ("style"=:"height:300px")

  dynLayers' <- layerListWidget (constDyn initialLayers)
  dynLayers <- layerListWidget dynLayers'

  (eCenter, eResolution, eRotation) <- dtdd "view" $ do
    eRotation <- dtdd "rotation" $ do
      curValue <- sample (current (mapWidget^.rotation))
      input <- htmlTextInput "number" $ def
        & widgetConfig_initialValue .~ show curValue
        & attributes .~ constDyn ("step" =: "0.05" <> "novalidate" =: "true")
        & setValue   .~ fmap show (updated (mapWidget^.rotation))
      return $ fmapMaybe readMay (change input)

    eResolution <- dtdd "resolution" $ do
      curValue <- sample (current (mapWidget^.resolution))
      input <- htmlTextInput "number" $ def
        & widgetConfig_initialValue .~ show curValue
        & attributes .~ constDyn ("step" =: "1000" <> "novalidate" =: "true")
        & setValue   .~ fmap show (updated (mapWidget^.resolution))
      return $ fmapMaybe readMay (change input)

    eCenter <- dtdd "center" $ mdo
      let showCenter c = show (c^.x) ++ ", " ++ show (c^.y)
      dynText =<< mapDyn showCenter (mapWidget^.center)
      el "br" blank
      let applyMove = do
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
      return $ attachWith ($) applyMove $ leftmost [north, south, east, west]
    return (eCenter, eResolution, eRotation)
  return ()

initialLayers :: Reflex t => LayerSet (Layer t Property)
initialLayers = fromList
  [
    tile $
      tileWMS
        "http://demo.boundlessgeo.com/geoserver/wms"
        ("LAYERS" =: "topp:states")
  , group $ constDyn $ fromList [
        tile $ tileWMS
          "http://demo.boundlessgeo.com/geoserver/ne/wms"
          (   "LAYERS" =: "ne:ne_10m_admin_0_countries"
           <> "TILED" =: "true")
      , tile osm
      ]
  , tile $ mapQuest Satellite

  ]



data Direction = North | South | East | West
