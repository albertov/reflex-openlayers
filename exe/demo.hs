{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Reflex.Dom
import Reflex.OpenLayers
import Reflex.OpenLayers.Widgets

import Data.FileEmbed (embedFile)

import Control.Monad
import Control.Lens ((^.), (^?), (^?!), (%~))
import qualified Data.Map as M
import Data.Monoid ((<>))
import Data.Readable (fromText)
import Data.String (fromString)
import Data.Text (Text)


main :: IO ()
main = mainWidgetWithCss $(embedFile "static/ol.css") $ mdo
  mapWidget <- olMap $ def
    & center.initialValue .~ mkCenter crs84 (-3.690032958984375)
                                             40.41801452636719
    & center.setValue     .~ eCenter
    & resolution.initialValue .~ 0.001
    & resolution.setValue .~ eResolution
    & rotation.setValue   .~ eRotation
    & layers              .~ dynLayers
    & attributes          .~ constDyn ("style"=:"height:300px")

  dynLayers <- layerListWidget (constDyn initialLayers) layerWidget
  --dynLayers <- layerListWidget dynLayers'

  (eCenter, eResolution, eRotation) <- dtdd "view" $ do
    eRotation <- dtdd "rotation" $ do
      curValue <- sample (current (mapWidget^.rotation))
      input <- textInput  $ def
        & textInputConfig_inputType .~ "number"
        & textInputConfig_initialValue .~ tShow curValue
        & attributes .~ constDyn ("step" =: "0.05" <> "novalidate" =: "true")
        & setValue   .~ fmap tShow (updated (mapWidget^.rotation))
      return $ fmapMaybe fromText (input^.textInput_input)

    eResolution <- dtdd "resolution" $ do
      curValue <- sample (current (mapWidget^.resolution))
      input <- textInput $ def
        & textInputConfig_inputType .~ "number"
        & textInputConfig_initialValue .~ tShow curValue
        & attributes .~ constDyn ("step" =: "1000" <> "novalidate" =: "true")
        & setValue   .~ fmap tShow (updated (mapWidget^.resolution))
      return $ fmapMaybe fromText (input^.textInput_input)

    eCenter <- dtdd "center" $ mdo
      let showCenter c = tShow (c^.x) <> ", " <> tShow (c^.y)
      dynText =<< mapDyn showCenter (mapWidget^.center)
      el "br" blank
      let applyMove = do
            pixels <- current (fmap fromText (pixelInput^.textInput_value))
            curResolution <- current (mapWidget^.resolution)
            curCenter <- current (mapWidget^.center)
            let delta = curResolution * (maybe 0 id pixels)
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
      pixelInput <- textInput $ def
        & textInputConfig_inputType .~ "number"
        & textInputConfig_initialValue .~  "10"
      return $ attachWith ($) applyMove $ leftmost [north, south, east, west]
    return (eCenter, eResolution, eRotation)
  return ()

tShow :: Show a => a -> Text
tShow = fromString . show

initialLayers :: Reflex t => LayerSet (Layer t Property)
initialLayers = fromList
  [
    tile $
      tileWMS
        "http://demo.boundlessgeo.com/geoserver/wms"
        ("LAYERS" =: "topp:states")
  , group $ constDyn $ fromList [
      tile $
        tileWMS' crs84
        "http://demo.boundlessgeo.com/geoserver/ne/wms"
          (   "LAYERS" =: "ne:ne_10m_admin_0_countries"
           <> "TILED" =: "true")
    , tile osm
    ]
  , tile $ mapQuest Satellite

  ]



data Direction = North | South | East | West
