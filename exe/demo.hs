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
    & OL.setRotation .~ fmapMaybe readMay (updated (value rotationInput))
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

  rotationInput <- dtdd "rotation" $ do
    let d = mapWidget^.OL.rotation
    cur <- sample (current d)
    input <- textInput $ def
      & textInputConfig_initialValue .~ show cur
      & setValue   .~ fmap show  (updated d)
      & attributes .~ constDyn ("type" =: "number")
    display d
    return input

  resolutionInput <- dtdd "resolution" $ do
    let dynResolution = mapWidget^.OL.resolution
    curResolution <- sample (current dynResolution)
    resolutionInput <- textInput $ def
      & textInputConfig_initialValue .~ show (fromMaybe 0 curResolution)
      & setValue .~ fmap (show . fromMaybe (-1))  (updated dynResolution)
      & attributes .~ constDyn ("type" =: "number")
    display dynResolution
    return resolutionInput

  eCenter <- dtdd "center" $ do
    let mover = do
          mResolution <- current (mapWidget^.OL.resolution)
          mCenter <- current (mapWidget^.OL.center)
          return $ do
            delta <- liftM (*10) mResolution
            (x,y) <- mCenter
            return $ \dir -> case dir of
                               North -> (x        , y + delta)
                               South -> (x        , y - delta)
                               East  -> (x + delta, y        )
                               West  -> (x - delta, y        )
    north <- liftM (fmap (const North)) (button "north")
    south <- liftM (fmap (const South)) (button "south")
    east <- liftM (fmap (const East)) (button "east")
    west <- liftM (fmap (const West)) (button "west")
    display (mapWidget^.OL.center)
    return $ attachWithMaybe (\mFun dir -> fmap ($dir) mFun) mover
              $ leftmost [north, south, east, west]

  (reverseButton,layerBox, opacityInput) <- dtdd "layers" $ do
    l <- checkbox True def
    o <- textInput $ def
      & textInputConfig_initialValue .~ "1"
      & attributes .~ constDyn ("type" =: "number")
    b <- button "reverse"
    return (b,l, o)
  return ()

data Direction = North | South | East | West
