{-# LANGUAGE RecursiveDo #-}
module Main (main) where

import Control.Arrow
import Control.Monad
import Control.Lens ((^.))
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Common
import qualified Reflex.OpenLayers as OL
import Reflex.OpenLayers.Source as OL
import Reflex.OpenLayers.Layer as OL
import Safe (readMay)

main :: IO ()
main = mainWidgetWithCss OL.css $ mdo
  dynSrc <- holdDyn (OL.mapQuest (Satellite)) never
  dynOpacity <- mapDyn (fromMaybe 0 . readMay) (value opacityInput)
  let initialLayers = [
          OL.tile dynSrc
        , (OL.image $ constDyn $
            OL.imageWMS
              (constDyn "http://demo.boundlessgeo.com/geoserver/wms")
              (constDyn ("LAYERS" =: "topp:states")))
          & OL.opacity .~ dynOpacity
          & OL.visible .~ value layerBox
        ]
  dynLayers <- holdDyn initialLayers $
                 tag (fmap reverse (current dynLayers)) reverseButton
  let initialView = def
        & OL.zoom   .~ OL.This 4
        & OL.center .~ (-10997148, 4569099)
  mapWidget <- OL.map $ def
    & OL.view  .~ dynView
    -- & OL.setView  .~ updated dynView
    & OL.layers  .~ dynLayers

  (reverseButton,layerBox, opacityInput) <- dtdd "layers" $ do
    l <- checkbox True def
    o <- textInput $ def
      & textInputConfig_initialValue .~ "1"
      & attributes .~ constDyn ("type" =: "number")
    b <- button "reverse"
    return (b,l, o)

  dynView <- foldDyn ($) initialView $ mergeWith(.) [
          fmap const (mapWidget^.OL.viewChanged)
        , rotationChange
        , zoomChange
        ]
  dtdd "view" $ dynText =<< mapDyn show dynView



  rotationChange <- dtdd "rotation" $ do
    curView <- sample (current dynView)
    input <- htmlTextInput "number" $ def
      & widgetConfig_initialValue .~ show (curView^.OL.rotation)
      & setValue   .~ fmap (show . (^.OL.rotation))  (mapWidget^.OL.viewChanged)
    let eVal = fmapMaybe readMay (updated (value input))
    return $ fmap (\v -> OL.rotation .~ v) eVal

  zoomChange <- dtdd "zoom" $ do
    curView <- sample (current dynView)
    input <- textInput $ def
      & textInputConfig_initialValue .~ show (curView^.OL.zoom)
      & setValue   .~ fmap (show . (^.OL.zoom))  (mapWidget^.OL.viewChanged)
      & attributes .~ constDyn ("type" =: "number")
    let eVal = fmapMaybe readMay (updated (value input))
    return $ fmap (\v -> OL.zoom .~ v) eVal

  {-

  eCenter <- dtdd "center" $ do
    let mover = do
          mResolution <- current (mapWidget^.OL.zoom)
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
 -}
  return ()

data Direction = North | South | East | West
