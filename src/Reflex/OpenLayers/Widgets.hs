{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Reflex.OpenLayers.Widgets (
    LayerWidget
  , layerWidget
  , layerListWidget
) where

import Reflex.OpenLayers.Projection
import Reflex.OpenLayers.Util
import Reflex.OpenLayers

import Reflex.Dom
import Reflex.Dom.Widget.Input

import Control.Monad
import Control.Lens ((^.), (^?), (^?!), (%~))
import qualified Data.Map as M
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (Text)
import Data.Readable (fromText)
import Safe (readMay)

layerListWidget
  :: MonadWidget t m
  => Dynamic t (M.Map Int (Layer t Property))
  -> LayerWidget t m
  -> m (Dynamic t (M.Map Int (Layer t Property)))
layerListWidget layerMap widget = mdo
  initialLayers <- sample (current layerMap)
  remover <- mapDyn (mergeWith (.) . map fst . M.elems) layerList
  dynLayerMap <- foldDyn ($) initialLayers $
    leftmost [ switchPromptlyDyn remover
             , fmap (\n -> const n) (updated layerMap)
             ]
  layerList <- listWithKey dynLayerMap widget
  mapDyn (M.map snd) layerList

type LayerWidget t m
  =  Int
  -> Dynamic t (Layer t Property)
  -> m ( Event t (LayerSet (Layer t Property) -> LayerSet (Layer t Property))
       , Layer t Property)

layerWidget :: MonadWidget t m => LayerWidget t m
layerWidget key layer = do
  curLayer <- sample (current layer)

  _ <- dyn =<< mapDyn (maybe (return ())
                      (elClass "h4" "list-group-item-heading" . text))
           =<< dynFromProp (curLayer^.title)

  elClass "div" "list-group-item-text" $ do
    eVisible <- el "label" $ do
      input <- checkbox (curLayer^.visible.initialValue) $ def
        & setValue .~ curLayer^.visible.setValue
      text "Visible?"
      return $ visible.setValue %~ leftmost . (:[input^.checkbox_change])

    eOpacity <- el "label" $ do
      text "Opacity"
      input <- textInput $ def
        & textInputConfig_inputType .~ "number"
        & textInputConfig_initialValue .~ tShow (curLayer^.opacity.initialValue)
        & setValue .~ fmap tShow (curLayer^.opacity.setValue)
        & attributes .~ constDyn ("step" =: "0.05")
      let eChange = fmapMaybe fromText (input^.textInput_input)
      return $ opacity.setValue %~ leftmost . (:[eChange])

    eZIndex <- el "label" $ do
      text "ZIndex"
      input <- textInput $ def
        & textInputConfig_inputType .~ "number"
        & textInputConfig_initialValue .~ tShow (curLayer^.zIndex.initialValue)
        & setValue .~ fmap tShow (curLayer^.zIndex.setValue)
      let eChange = fmapMaybe fromText (input^.textInput_input)
      return $ zIndex.setValue %~ leftmost . (:[eChange])

    eSource <- case curLayer of
      Image{} -> do
        eChange <- sourceWidget (curLayer^?!imageSource)
        return $ imageSource.setValue %~ leftmost . (:[eChange])
      Tile{} -> do
        eChange <- sourceWidget (curLayer^?!tileSource)
        return $ tileSource.setValue %~ leftmost . (:[eChange])
      _ -> return id

    eGroupLayers <- case curLayer of
      Group{} -> do
        dynLayers <- layerListWidget (curLayer^?!layers) layerWidget
        return $ layers .~ dynLayers
      _ -> return id

    eDelete <- button "delete"

    return ( fmap (const (M.delete key)) eDelete
           , foldl' (&) curLayer [
                  eVisible
                , eOpacity
                , eZIndex
                , eSource
                , eGroupLayers
                ]
           )

sourceWidget
  :: MonadWidget t m
  => Property t (WithSomeCrs (Source r k t))
  -> m (Event t (WithSomeCrs (Source r k t)))
sourceWidget = propertyWidget $ \case
  WithSomeCrs (s@ImageWMS{_imageWmsUrl}) ->
    el "label" $ do
      text "URL"
      input <- textInput $ def
        & textInputConfig_inputType .~ "url"
        & textInputConfig_initialValue .~ _imageWmsUrl
        & attributes .~ (constDyn ("size" =: "60"))
      return $
        fmap (\v -> s {_imageWmsUrl=v} `asCrsOf` s) (input^.textInput_input)
  WithSomeCrs (s@TileWMS{_tileWmsUrl}) ->
    el "label" $ do
      text "URL"
      input <- textInput $ def
        & textInputConfig_inputType .~ "url"
        & textInputConfig_initialValue .~ _tileWmsUrl
        & attributes .~ (constDyn ("size" =: "60"))
      return $ fmap (\v -> s {_tileWmsUrl=v} `asCrsOf` s) (input^.textInput_input)
  WithSomeCrs (s@MapQuest{_mapQuestLayer}) ->
    el "label" $ do
      text "Layer"
      let opts = constDyn (M.fromList (map (\i -> (i, tShow i)) [minBound..maxBound]))
      input <- dropdown _mapQuestLayer opts def
      return $ fmap (\v -> s {_mapQuestLayer=v} `asCrsOf` s) (input^.dropdown_change)
  _ -> return never

asCrsOf :: KnownCrs crs => a crs -> a crs -> WithSomeCrs a
asCrsOf a _ = WithSomeCrs a

tShow :: Show a => a -> Text
tShow = fromString . show
