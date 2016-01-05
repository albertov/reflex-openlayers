{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Reflex.OpenLayers.Source (
    Source (..)
  , MapQuestLayer (..)
  , imageWMS
  , mapQuest
  , mkSource
) where

import Reflex
import Reflex.Dom
import Reflex.OpenLayers.Util (dynInitialize)

import Data.Typeable (Typeable, cast)
import qualified Data.Map as M
import Control.Monad (liftM, forM_)
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Types (JSVal, JSString, jsval)
import GHCJS.DOM.Types
import GHCJS.Foreign.QQ


data MapQuestLayer = OpenStreetMap | Satellite | Hybrid deriving (Show, Read)

instance PToJSVal MapQuestLayer where
  pToJSVal OpenStreetMap = jsval ("osm" :: JSString)
  pToJSVal Satellite     = jsval ("sat" :: JSString)
  pToJSVal Hybrid        = jsval ("hyb" :: JSString)

data Source t
  = ImageWMS {
      _url    :: Dynamic t String
    , _params :: Dynamic t (M.Map String String)
    }
  | MapQuest {
      _layer  :: MapQuestLayer
    }
makeLenses ''Source

imageWMS
  :: Reflex t
  => Dynamic t String -> Dynamic t (M.Map String String) -> Source t
imageWMS = ImageWMS

mapQuest :: Reflex t => MapQuestLayer -> Source t
mapQuest = MapQuest

mkSource :: MonadWidget t m => Source t -> m JSVal
mkSource s =
  case s of
    ImageWMS{_url, _params} -> do
      r <- liftIO [jsu|$r=new ol.source.ImageWMS({params:{}});|]
      dynInitialize _url $ \newUrl -> liftIO $ [jsu_|`r.setUrl(`newUrl);|]
      dynInitialize _params $ \ps -> liftIO $ [jsu_|`r.updateParams(`ps);|]
      return r
    MapQuest{_layer} ->
      liftIO [jsu|$r=new ol.source.MapQuest({layer:`_layer});|]
