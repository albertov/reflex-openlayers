{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module Reflex.OpenLayers.Collection (
    Collection
  , HasItems (..)
  , mkCollection
) where

import Reflex.OpenLayers.Util

import Reflex
import Reflex.Dom

import qualified Data.Map as M
import Data.These
import Data.Align
import Control.Lens
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHCJS.Marshal (ToJSVal(toJSVal), FromJSVal(fromJSVal))
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Types
import GHCJS.Foreign.QQ

data Collection t
  = Collection {
      _collectionJsVal :: JSVal
    , _collectionItems :: Dynamic t JSVal
    }
makeFields ''Collection

instance PToJSVal (Collection t) where
  pToJSVal = _collectionJsVal

instance ToJSVal (Collection t) where
  toJSVal = return . pToJSVal

mkCollection
  :: MonadWidget t m
  => Dynamic t (M.Map Int JSVal) -> m (Collection t)
mkCollection dynItems = do
  c <- liftIO [jsu|$r=new ol.Collection();|]
  let eOldNew = attach (current dynItems) (updated dynItems)
  performEvent_ $ ffor eOldNew  $ \(curItems, newItems) -> liftIO $ do
    forM_ (align curItems newItems) $ \case
      This old                      -> [js_|`c.remove(`old);|]
      That new                      -> [js_|`c.push(`new);|]
      These _ _                     -> return ()
  return $ Collection c undefined
