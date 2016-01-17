{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.OpenLayers.Collection (
    Collection
  , HasItems (..)
  , collection
  , collectionWith
) where

import Reflex.OpenLayers.Util
import Reflex.OpenLayers.Event

import Reflex
import Reflex.Dom

import qualified Data.Map as M
import Data.These
import Data.Align
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHCJS.Marshal (ToJSVal(toJSVal), FromJSVal(fromJSVal))
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Types
import GHCJS.Foreign.QQ

data Collection t k a
  = Collection {
      _collectionJsVal :: JSVal
    , _collectionItems :: Dynamic t (M.Map k a)
    }
makeFields ''Collection

instance PToJSVal (Collection t k a) where
  pToJSVal = _collectionJsVal

instance ToJSVal (Collection t k a) where
  toJSVal = return . pToJSVal

collection
  :: (Ord k, Enum k, ToJSVal a, FromJSVal a, MonadWidget t m)
  => Dynamic t (M.Map k a) -> m (Collection t k a)
collection = collectionWith toJSVal fromJSVal

collectionWith
  :: (Ord k, Enum k, MonadWidget t m)
  => (a -> IO JSVal) -> (JSVal -> IO (Maybe a)) -> Dynamic t (M.Map k a)
  -> m (Collection t k a)
collectionWith toJS fromJS inMap = mdo
  dynItems <- mapDynIO (mapM (\o -> toJS o >>= \jo -> return (Just o,jo)))
              inMap
  initial <- liftIO . toJSVal
              =<< fmap (map snd . M.elems) (sample (current dynItems))
  c <- liftIO [jsu|$r=new ol.Collection(`initial);|]
  eAdd    <- wrapOLEvent "add" c (\(e::JSVal) -> [jsu|$r=`e.element|])
  eRemove <- wrapOLEvent "remove" c (\(e::JSVal) -> [jsu|$r=`e.element|])
  curItems <- sample (current dynItems)
  jsOut <- mapDynIO (mapM fromJS') =<< (foldDyn ($) curItems $ mergeWith (.) [
      attachWith (\m n -> case M.foldlWithKey (folder n) Nothing m of
                            Just k -> M.delete k
                            Nothing -> id
                 ) (current jsOut) eRemove
    , fmap (\j -> pushToMap (Nothing,j)) eAdd
    ])
  dynItemsOut <- mapDyn (M.map (fromMaybe fromJSErr . fst)) jsOut
  let eOldNew = attach (current dynItems) (updated dynItems)
  performEvent_ $ ffor eOldNew  $ \(curVals, newVals) -> liftIO $ do
    forM_ (align curVals newVals) $ \case
      This (_,old)                  -> [js_|`c.remove(`old);|]
      That (_,new)                  -> [js_|`c.push(`new);|]
      These _ _                     -> return ()
  return $ Collection c dynItemsOut
  where
    fromJS' i@(Just _, _) = return i
    fromJS' (Nothing,j)   = do
      mH <- fromJS j
      return (mH, j)
    fromJSErr = error "Collection: could not convert from JS"
    jEq :: JSVal -> JSVal -> Bool
    jEq a b = [jsu'|$r=(`a===`b);|]
    folder needle Nothing k v
      | snd v `jEq` needle = Just k
    folder _ acc _ _       = acc
