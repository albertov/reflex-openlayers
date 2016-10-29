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
import Data.Monoid (mconcat)
import Data.These
import Data.Align
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHCJS.Marshal (ToJSVal(toJSVal), FromJSVal(fromJSVal))
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal))
import GHCJS.Types
import GHCJS.Foreign.QQ

data Collection t k a
  = Collection {
      collectionJsVal :: JSVal
    , _collectionItems :: Dynamic t (M.Map k a)
    }
makeFields ''Collection

instance PToJSVal (Collection t k a) where
  pToJSVal = collectionJsVal

instance ToJSVal (Collection t k a) where
  toJSVal = return . pToJSVal

collection
  :: (Ord k, Enum k, ToJSVal a, FromJSVal a, MonadWidget t m)
  => Dynamic t (M.Map k a) -> m (Collection t k a)
collection inMap = do
  let eOldNew = attach (current inMap) (updated inMap)
      eUpdate = ffor eOldNew  $ \(curVals, newVals) ->
        ffor (align curVals newVals) $  \case
          This _      -> Nothing
          That new    -> Just new
          These _ new -> Just new
  initial <- sample (current inMap)
  collectionWith toJSVal fromJSVal (const (return never)) initial eUpdate

jEq :: JSVal -> JSVal -> Bool
jEq a b = [jsu'|$r=(`a===`b);|]

collectionWith
  :: forall k t m a. (Ord k, Enum k, MonadWidget t m)
  => (a -> IO JSVal)
  -> (JSVal -> IO (Maybe a))
  -> ((a, JSVal) -> m (Event t a))
  -> M.Map k a
  -> Event t (M.Map k (Maybe a))
  -> m (Collection t k a)
collectionWith toJS fromJS' mkEvent initialItems eUpdate = mdo
  initialItems2 <- mapM (\o -> liftIO (toJS o >>= \j -> return (o,j)))
                        initialItems
  initialJS <- liftIO (toJSVal (map snd (M.elems initialItems2)))
  col <- liftIO [jsu|$r=new ol.Collection(`initialJS);|]
  (eS,suppress) <- mkSuppressor
  eAdd' <- fmap (gate eS) $
           wrapOLEvent "add" col (\(e::JSVal) -> [jsu|$r=`e.element|])
  eDel' <- fmap (gate eS) $
           wrapOLEvent "remove" col (\(e::JSVal) -> [jsu|$r=`e.element|])
  eAdd <- performEvent $ attachWith onAdd (current dynItems) eAdd'
  eDel <- performEvent $ attachWith onDel (current dynItems) eDel'
  eUpdate2 <- performEvent $
      attachWith (onUpdate suppress col) (current dynItems) eUpdate
  let eChanges = mconcat [ eDel
                         , eAdd
                         , eUpdate2
                         ]
  dynItems <- joinDynThroughMap
          <$> listWithKeyShallowDiff initialItems2 eChanges mkChild
  return (Collection col (fmap (fmap fst) dynItems))
  where
    mkChild _ v eUp = do
      ev <- liftM switchPromptlyDyn $
        widgetHold (mkEvent' v) (fmap mkEvent' eUp)
      holdDyn v $ leftmost [ev, eUp]
    mkEvent' i@(_,j) = liftM (fmap (\v -> (v,j))) (mkEvent i)
    onAdd, onDel
      :: M.Map k (a, JSVal)
      -> JSVal
      -> WidgetHost m (M.Map k (Maybe (a, JSVal)))
    onAdd cur v = liftIO $ do
      h <- fmap (fromMaybe (error "could not convert from JS")) (fromJS' v)
      let k = maybe (toEnum 0) (\((k',_),_) -> succ k') (M.maxViewWithKey cur)
      return (M.singleton k (Just (h,v)))
    onDel cur del = return (M.foldlWithKey folder M.empty cur)
      where
        folder m k v
          | snd v `jEq` del = M.insert k Nothing m
          | otherwise       = m
    onUpdate
      :: (IO (M.Map k (Maybe (a, JSVal))) -> IO (M.Map k (Maybe (a, JSVal))))
      -> JSVal
      -> M.Map k (a, JSVal)
      -> M.Map k (Maybe a)
      -> WidgetHost m (M.Map k (Maybe (a, JSVal)))
    onUpdate suppress col cur updates = liftIO $ suppress $ liftM fst $
      foldM folder (M.empty, Nothing) (M.toList updates)
      where
        folder (ret,prev) (k,v) = case (M.lookup k cur, v) of
          (Just (_,old), Nothing) -> do
            [js_|`col.remove(`old);|]
            return (M.insert k Nothing ret, prev)
          (Just (_,old), Just newH) -> do --FIXME
            new <- toJS newH
            [js_|var arr = `col.getArray();
                 for (var i=0, l=arr.length; i<l; i++) {
                   if (arr[i]===`old) {
                      `col.setAt(i,`new);
                      break;
                   }
                 }|]
            return (M.insert k (Just (newH,new)) ret, Just k)
          (Nothing, Just newH) -> do
            let prevItem = do
                  i <- prev
                  (_,item) <- M.lookup i cur
                  return item
            new <- toJS newH
            case prevItem of
              Nothing -> [js_|`col.insertAt(0, `new);|]
              Just item ->
                [js_|var arr = `col.getArray();
                     for (var i=0, l=arr.length; i<l; i++) {
                       if (arr[i]===`item) {
                         `col.insertAt(i+1, `new);
                         break;
                       }
                     }|]
            return (M.insert k (Just (newH,new)) ret, Just k)
          _  -> return (ret, Just k)
