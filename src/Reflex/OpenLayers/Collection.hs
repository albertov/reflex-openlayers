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
  , collectionWith2
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
    folder needle Nothing k v
      | snd v `jEq` needle = Just k
    folder _ acc _ _       = acc

jEq :: JSVal -> JSVal -> Bool
jEq a b = [jsu'|$r=(`a===`b);|]

collectionWith2
  :: forall k t m a. (Ord k, Enum k, MonadWidget t m)
  => (a -> IO JSVal)
  -> (JSVal -> IO (Maybe a))
  -> ((a, JSVal) -> m (Event t a))
  -> M.Map k a
  -> Event t (M.Map k (Maybe a))
  -> m (Collection t k a)
collectionWith2 toJS fromJS mkEvent initialItems eUpdate = mdo
  initialItems2 <- mapM (\o -> liftIO (toJS o >>= \j -> return (o,j)))
                        initialItems
  initialJS <- liftIO (toJSVal (map snd (M.elems initialItems2)))
  col <- liftIO [jsu|$r=new ol.Collection(`initialJS);|]
  (eS,suppress) <- mkSuppressor
  eAdd' <- fmap (gate eS) $
           wrapOLEvent "add" col (\(e::JSVal) -> [jsu|$r=`e.element|])
  eDel' <- fmap (gate eS) $
           wrapOLEvent "remove" col (\(e::JSVal) -> [jsu|$r=`e.element|])
  eAdd <- performEvent $ attachWith onAdd (current dynItems2) eAdd'
  eDel <- performEvent $ attachWith onDel (current dynItems2) eDel'
  eUpdate2 <- performEvent $
      attachWith (onUpdate suppress col) (current dynItems2) eUpdate
  let eChanges = mconcat [ eDel
                         , eAdd
                         , eUpdate2
                         ]
  dynItems2 <- liftM joinDynThroughMap $
                    listWithKey' initialItems2 eChanges mkChild
  dynItems <- mapDyn (M.map fst) dynItems2
  return (Collection col dynItems)
  where
    mkChild k v eUp = do
      ev <- liftM switchPromptlyDyn $
        widgetHold (mkEvent' v) (fmap mkEvent' eUp)
      holdDyn v $ leftmost [ev, eUp]
    mkEvent' i@(_,j) = liftM (fmap (\v -> (v,j))) (mkEvent i)
    onAdd, onDel
      :: M.Map k (a, JSVal)
      -> JSVal
      -> WidgetHost m (M.Map k (Maybe (a, JSVal)))
    onAdd cur v = liftIO $ do
      h <- fmap (fromMaybe (error "could not convert from JS")) (fromJS v)
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
                  ix <- prev
                  (_,item) <- M.lookup ix cur
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
