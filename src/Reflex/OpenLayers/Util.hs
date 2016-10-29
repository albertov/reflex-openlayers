{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Reflex.OpenLayers.Util (
    HasInitialValue (..)
  , Property (..)
  , constProperty
  , propertyWidget
  , initProperty
  , initPropertyWith
  , dynFromProp
  , pushToMap
  , mkSuppressor
  , map_toJSVal
  , map_fromJSVal
  ) where

import Reflex.OpenLayers.Event

import Reflex
import Reflex.Dom

import Control.Monad
import Control.Lens
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Map as M
import Data.Text (Text)
import Data.Default (Default(..))
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Marshal (fromJSVal)
import GHCJS.Foreign.QQ
import GHCJS.Types
import GHCJS.DOM.Types (toJSString)
import qualified JavaScript.Object as O
import System.IO.Unsafe (unsafePerformIO)


map_toJSVal :: PToJSVal a => M.Map Text a -> JSVal
map_toJSVal m = unsafePerformIO $ do
  o <- O.create
  forM_ (M.toList m) $ \(k,v) -> do
    O.setProp (toJSString k) (pToJSVal v) o
  return (jsval o)

map_fromJSVal :: (Show a, PFromJSVal a) => JSVal -> M.Map Text a
map_fromJSVal =
    M.fromList
  . maybe (error "should not happen") toAssocs
  . unsafePerformIO
  . fromJSVal
  . keyValues

  where
    toAssocs [] = []
    toAssocs (_:[]) = error "pFromJSVal: should not happen;"
    toAssocs (k:v:r) = (pFromJSVal k, pFromJSVal v):toAssocs r
    keyValues :: JSVal -> JSVal
    keyValues o = [jsu'|
      $r=[];
      for (var k in `o) {$r.push(k); $r.push(`o[k]);}
      |]

data Property t a
  = Property {
      _propertyInitialValue :: a
    , _propertySetValue     :: Event t a
    }
makeLenses ''Property


class HasInitialValue o a | o->a where
  initialValue :: Lens' o a

instance Reflex t => HasInitialValue (Property t a) a where
  initialValue = propertyInitialValue

instance Reflex t => HasSetValue (Property t a) where
  type SetValue (Property t a) = Event t a
  setValue = propertySetValue

constProperty :: Reflex t => a -> Property t a
constProperty = flip Property never

instance (Reflex t, Default a) => Default (Property t a) where
  def = Property def never

dynFromProp :: MonadHold t m => Property t a -> m (Dynamic t a)
dynFromProp (Property v e) = holdDyn v e

propertyWidget
  :: MonadWidget t m => (a -> m (Event t a)) -> Property t a -> m (Event t a)
propertyWidget go (Property v e) =
  switchPromptlyDyn <$> widgetHold (go v) (fmap go e)


initProperty
  :: (MonadWidget t m, PToJSVal o, PToJSVal a, PFromJSVal a)
  =>  String -> o -> Property t a -> m (Dynamic t a)
initProperty = initPropertyWith (Just (return . pFromJSVal)) return

initPropertyWith
  :: forall t m o a b. (MonadWidget t m, PToJSVal o, PToJSVal b)
  => Maybe (JSVal -> m a) -> (a -> m b) -> String -> o -> Property t a
  -> m (Dynamic t a)
initPropertyWith mUnBuild build name ob p = do
  (changeWidget, update :: b -> Performable m ()) <- case mUnBuild of
    Nothing -> return (fmap return (p^.setValue), setIfNotNull name ob)
    Just unBuild -> do
      (emit,suppress) <- mkSuppressor
      eChangeJs <- wrapOLEvent_ ("change:"++name) ob [jsu|$r=`ob.get(`name);|]
      let eChange = fmap unBuild (gate emit eChangeJs)
      return (leftmost [fmap return (p^.setValue), eChange], suppress . setIfNotNull name ob)
  postBuild <- fmap (const (p^.initialValue)) <$> getPostBuild
  dynUChange <- widgetHold (build (p^.initialValue))
                           (fmap build (leftmost [postBuild, p^.setValue])) 
  performEvent_ $ fmap update (updated dynUChange)
  widgetHold (return (p^.initialValue)) changeWidget 

setIfNotNull :: (MonadIO m, PToJSVal a, PToJSVal b) => String -> a -> b -> m ()
setIfNotNull name ob v = liftIO [jsu_|if(`v!==null) {`ob.set(`name, `v)};|]


mkSuppressor
  :: (MonadIO m', MonadWidget t m)
  => m (Behavior t Bool, m' a -> m' a)
mkSuppressor = do
  (eEmit, setEmit) <- newTriggerEvent
  emit <- hold True eEmit
  return (emit, \x -> liftIO (setEmit False) >> x >>= \a -> liftIO (setEmit True) >> return a)

pushToMap :: (Enum k, Ord k) => a -> M.Map k a -> M.Map k a
pushToMap v m = case M.maxViewWithKey m of
  Nothing          -> M.singleton (toEnum 0) v
  Just ((k, _), _) -> M.insert (succ k) v m
