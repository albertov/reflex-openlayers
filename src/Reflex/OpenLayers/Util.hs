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
  , pushToMap
  , mapDynIO
  , mkSuppressor
  , getLiftWidget
  ) where

import Reflex.OpenLayers.Event

import Reflex
import Reflex.Host.Class
import Reflex.Dom
import Data.Dependent.Sum (DSum (..))

import Control.Monad (forM_, when, void, liftM, (>=>), (<=<))
import Control.Monad.Ref (MonadRef(readRef), Ref)
import Control.Exception (finally)
import Control.Lens
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Map as M
import Data.Default (Default(..))
import GHCJS.Marshal.Pure (PToJSVal(pToJSVal), PFromJSVal(pFromJSVal))
import GHCJS.Marshal (fromJSVal)
import GHCJS.Foreign.QQ
import GHCJS.Types
import GHCJS.DOM.Types (toJSString)
import qualified JavaScript.Object as O
import System.IO.Unsafe (unsafePerformIO)

import GHCJS.DOM.Document (createDocumentFragment)


instance PToJSVal a => PToJSVal (M.Map String a) where
  pToJSVal m = unsafePerformIO $ do
    o <- O.create
    forM_ (M.toList m) $ \(k,v) -> do
      O.setProp (toJSString k) (pToJSVal v) o
    return (jsval o)

instance (Show a, PFromJSVal a) => PFromJSVal (M.Map String a) where
  pFromJSVal = M.fromList
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

dynInitializeWith
  :: MonadWidget t m
  => (a -> m b) -> Dynamic t a -> (b -> WidgetHost m ()) -> m ()
dynInitializeWith build v f = addVoidAction . fmap f =<< dyn =<< mapDyn build v

dynInitialize
  :: MonadWidget t m
  => Dynamic t a -> (a -> WidgetHost m ()) -> m ()
dynInitialize = dynInitializeWith return

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
  liftM switchPromptlyDyn $ widgetHold (go v) (fmap go e)


initProperty
  :: (MonadWidget t m, PToJSVal o, PToJSVal a, PFromJSVal a)
  => String -> o -> Property t a -> m (Dynamic t a)
initProperty = initPropertyWith (Just (return . pFromJSVal)) return

initPropertyWith
  :: (MonadWidget t m, PToJSVal o, PToJSVal b)
  => Maybe (JSVal -> m a) -> (a -> m b) -> String -> o -> Property t a
  -> m (Dynamic t a)
initPropertyWith mUnBuild build name ob p = do
  liftWidget <- getLiftWidget
  liftWidget2 <- getLiftWidget
  (newSetValue, update) <- case mUnBuild of
    Nothing -> return (p^.setValue, set)
    Just unBuild -> do
      (emit,suppress) <- mkSuppressor
      eChangeJs <- wrapOLEvent_ ("change:"++name) ob [jsu|$r=`ob.get(`name);|]
      eChange <- performEvent $
        fmap (liftWidget2 . unBuild) (gate emit eChangeJs)
      return (leftmost [p^.setValue, eChange], suppress . set)
  liftIO . update =<< build (p^.initialValue)
  performEvent_ $ fmap (liftIO . update <=< liftWidget . build) (p^.setValue)
  dynFromProp (p & setValue .~ newSetValue)
  where
    set :: PToJSVal c => c -> IO ()
    set v = [jsu_|if(`v!==null) {`ob.set(`name, `v)};|]

getLiftWidget
  :: MonadWidget t m => m (m a -> WidgetHost m a)
getLiftWidget = do
  doc <- askDocument
  runWidget <- getRunWidget
  return $ \act -> do
    Just df <- liftIO $ createDocumentFragment doc
    (result, postBuild, _) <- runWidget df act
    postBuild
    return result


mkSuppressor
  :: MonadWidget t m
  => m (Behavior t Bool, IO a -> IO a)
mkSuppressor = do
  (eEmit, r) <- newEventWithTriggerRef
  emit <- hold True eEmit
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  let setEmit v = mapM_ (\t -> postGui $ runWithActions [t:=>v]) =<< readRef r
      suppress x = setEmit False >> x `finally` setEmit True
  return (emit, suppress)

pushToMap :: (Enum k, Ord k) => a -> M.Map k a -> M.Map k a
pushToMap v m = case M.maxViewWithKey m of
  Nothing          -> M.singleton (toEnum 0) v
  Just ((k, _), _) -> M.insert (succ k) v m


mapDynIO :: MonadWidget t m => (a -> IO b) -> Dynamic t a -> m (Dynamic t b)
mapDynIO f d = do
  initial <- liftIO . f =<< sample (current d)
  eChange <- performEvent $ fmap (liftIO . f) (updated d)
  holdDyn initial eChange
