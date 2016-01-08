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
    HasNamedProperty (..)
  , HasInitialValue (..)
  , Property (..)
  , PropertyObj (..)
  , property
  , updateProperty
  , dynInitialize
  , dynInitializeWith
  ) where

import Reflex.OpenLayers.Event

import Reflex
import Reflex.Host.Class
import Reflex.Dom
import Data.Dependent.Sum (DSum (..))

import GHC.TypeLits
import Data.Proxy

import Control.Monad (forM_, when, void, (>=>))
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
import Unsafe.Coerce (unsafeCoerce)
import System.Mem.StableName



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

data Property t (n::Symbol) a
  = Property {
      _propertyInitialValue :: a
    , _propertySetValue     :: Event t a
    }
makeLenses ''Property


class HasInitialValue o a | o->a where
  initialValue :: Lens' o a

instance Reflex t => HasInitialValue (Property t n a) a where
  initialValue = propertyInitialValue

instance Reflex t => HasSetValue (Property t n a) where
  type SetValue (Property t n a) = Event t a
  setValue = propertySetValue

property :: Reflex t => a -> Property t n a
property = flip Property never

instance (Reflex t, Default a) => Default (Property t n a) where
  def = Property def never


data PropertyObj t (n::Symbol) a
  = PropertyObj {
      _propertyObjValue  :: !(Dynamic t a)
    , _propertyObjUpdate :: !(a -> IO ())
    }

updateProperty :: MonadWidget t m => PropertyObj t n a -> Event t a -> m ()
updateProperty (PropertyObj _ act) = performEvent_ . fmap (liftIO . act)

instance Reflex t => HasValue (PropertyObj t n a) where
  type Value (PropertyObj t n a) = Dynamic t a
  value = _propertyObjValue

class (KnownSymbol n, PToJSVal o)
  => HasNamedProperty o n a b t | o n t->a, a->b, b->a where
  initProperty :: MonadWidget t m
               => o -> Property t n a -> m (PropertyObj t n b)

  default initProperty
    :: (MonadWidget t m, PToJSVal a, PFromJSVal a)
    => o -> Property t n a -> m (PropertyObj t n a)
  initProperty o' (Property v e) = do
    let name = symbolVal (Proxy :: Proxy n)
        o    = pToJSVal o'
    (emit, suppress) <- mkSuppressor
    let update v2
          = suppress [jsu_|if(`v2!==null) {`o.set(`name, `v2)};|] -- FIXME
    e' <- wrapOLEvent_ ("change:"++name) o [jsu|`o.get(`name);|]
    r <- PropertyObj <$> holdDyn v (leftmost [e, gate emit e'])
                     <*> pure update
    liftIO $ update v
    updateProperty r e
    return r

mkSuppressor
  :: MonadWidget t m
  => m (Behavior t Bool, IO () -> IO ())
mkSuppressor = do
  (eEmit, r) <- newEventWithTriggerRef
  emit <- hold True eEmit
  postGui <- askPostGui
  runWithActions <- askRunWithActions
  let setEmit v = mapM_ (\t -> postGui $ runWithActions [t:=>v]) =<< readRef r
      suppress x = setEmit False >> x `finally` setEmit True
  return (emit, suppress)
