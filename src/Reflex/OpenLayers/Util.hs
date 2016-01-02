{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}

module Reflex.OpenLayers.Util (
    olSetter
  , initProp
  , initPropWith
  , initOLProp
  , initOLPropWith
  , wrapObservableProp
  , declareProperties
  , hasProperties
  ) where

import Reflex.OpenLayers.Event

import Reflex
import Reflex.Dom
import Reflex.Host.Class (newEventWithTrigger)

import Control.Lens (Lens', (^.), lens)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Dependent.Sum (DSum (..))
import GHCJS.Marshal (ToJSVal(toJSVal), FromJSVal(fromJSVal))
import GHCJS.Marshal.Pure (pToJSVal)
import GHCJS.Types
import GHCJS.DOM.Types (toJSString)
import Language.Haskell.TH
import Data.Char

initProp
  :: (ToJSVal a, MonadWidget t m)
  => (JSVal -> a -> WidgetHost m ())
  -> Lens' cfg (Dynamic t a)
  -> JSVal -> cfg -> m ()
initProp setter lens_ = initPropWith setter lens_ return

initPropWith
  :: (ToJSVal a, MonadWidget t m)
  => (JSVal -> a -> WidgetHost m ())
  -> Lens' cfg (Dynamic t b)
  -> (b -> WidgetHost m a)
  -> JSVal -> cfg -> m ()
initPropWith setter lens_ build jsVal cfg = do
  schedulePostBuild $ do
    src <- build =<< sample (current (cfg^.lens_))
    setter jsVal src
  performEvent_ $
    fmap (build >=> setter jsVal)
    (updated (cfg^.lens_))

wrapObservableProp
  :: (ToJSVal a, FromJSVal b, MonadWidget t m)
  => String
  -> (a -> b)
  -> JSVal
  -> a
  -> Event t a
  -> m (Dynamic t b)
wrapObservableProp name wrap obj initialValue eSet = do
  let jName  = toJSString name
      err = fail "wrapObservableProp.set (fromJSVal)"
      set = liftIO . (toJSVal >=> googSet jName (pToJSVal False) obj)
      get = liftIO (googGet jName obj >>= fromJSVal >>= maybe err return)
  performEvent_ $ fmap set eSet
  schedulePostBuild $ set initialValue

  postGui <- askPostGui
  runWithActions <- askRunWithActions
  -- subscribe to change event
  ev <- newEventWithTrigger $ \trig -> do
    unsubscribe <- liftIO $ on_ ("change:"++name) obj $ \_ -> do
      val <- get
      postGui $ runWithActions [trig :=> val]
    return (liftIO unsubscribe)
  holdDyn (wrap initialValue) ev

initOLProp
  :: (ToJSVal a, MonadWidget t m)
  => String
  -> Lens' cfg (Dynamic t a)
  -> JSVal -> cfg -> m ()
initOLProp = initProp . olSetter

initOLPropWith
  :: (ToJSVal a, MonadWidget t m)
  => String
  -> Lens' cfg (Dynamic t b)
  -> (b -> WidgetHost m a)
  -> JSVal -> cfg -> m ()
initOLPropWith = initPropWith . olSetter

olSetter
  :: (MonadIO m , ToJSVal a, ToJSVal b)
  => String -> a -> b -> m ()
olSetter propName obj val = liftIO $ do
  obj' <- toJSVal obj
  val' <- toJSVal val
  js_olSetter (toJSString propName)  obj' val'

foreign import javascript unsafe "$2[$1]($3)"
  js_olSetter :: JSString -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$3['set']($1,$4,$2)"
  googSet :: JSString -> JSVal -> JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$2['get']($1)"
  googGet :: JSString -> JSVal -> IO JSVal

declareProperty :: String -> Q Dec
declareProperty name = do
  o <- newName "o"
  a <- newName "a"
  return $
    ClassD [] clsName [PlainTV o,PlainTV a] [FunDep [o] [a]]
      [SigD (mkName name) (AppT (AppT (ConT ''Lens') (VarT o)) (VarT a))]
  where
    clsName = mkName ("Has" ++ capitalize name)

declareProperties :: [String] -> Q [Dec]
declareProperties = mapM declareProperty

hasProperty :: Name -> String -> Q Dec
hasProperty name propName = do
  TyConI (DataD _ _ tyVars [RecC _ fields] []) <- reify name
  o <- newName "o"
  v <- newName "v"
  let (fName,_,fType) = findField fields
      setter = LamE [VarP o,VarP v] (RecUpdE (VarE o) [(fName,VarE v)])
      theLens = VarE 'lens `AppE` (VarE fName) `AppE` setter
      gTyName (KindedTV n _) = n
      gTyName (PlainTV n) = n
      tyNames = map gTyName tyVars
  return $
    InstanceD
      []
      (ConT clsName
        `AppT` (foldl AppT (ConT name) (map VarT tyNames))
        `AppT` fType)
      [ValD (VarP (mkName propName)) (NormalB theLens) []]
  where
    clsName = mkName ("Has" ++ capitalize propName)
    findField [] = error $ "hasProperty: " ++
                   "could not find field " ++ propName ++ " on " ++ show name
    findField (x@(n,_,_):xs)
      | take (length propName) (reverse (nameBase n)) == reverse propName = x
      | otherwise = findField xs

hasProperties :: Name -> [String] -> Q [Dec]
hasProperties name = mapM (hasProperty name)


capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs
