{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Hie.Session where


import Control.Monad.Except
import Data.Comp
import Data.Dynamic.PolyDyn
import Data.Monoid
import Hie.Ui.Types
import Reflex.Aux
import Reflex.Dom
import qualified Data.Map as M
import qualified Data.Map.Lazy as LM

lookupUiEnv ::
  forall uidomain t m.
  MonadHold t m =>
  UiEnv uidomain t m ->
  LookupUi uidomain t m
lookupUiEnv env (Term ui) (pd, pdEv) lookupHieVal = do
  getFirst . mconcat . map (\impl -> First $ liftUiImpl impl) $ env

  where
    liftUiImpl ::
      UiImpl uidomain t m ->
      Maybe (m (HieSessionActions t uidomain, Event t PolyDyn))
    liftUiImpl (UiImpl impl) = do
      ui' <- proj ui
      sig <- polyMatch pd
      let sigEvMaybe = polyMatch <$> pdEv

      return $ do
        sigDyn <- holdDyn sig (fmapMaybe id sigEvMaybe)
        (sesActions, updateValEv) <- impl lookupHieVal (lookupUiEnv env) ui' sigDyn
        return (sesActions, eraseMatch <$> updateValEv)

data UiSession t uidomain =
  UiSession {
    sessionModel :: Dynamic t (M.Map String (HieValue uidomain))
    }

wireSession ::
  forall t m uidomain.
  MonadWidget t m =>
  MonadFix m =>
  MonadHold t m =>
  UiEnv uidomain t m ->
  m (Event t (Term uidomain)) ->
  Event t (M.Map String (Maybe (HieValue uidomain))) ->
  m ()
wireSession uiEnv uiSelector updateBindingsExo = mdo

  updateBindingsEndo <-
    switchPromptlyDyn <$>
    mapDyn (mergeWith M.union . M.elems . M.map (updateBindings . fst)) widgetModelDyn 
  let updateBindings' = mergeWith M.union [updateBindingsExo, updateBindingsEndo]

  widgetModelDyn <- listWithKeyShallowDiff M.empty updateBindings' (bindingWidget uiEnv uiSelector valuesModel)

  valuesModel' <- mapDyn (M.map snd) widgetModelDyn 
  let valuesModel k = joinDynThroughMaybe $ selectMapShallow valuesModel' k

  -- TODO: Construct a suitable session (data)model.
  return ()

bindingWidget ::
  (MonadWidget t m) =>
  (UiEnv uidomain t m) ->
  m (Event t (Term uidomain)) ->
  (String -> Dynamic t (Maybe (HieValue uidomain))) ->
  String ->
  HieValue uidomain ->
  Event t (HieValue uidomain)->
  m (HieSessionActions t uidomain, Dynamic t (HieValue uidomain))
bindingWidget uiEnv uiSelector lookupHieVal name val updateBindingEvent =
  elWith "div" bindingCssStyle $ mdo

  el "h2" $ do
    valTy <- holdDyn
             (polyDynTy . hieValue $ val)
             (polyDynTy . hieValue <$> updateBindingEvent)

    nameDyn <- mapDyn (\t -> name ++ " : "  ++ show t) valTy
    dynText nameDyn

  let valueUpdateEvent = leftmost
        [
          hieValue <$> updateBindingEvent
        ]
  let uiUpdateEvent = leftmost
        [
          hieUi <$> updateBindingEvent,
          selectUiEvent
        ]
    
  selectUiEvent <- el "div" uiSelector
  (actionEvents, bindingUpdatedEvent) <- el "div" $
    valueWidget uiEnv lookupHieVal val valueUpdateEvent uiUpdateEvent

  bindingDyn <- holdDyn val bindingUpdatedEvent

  return (actionEvents, bindingDyn)

  where

    bindingCssStyle = def {
      _elConfig_attributes = LM.fromList [("style", "background: rgb(240, 240, 240);")]
      }

valueWidget ::
  forall t m uidomain.
  (MonadHold t m, MonadWidget t m) =>
  (UiEnv uidomain t m) ->
  (String -> Dynamic t (Maybe (HieValue uidomain))) ->
  HieValue uidomain ->
  Event t (PolyDyn) ->
  Event t (Term uidomain) ->
  m (HieSessionActions t uidomain, Event t (HieValue uidomain))
valueWidget uiEnv lookupHieVal val valueUpdateEvent uiUpdateEvent = do

  uiDyn <- holdDyn (hieUi val) uiUpdateEvent

  x <- dyn =<<
    mapDyn (\ui -> do
      case lookupUiEnv uiEnv ui (hieValue val, valueUpdateEvent) lookupHieVal of
        Nothing -> do
          text "Designated UI is nonexistant or incompatible"
          return (noAction, never)
        Just es -> es
           ) uiDyn

  sessionActions <- switchSessionActions (HieSessionActions never) (fmap (\(e,_) -> e) x)
  valUpdateEndoEvent <- switchPromptly never (fmap (\(_,e) -> e) x)
  return (
    sessionActions,
    (\(ui, pd) -> HieValue pd ui) <$> attachDyn uiDyn valUpdateEndoEvent)

switchSessionActions ::
  (MonadHold t m, Reflex t) =>
  HieSessionActions t uidomain ->
  Event t (HieSessionActions t uidomain) ->
  m (HieSessionActions t uidomain)
switchSessionActions acts ev =
  HieSessionActions <$> switchPromptly (updateBindings acts) ((fmap updateBindings) ev)

switchSessionActionsDyn ::
  (MonadHold t m, Reflex t) =>
  Dynamic t (HieSessionActions t uidomain) ->
  m (HieSessionActions t uidomain)
switchSessionActionsDyn actsDyn = do
  updateDyn <- mapDyn updateBindings actsDyn
  return (HieSessionActions $ switchPromptlyDyn updateDyn)
