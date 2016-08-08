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
import Data.List
import Data.Maybe
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
  UiSelectable uidomain =>
  Traversable uidomain  =>
  MonadWidget t m       =>
  MonadFix m            =>
  MonadHold t m         =>
  UiEnv uidomain t m ->
  Event t (M.Map String (Maybe (HieValue uidomain))) ->
  m ()
wireSession uiEnv updateBindingsExo = mdo

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
  divClass "bindingWidget" $ mdo

  selectUiEvent <- el "div" uiSelector
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
    
  (actionEvents, bindingUpdatedEvent) <- el "div" $
    valueWidget uiEnv lookupHieVal val valueUpdateEvent uiUpdateEvent

  bindingDyn <- holdDyn val bindingUpdatedEvent

  return (actionEvents, bindingDyn)

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

noHoles :: (Traversable f) => Cxt h f a -> Maybe (Term f)
noHoles (Hole _) = Nothing
noHoles (Term x) = Term <$> traverse noHoles x

uiGrammar :: (Functor uidomain, UiSelectable uidomain) => UiGrammar uidomain
uiGrammar = allGrammar [ (<$ ui) | ui <- enumerateUi ]

uiSelector ::
  forall uidomain t m.
  (
    Functor uidomain,
    Traversable uidomain,
    UiSelectable uidomain,
    MonadHold t m,
    Reflex t,
    MonadWidget t m
  ) =>
  m (Event t (Term uidomain))
uiSelector = divClass "uiSelector" $ do
  text "Set ui"
  uiDyn <- uiSelectorStep (uiGrammar :: UiGrammar uidomain)
  return $ push (return . noHoles) (updated uiDyn)

uiSelectorStep ::
  forall uidomain t m.
  (
    Traversable uidomain,
    UiSelectable uidomain,
    MonadHold t m,
    Reflex t,
    MonadWidget t m
  ) =>
  UiGrammar uidomain ->
  -- TODO: Maybe we should just work in terms of 'Event's?
  m (Dynamic t (Cxt Hole uidomain ()))
uiSelectorStep (UiGrammar prods) = divClass "uiSelectorStep" $ mdo

  let idMap = identMap prods
  input <- textInput def

  alternativesDyn <-
    mapDyn (\v -> filter (isPrefixOf v) (M.keys idMap)) (value input)

  -- Show the list of alternatves, as long as a choice has not been comitted.
  _ <- elDynAttr "ul" altAttrs $ do
    dyn =<< mapDyn (mapM (el "li" . text)) alternativesDyn

  altAttrs <- mapDyn
    (\sel ->
     M.singleton "style"
      (if isJust sel
       then "display: none;"
       else "")
    ) selectedDyn

  selectedDyn <-
    mapDyn (`M.lookup` idMap) (value input)

  uiDynEv <- dyn =<< mapDyn (\uiMatch -> case uiMatch of
    Nothing -> return $ constDyn $ Hole ()
    Just ui -> unUiBuilder (Term <$> traverse (UiBuilder . uiSelectorStep) ui)
    ) selectedDyn

  joinDyn <$> holdDyn (constDyn $ Hole ()) uiDynEv

  where

    -- TODO: What if two different 'ui' types yield the same 'uiIdentifier'-strings?
    -- Maybe we should just let the user choose between them, though we will
    -- probably need 'Typeable ui' to usefully differentiate them. But this is
    -- sort of pathological anyway, as 'uiIdentifier' is meant to be unique.
    identMap :: [uidomain a] -> M.Map String (uidomain a)
    identMap = M.unions . map (\ui -> M.singleton (uiIdentifier ui) ui)
