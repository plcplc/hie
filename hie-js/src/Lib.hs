{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
    {-( hieJsMain
    )-} where

import Control.Monad
import Control.Monad.Reader
import Data.Comp
import Data.Functor.Misc
import Data.Monoid
import Data.Proxy
import Data.Time
import Data.Typeable
import GHC.TypeLits
import Reflex.Dom
import Unsafe.Coerce
import qualified Data.Map as M
import GHC.Exts
import Data.Constraint

data PolyDyn where
  PolyDyn :: TypeRep -> a -> PolyDyn

polyDyn :: Typeable a => a -> PolyDyn
polyDyn x = PolyDyn (typeOf x) x

-- Type signatures for matching 'PolyDyn's. Use 'TyVar'
-- polymorphic 'UiImplementation's.
newtype TySigRep = TySigRep { unTySig :: TypeRep }

-- | Special, sanctioned type to use in place of free type variables.
newtype TyVar (name :: Symbol) = MkTyVar { unTyVar :: forall a. a }

-- | Construct a 'TySig'. It is restricted to kind Star. Use 'PolyVar'
-- in place of type variables.
tySig :: (Typeable (a :: *)) => Proxy a -> TySigRep
tySig = TySigRep . typeRep

-- | Pattern matching for TypeRep and TySigRep.
pattern Ty' con args <- (splitTyConApp -> (con, args))
pattern TySig' con args <- (fmap (map TySigRep) . splitTyConApp . unTySig -> (con, args))
pattern TyVar' v <-
  TySig'
    ((== typeRepTyCon (typeRep (Proxy :: Proxy (TyVar "")))) -> True)
    [TySig' (tyConName -> v) []]

-- | Match a TypeRep with a TySig. Yields a mapping from PolyVar names to
-- corresponding TypeReps.
tySigMatch :: TypeRep -> TySigRep -> Maybe (M.Map String TypeRep)
tySigMatch (Ty' c1 a1) (TySig' c2 a2)
  | c1 == c2 && length a1 == length a2
  -- TODO: Throw up if unifying the same name to different TypeReps.
  = foldr M.union M.empty <$> (sequence $ zipWith tySigMatch a1 a2)
tySigMatch ty           (TyVar' name) = Just (M.singleton name ty)
tySigMatch _ _ = Nothing

-- | Deconstruct a 'PolyDyn'. Occurences of 'TyVar name' in 'sig' will
-- match anyhting. Values of type 'TyVar name' may be turned into
-- 'PolyDyn' by means of the supplied 'ToPolyDyn' function.
matchPolyDyn ::
  forall sig b. Typeable sig =>
  PolyDyn ->
  (ToPolyDyn -> sig -> b) ->
  Maybe b
matchPolyDyn (PolyDyn tx x) f 
  | Just typeRepEnv <- tySigMatch tx (tySig (Proxy :: Proxy sig)) = Just (f (toDyn typeRepEnv) (unsafeCoerce x))
  where
    toDyn :: M.Map String TypeRep -> (KnownSymbol name => TyVar name -> PolyDyn)
    toDyn env (MkTyVar x' :: TyVar name) =
      let
        pn = (Proxy :: Proxy name)
        name = symbolVal pn
      -- Not exactly pretty, but that's how they appear in TypeReps...
      in case M.lookup ("\"" ++ name ++ "\"") env of
        Just tx' -> PolyDyn tx' x'
        Nothing -> error $ "PolyVar " ++ name ++ " is not present in type map"
    
matchPolyDyn _ _ = Nothing

type ToPolyDyn = forall name. KnownSymbol name => TyVar name -> PolyDyn
newtype ToPolyDyn' = ToPolyDyn' ToPolyDyn

data UiImpl uidomain t m where
  UiImpl ::
    (Typeable sig, ui :<: uidomain,
     MonadWidget t m,
     Functor m,
     Reflex t) =>
    (ui (Term uidomain) ->
    ToPolyDyn' ->
    Dynamic t sig ->
    m (HieSessionActions t uidomain, Event t sig)) ->
    UiImpl uidomain t m

type UiEnv uidomain t m = [UiImpl uidomain t m]

lookupUiEnv ::
  forall uidomain t m.
  MonadHold t m =>
  UiEnv uidomain t m ->
  Term uidomain -> 
  (PolyDyn, Event t PolyDyn) ->
  Maybe (m (HieSessionActions t uidomain, Event t PolyDyn))
lookupUiEnv env (Term ui) (pd, pdEv) = do
  getFirst . mconcat . map (\impl -> First $ liftUiImpl impl) $ env

  where
    liftUiImpl ::
      UiImpl uidomain t m ->
      Maybe (m (HieSessionActions t uidomain, Event t PolyDyn))
    liftUiImpl (UiImpl impl) = do
      ui' <- proj ui
      (toDyn, sig) <- matchPolyDyn pd (\toDyn d -> (ToPolyDyn' toDyn, d))
      let sigEvMaybe =
            (\pd' -> matchPolyDyn pd' (\_ d -> d)) <$>
            pdEv

      return $ do
        sigDyn <- holdDyn sig (fmapMaybe id sigEvMaybe)
        (sesActions, updateValEv) <- impl ui' toDyn sigDyn
        return (sesActions, polyDyn <$> updateValEv)

  {-
class (Monad m) => MonadHieUi m uidomain t where
  getUi ::
    Term uidomain ->
    Dynamic t PolyDyn ->
    m (Maybe (m (HieSessionActions t uidomain, Event t PolyDyn)))
-}

  {-
instance
  (MonadHold t m,
   MonadReader (UiEnv uidomain t (HieUiReaderT m uidomain t))
               (HieUiReaderT m uidomain t),
   MonadSample t m, MonadReader (UiEnv uidomain t m) m) =>
  MonadHieUi (HieUiReaderT m uidomain t) uidomain t where
  getUi ui pdDyn = do
    env <- HieUiReaderT ask
    pd <- HieUiReaderT $ sample $ current pdDyn
    return $ lookupUiEnv env ui (pd, updated pdDyn)

newtype HieUiReaderT m uidomain t a = HieUiReaderT { unHieUiReaderT :: ReaderT (UiEnv uidomain t (HieUiReaderT m uidomain t)) m a }
  deriving (Functor, Applicative, Monad)

runHieUiReaderT a ev = runReaderT (unHieUiReaderT a) ev

-}
    
-- * Uidomains:

pattern Proj x <- (proj -> Just x)

data UiNonShowable e = UiNonShowable
  deriving (Eq, Functor)

instance EqF UiNonShowable where
  eqF _ _ = True

data UiTextLabel e = UiTextLabel
  deriving (Eq, Functor)

instance EqF UiTextLabel where
  eqF _ _ = True

data UiList e = UiList e
  deriving (Eq, Functor)

instance EqF UiList where
  eqF _ _ = True

data UiFunc e = UiFunc String e
  deriving (Eq, Functor)

instance EqF UiFunc where
  eqF _ _ = True

uiNonShowable :: (UiNonShowable :<: uidomain) => Term uidomain
uiNonShowable = Term $ inj UiNonShowable

uiTextLabel :: (UiTextLabel :<: uidomain) => Term uidomain
uiTextLabel = Term $ inj UiTextLabel

uiList :: (UiList :<: uidomain) => Term uidomain -> Term uidomain
uiList x = Term $ inj (UiList x)

uiFunc :: (UiFunc :<: uidomain) => String -> Term uidomain -> Term uidomain
uiFunc title resultui = Term $ inj (UiFunc title resultui)

-- * Reflex-based ui

data HieSessionActions t uidomain =
  HieSessionActions {
    addValue    :: Event t (String, HieValue uidomain)
    }

noAction :: Reflex t => HieSessionActions t uidomain
noAction = HieSessionActions never

uiNonShowableImpl ::
  forall uidomain t m.
  (MonadWidget t m, UiNonShowable :<: uidomain) =>
  UiImpl uidomain t m
uiNonShowableImpl = UiImpl go
  where
    go ::
      (sig ~ TyVar "x") =>
      UiNonShowable (Term uidomain) ->
      ToPolyDyn' ->
      Dynamic t sig ->
      m (HieSessionActions t uidomain, Event t sig)
    go _ _ _ = text "<Nonshowable>" >> return (noAction, never)

uiTextLabelImpl ::
  forall a uidomain t m.
  (MonadWidget t m, Typeable a, UiTextLabel :<: uidomain) =>
  (a -> String) ->
  UiImpl uidomain t m
uiTextLabelImpl f = UiImpl go
  where
    go :: UiTextLabel (Term uidomain) -> ToPolyDyn' -> Dynamic t a -> m (HieSessionActions t uidomain, Event t a)
    go _ _ x = do
      strDyn <- mapDyn f x
      dynText strDyn
      return (noAction, never)

  {-
uiListImpl ::
  forall t m.
  (MonadWidget t m) =>
  UiImpl UiDomain m
uiListImpl = UiImpl go
  where
    go :: UiList (Term UiDomain) -> ToPolyDyn' -> [TyVar "x"] -> m ()
    go (UiList innerUi) (ToPolyDyn' d) xs = do
      -- This should really be encapsulated in a monad x_x...
      uiEnv' <- uiEnv
      mapM_ (\pd ->  do
               case lookupUiEnv uiEnv' innerUi pd of
                 Nothing -> el "div" (text "Designated UI is nonexistant or incompatible")
                 Just m -> el "div" m
           ) (map d xs)
-}

uiFuncImpl ::
  forall uidomain t m.
  (UiFunc :<: uidomain, MonadWidget t m, Reflex t) =>
  UiImpl uidomain t m
uiFuncImpl = UiImpl go
  where
    go ::
      (sig ~ (TyVar "a" -> TyVar "b")) =>
      UiFunc (Term uidomain) ->
      ToPolyDyn' ->
      Dynamic t sig ->
      m (HieSessionActions t uidomain, Event t sig)
    go _ _ _ = return (noAction, never)

data HieValue uidomain =
  HieValue {
    hieValue :: PolyDyn,
    hieUi    :: Term uidomain
    }

data UiSession t uidomain = UiSession {
    sessionModel :: Dynamic t (M.Map String (HieValue uidomain))
    }

type UiDomain = UiList :+: UiTextLabel :+: UiNonShowable :+: UiFunc

hieJsMain :: IO ()
hieJsMain = mainWidget $ mdo

  el "h1" (text "HIE Document")

  -- Initial test bindings
  addBindingEvent <-
    (("foo", HieValue (polyDyn (42 :: Int)) uiTextLabel) <$) <$> getPostBuild
  
  wireSession addBindingEvent
  return ()

uiEnv :: forall t m . (MonadWidget t m) =>m (UiEnv UiDomain t m)
--uiEnv :: HieUiMonad m UiDomain t => m (UiEnv UiDomain t m)
uiEnv = return
  [
    (uiTextLabelImpl (show :: Double -> String)),
    (uiTextLabelImpl (show :: Int -> String)),
    (uiTextLabelImpl (show :: Char -> String)),
    (uiTextLabelImpl (id :: String -> String)),
     -- uiListImpl,
    uiNonShowableImpl,
    uiFuncImpl
  ]

wireSession ::
  forall t m.
  MonadWidget t m =>
  MonadHold t m =>
  Event t (String, HieValue UiDomain) ->
  m ()
wireSession addEvent = do

  let addEvent' = uncurry M.singleton . (\(f,s) -> (f, Just s)) <$> addEvent

  -- TODO: wire 'HieSessionActions's with recursive binding.
  widgetModelDyn <- listWithKeyShallowDiff M.empty addEvent' bindingWidget

  -- TODO: Construct a suitable session (data)model.
  return ()
  {-

  -- TODO: Consider merging HieSessionActions to a single, compound event.
  -- All this merging and extracting seems a little awkward..

  let switchUiAction actionDyn selector =
        switchPromptlyDyn <$>
        mapDyn (mergeMap . M.map selector) (joinDynThroughMap actionDyn)

  let updateEndo f = fmap (mconcat . map (Endo . f ) . M.toList)
  let updateValEndo set = updateEndo (\(name, val) -> M.update (Just . set val ) name)

  addValueEvents    <-
    updateEndo (uncurry M.insert . snd)
      <$> switchUiAction bindingEventsDyn addValue
  -}

  {-

  -- NOTE: 'updateValue' and 'updateUi' shouldn't make model updates
  -- in this function. Rather, they are wired at their use site. A use
  -- case for observing these events here could be to update a server.

  let switchUiAction actionDyn selector =
        switchPromptlyDyn <$>
        mapDyn (mergeMap . M.map selector) (joinDynThroughMap actionDyn)

  let updateEndo f = fmap (mconcat . map (Endo . f ) . M.toList)
  let updateValEndo set = updateEndo (\(name, val) -> M.update (Just . set val ) name)

  updateValueEvents <-
    updateValEndo (\val v -> v { hieValue = val })
    <$> switchUiAction bindingEventsDyn updateValue

  updateUiEvents    <-
    updateValEndo (\ui v -> v { hieUi = ui })
    <$> switchUiAction bindingEventsDyn updateUi

  let updateEvents = mergeWith mappend [addValueEvents, updateValueEvents, updateUiEvents]
  -}


bindingWidget ::
  (MonadWidget t m) =>
  String ->
  HieValue UiDomain ->
  Event t (HieValue UiDomain)->
  m (HieSessionActions t UiDomain, Dynamic t (HieValue UiDomain))
bindingWidget name val updateBindingEvent = el "div" $ mdo

  el "h2" (text name)

  let valueUpdateEvent = leftmost
        [
          hieValue <$> updateBindingEvent
        ]
  let uiUpdateEvent = leftmost
        [
          hieUi <$> updateBindingEvent,
          selectUiEvent
        ]
    
  (actionEvents, bindingUpdatedEvent) <-
    valueWidget val valueUpdateEvent uiUpdateEvent
  selectUiEvent <- uiSelector

  bindingDyn <- holdDyn val bindingUpdatedEvent

  return (actionEvents, bindingDyn)

valueWidget ::
  forall t m.
  (MonadHold t m, MonadWidget t m) =>
  HieValue UiDomain ->
  Event t (PolyDyn) ->
  Event t (Term UiDomain) ->
  m (HieSessionActions t UiDomain, Event t (HieValue UiDomain))
valueWidget val valueUpdateEvent uiUpdateEvent = do

  uiDyn <- holdDyn (hieUi val) uiUpdateEvent

  x <- dyn =<< mapDyn (\ui -> do
                     uiEnv' <- uiEnv
                     case lookupUiEnv uiEnv' ui (hieValue val, valueUpdateEvent) of
                         Nothing -> do
                           text "Designated UI is nonexistant or incompatible"
                           return (noAction, never)
                         Just es -> es
                         ) uiDyn

  sessionActions <- switchSessionActions (fmap (\(e,_) -> e) x)
  valUpdateEndoEvent <- switchPromptly never (fmap (\(_,e) -> e) x)
  return (
    sessionActions,
    (\(ui, pd) -> HieValue pd ui) <$> attachDyn uiDyn valUpdateEndoEvent)

switchSessionActions ::
  (MonadHold t m, Reflex t) =>
  Event t (HieSessionActions t uidomain) ->
  m (HieSessionActions t uidomain)
switchSessionActions ev = HieSessionActions <$> switchPromptly never ((fmap addValue) ev)

uiSelector ::
  (Reflex t, MonadWidget t m) =>
  m (Event t (Term UiDomain))
uiSelector = do

  (uiListTextLabelBtn, _) <- el' "button" (text "List[TextLabel]")
  (uiTextLabelBtn, _) <- el' "button" (text "TextLabel")
  (uiNonShowableBtn, _) <- el' "button" (text "NonShowable")

  return $ leftmost $
    [
      (uiList uiTextLabel) <$ domEvent Click uiListTextLabelBtn,
      (uiTextLabel) <$ domEvent Click uiTextLabelBtn,
      (uiNonShowable) <$ domEvent Click uiNonShowableBtn
    ]
