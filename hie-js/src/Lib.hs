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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Lib
    {-( hieJsMain
    )-} where

import Control.Monad
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

data PolyDyn where
  PolyDyn :: TypeRep -> a -> PolyDyn

polyDyn :: Typeable a => a -> PolyDyn
polyDyn x = PolyDyn (typeOf x) x

-- Type signatures for matching 'PolyDyn's. Use 'TyVar'
-- polymorphic 'UiImplementation's.
newtype TySigRep = TySigRep { unTySig :: TypeRep }

-- | Special, sanctioned type to use in place of free type variables.
data TyVar (name :: Symbol) = MkTyVar { unTyVar :: forall a. a }

-- | Construct a 'TySig'. It is restricted to kind Star. Use 'PolyVar'
-- in place of type variables.
tySig :: (Typeable (a :: *)) => Proxy a -> TySigRep
tySig = TySigRep . typeRep

-- Pattern matching for TypeRep and TySigRep.
pattern Ty' con args <- (splitTyConApp -> (con, args))
pattern TySig' con args <- (fmap (map TySigRep) . splitTyConApp . unTySig -> (con, args))
pattern TyVar' v <-
  TySig'
    ((== typeRepTyCon (typeRep (Proxy :: Proxy (TyVar "")))) -> True)
    [TySig' (tyConName -> v) []]

-- Match a TypeRep with a TySig. Yields a mapping from PolyVar names to
-- corresponding TypeReps.
tySigMatch :: TypeRep -> TySigRep -> Maybe (M.Map String TypeRep)
tySigMatch (Ty' c1 a1) (TySig' c2 a2)
  | c1 == c2 && length a1 == length a2
  -- TODO: Throw up if unifying the same name to different TypeReps.
  = foldr M.union M.empty <$> (sequence $ zipWith tySigMatch a1 a2)
tySigMatch ty           (TyVar' name) = Just (M.singleton name ty)
tySigMatch _ _ = Nothing

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
      in case M.lookup name env of
        Just tx' -> PolyDyn tx' x'
        Nothing -> error $ "PolyVar " ++ name ++ " is not present in type map"
    
matchPolyDyn _ _ = Nothing

type ToPolyDyn = forall name. KnownSymbol name => TyVar name -> PolyDyn
newtype ToPolyDyn' = ToPolyDyn' ToPolyDyn

withPolyDynEnv ::
  (Typeable a) =>
  [PolyDyn] ->
  (ToPolyDyn -> a -> b) ->
  Maybe b
withPolyDynEnv ds f =
  getFirst $ mconcat $ map (\d -> First $ matchPolyDyn d f) ds

data UiImpl ui uidomain t m where
  UiImpl ::
    (Typeable sig) =>
    (
      ui (),
      ui (Term uidomain) ->
      ToPolyDyn' ->
      sig ->
      m ()
    ) -> UiImpl ui uidomain t m

type UiEnv uidomain t m =
  [Term uidomain -> PolyDyn -> Maybe (m ())]

lookupUiEnv ::
  Eq (Const uidomain) =>
  UiEnv uidomain t m ->
  Term uidomain -> 
  PolyDyn ->
  Maybe (m ())
lookupUiEnv env ui pd = getFirst $ mconcat $ map (\f -> First $ f ui pd) env

wrapUiImpl ::
  forall uidomain ui t m.
  (Eq (Const uidomain), Functor ui, EqF ui, Functor uidomain, ui :<: uidomain, MonadWidget t m) =>
  UiImpl ui uidomain t m -> 
  UiEnv uidomain t m
wrapUiImpl (UiImpl (key, impl)) =
  [(
  (\(Term ui) pd -> do
      ui' <- proj ui
      guard (eqF (() <$ ui') key)
      go ui' pd
  ))]

  where

    go :: ui (Term uidomain) -> PolyDyn -> Maybe (m ())
    go ui pd = matchPolyDyn pd (\toDyn d -> impl ui (ToPolyDyn' toDyn) d)
 
-- * Uidomains:

pattern Proj x <- (proj -> Just x)

data UiNonShowable e = UiNonShowable
  deriving (Eq, Functor)

data UiTextLabel e = UiTextLabel
  deriving (Eq, Functor)

instance EqF UiTextLabel where
  eqF _ _ = True

data UiList e = UiList e
  deriving (Eq, Functor)

instance EqF UiList where
  eqF _ _ = True

data UiFunc e = UiFunc e
  deriving (Eq, Functor)

instance EqF UiFunc where
  eqF _ _ = True

uiNonShowable :: (UiNonShowable :<: ui) => Term ui
uiNonShowable = Term $ inj UiNonShowable

uiTextLabel :: (UiTextLabel :<: ui) => Term ui
uiTextLabel = Term $ inj UiTextLabel

uiList :: (UiList :<: ui) => Term ui -> Term ui
uiList x = Term $ inj (UiList x)

-- * Reflex-based ui

uiTextLabelImpl ::
  forall uidomain t m.
  (Functor uidomain, UiTextLabel :<: uidomain, MonadWidget t m) =>
  UiImpl UiTextLabel uidomain t m
uiTextLabelImpl = UiImpl (UiTextLabel, go)
  where
    go :: UiTextLabel (Term uidomain) -> ToPolyDyn' -> String -> m ()
    go _ _ str = text str

data HieValue uidomain where
  HieValue :: PolyDyn -> Term uidomain -> HieValue uidomain

data Session t uidomain = Session {
    sessionModel :: Dynamic t (M.Map String (Dynamic t (HieValue uidomain)))
    }

-- A toy session currently.
type UiDomain = UiList :+: UiTextLabel :+: UiNonShowable
connectSession ::
  MonadWidget t m =>
  MonadHold t m =>
  Dynamic t (M.Map String (Event t (Term UiDomain)))->
  m (Session t UiDomain)
connectSession bindingEventsDyn = do

  bindingEvents <- switchPromptlyDyn <$> mapDyn mergeMap bindingEventsDyn
  let fanBindings = fanMap bindingEvents

  incrementEvent <- tickLossy 1 (UTCTime (toEnum 0) 0)
  fooValDyn <- count incrementEvent >>= mapDyn (polyDyn . show :: Int -> PolyDyn)
  foo1UiDyn <- holdDyn uiTextLabel (select fanBindings (Const2 "foo1"))
  foo1BindingDyn <- combineDyn HieValue fooValDyn foo1UiDyn

  let barValDyn = constDyn (polyDyn (42 :: Int))
  barUiDyn <- holdDyn uiTextLabel (select fanBindings (Const2 "bar"))
  barBindingDyn <- combineDyn HieValue barValDyn barUiDyn

  Session <$> pure (constDyn $ M.fromList [
                       ("foo1", foo1BindingDyn),
                       ("bar", barBindingDyn)
                       ])

hieJsMain :: IO ()
hieJsMain = mainWidget $ mdo

  el "h1" (text "HIE Document")
  Session model <- connectSession bindingEvents
  bindingEvents <- listWithKey model (\name val -> renderBinding name (joinDyn val))
  return ()

uiEnv :: forall t m. MonadWidget t m => m (UiEnv UiDomain t m)
uiEnv = return $ concat [wrapUiImpl uiTextLabelImpl]

renderBinding ::
  (MonadWidget t m) =>
  String -> Dynamic t (HieValue UiDomain) -> m (Event t (Term UiDomain))
renderBinding name val = el "div" $ do
  
  el "h2" (text name)
  _ <- dyn =<< mapDyn renderUi val
  uiSelector

renderUi :: MonadWidget t m => HieValue UiDomain -> m ()
renderUi (HieValue pd ui) = do

  uiEnv' <- uiEnv
  case lookupUiEnv uiEnv' ui pd of
    Nothing -> text "Designated UI is nonexistant or incompatible"
    Just m -> m

uiSelector ::
  (Reflex t, MonadWidget t m) =>
  m (Event t (Term UiDomain))
uiSelector = do

  (uiShowBtn, _) <- el' "button" (text "Show")
  (uiBlackBoxBtn, _) <- el' "button" (text "Black Box")

  return $ leftmost
    [
      (uiTextLabel) <$ domEvent Click uiShowBtn,
      (uiNonShowable) <$ domEvent Click uiBlackBoxBtn
    ]
