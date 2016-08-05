{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Hie.Ui.Types where

import Data.Comp
import Data.Dynamic.PolyDyn
import Data.Typeable
import Reflex.Dom
import qualified Data.Map as M

pattern Proj x <- (proj -> Just x)

data UiNonShowable e = UiNonShowable
  deriving (Eq, Functor)

instance EqF UiNonShowable where
  eqF _ _ = True

uiNonShowable :: (UiNonShowable :<: uidomain) => Term uidomain
uiNonShowable = Term $ inj UiNonShowable

data UiTextLabel e = UiTextLabel
  deriving (Eq, Functor)

instance EqF UiTextLabel where
  eqF _ _ = True

uiTextLabel :: (UiTextLabel :<: uidomain) => Term uidomain
uiTextLabel = Term $ inj UiTextLabel

data UiList e = UiList e
  deriving (Eq, Functor)

instance EqF UiList where
  eqF _ _ = True

uiList :: (UiList :<: uidomain) => Term uidomain -> Term uidomain
uiList x = Term $ inj (UiList x)

data UiFunc e = UiFunc String e
  deriving (Eq, Functor)

instance EqF UiFunc where
  eqF _ _ = True

uiFunc :: (UiFunc :<: uidomain) => String -> Term uidomain -> Term uidomain
uiFunc title resultui = Term $ inj (UiFunc title resultui)

type LookupUi uidomain t m = 
  Term uidomain -> 
  (PolyDyn, Event t PolyDyn) ->
  (String -> Dynamic t (Maybe (HieValue uidomain))) ->
  Maybe (m (HieSessionActions t uidomain, Event t PolyDyn))

data UiImpl uidomain t m where
  UiImpl ::
    (
      Typeable (sig :: *),
      ui :<: uidomain,
      MonadWidget t m,
      Functor m,
      Reflex t
    ) =>
    UiImplK t uidomain m ui sig ->
    UiImpl uidomain t m

type UiImplK t uidomain m ui sig =
  (String -> Dynamic t (Maybe (HieValue uidomain))) ->
  LookupUi uidomain t m ->
  ui (Term uidomain) ->
  Dynamic t (PolyMatch sig) ->
  m (HieSessionActions t uidomain, Event t (PolyMatch sig))

data HieSessionActions t uidomain =
  HieSessionActions {
    updateBindings :: Event t (M.Map String (Maybe (HieValue uidomain)))
    }

instance (Reflex t) => Monoid (HieSessionActions t uidomain) where
  mempty = HieSessionActions never
  mappend bs1 bs2 = mconcat [bs1, bs2]
  mconcat bs = HieSessionActions $ mconcat (map updateBindings bs)

noAction :: Reflex t => HieSessionActions t uidomain
noAction = HieSessionActions never
type UiEnv uidomain t m = [UiImpl uidomain t m]

data HieValue uidomain =
  HieValue {
    hieValue :: PolyDyn,
    hieUi    :: Term uidomain
    }
