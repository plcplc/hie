{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Hie.Ui.NonShowable where

import Data.Comp
import Data.Comp.Show
import Data.Dynamic.PolyDyn
import Hie.Ui.Types
import Reflex.Dom

data UiNonShowable e = UiNonShowable
  deriving (Eq, Functor, Foldable, Traversable)

instance EqF UiNonShowable where
  eqF _ _ = True

instance ShowF UiNonShowable where
  showF _ = "UiNonShowable"

instance UiSelectable UiNonShowable where
  uiIdentifier _ = "NonShowable"
  enumerateUi = [UiNonShowable]

uiNonShowable :: (UiNonShowable :<: uidomain) => Term uidomain
uiNonShowable = Term $ inj UiNonShowable

uiNonShowableImpl ::
  forall uidomain t m.
  (MonadWidget t m, UiNonShowable :<: uidomain) =>
  UiImpl uidomain t m
uiNonShowableImpl = UiImpl go
  where
    go ::
      (sig ~ FreeVar "x") =>
      UiImplK t uidomain m UiNonShowable sig
    go _ _ _ _ = text "<Nonshowable>" >> return (noAction, never)
