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

module Hie.Ui.TextLabel where

import Data.Comp
import Data.Comp.Show
import Data.Dynamic.PolyDyn
import Data.Typeable
import Hie.Ui.Types
import Reflex.Dom

data UiTextLabel e = UiTextLabel
  deriving (Eq, Functor, Foldable, Traversable)

instance EqF UiTextLabel where
  eqF _ _ = True

instance ShowF UiTextLabel where
  showF _ = "UiTextLabel"

instance UiSelectable UiTextLabel where

  uiIdentifier _ = "TextLabel"
  enumerateUi = [UiTextLabel]

uiTextLabel :: (UiTextLabel :<: uidomain) => Term uidomain
uiTextLabel = Term $ inj UiTextLabel

uiTextLabelImpl ::
  forall a uidomain t m.
  (
    MonadWidget t m,
    Typeable a,
    UiTextLabel :<: uidomain,
    HasTyVars a ~ 'False
  ) =>
  (a -> String) ->
  UiImpl uidomain t m
uiTextLabelImpl f = UiImpl go
  where
    go :: UiImplK t uidomain m UiTextLabel a
    go _ _ _ x = do
      strDyn <- mapDyn (f . unMatch) x
      dynText strDyn
      return (noAction, never)
