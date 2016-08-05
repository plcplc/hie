{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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

import Control.Monad.Except
import Data.Comp
import Data.Dynamic.PolyDyn
import Data.Monoid
import Data.Proxy
import Data.Typeable
import Hie.Ui.Types
import Reflex.Dom
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Map.Lazy as LM

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
