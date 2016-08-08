{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Hie.Ui.Types where

import Data.Comp
import Data.Dynamic.PolyDyn
import Data.Typeable
import Reflex.Dom
import qualified Data.Map as M

pattern Proj x <- (proj -> Just x)

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

-- | A data type expressing a grammar of 'f'-expression trees, represented by
-- lazy potentially infinite syntax tree of all the expressions permitted by the
-- grammar.
newtype UiGrammar f = UiGrammar { unGrammar :: [f (UiGrammar f)] }

allGrammar :: [UiGrammar f -> f (UiGrammar f)] -> UiGrammar f
allGrammar fs = UiGrammar (map ($ allGrammar fs) fs)

-- | Applicative interface to Dynamic values. Used by the 'uiSelector'. This is
-- necessary for traversals (using 'traverse') that generate 'Dynamic's.
newtype UiBuilder m t a = UiBuilder {unUiBuilder :: m (Dynamic t a)}

instance (MonadHold t m, Reflex t) => Functor (UiBuilder m t) where
  fmap f (UiBuilder m) = UiBuilder $ mapDyn f =<< m
  x <$ _ = UiBuilder $ return $ constDyn x

instance (MonadHold t m, Reflex t) => Applicative (UiBuilder m t) where
  pure = UiBuilder . return . constDyn
  (UiBuilder f) <*> (UiBuilder x) = UiBuilder $ do
    f' <- f
    x' <- x
    mapDyn (\(f'', x'') -> f'' x'') =<< collectDyn (f', x')

class UiSelectable (ui :: * -> *) where
  uiIdentifier :: ui a -> String
  enumerateUi  :: [Const ui]

instance (
  ui1 :<: (ui1 :+: ui2),
  ui2 :<: (ui1 :+: ui2),
  UiSelectable ui1,
  UiSelectable ui2
  ) =>
  UiSelectable (ui1 :+: ui2) where

  uiIdentifier = caseF uiIdentifier uiIdentifier

  enumerateUi =
    (map inj (enumerateUi :: [Const ui1])) ++
    (map inj (enumerateUi :: [Const ui2]))

