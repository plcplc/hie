{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Data.MaybeInstance where


{-
data HieValue (caps :: [* -> Constraint]) =
  forall a. HieValue
  {
    hieValue :: a,
    hieUi    :: Maybe (Term (UiDomain caps)),
    hieCapabilites :: DM.DMap (CapabilityKey caps a) Dict
  }


type family FilterUiDomain
   (caps :: [* -> Constraint]) :: [* -> *] where
  FilterUiDomain ((Ui ui) ': cs) = ui ': (FilterUiDomain cs)
  FilterUiDomain (x ': cs) = FilterUiDomain cs
  FilterUiDomain '[]  = '[]

type family SumList (exps :: [* -> *]) :: * -> * where
  SumList (e1 ': '[]) = e1
  SumList (e1 ': es)  = e1 :+: (SumList es)
  SumList '[] = VoidF

type family UiDomain (caps :: [* -> Constraint]) :: * -> * where
  UiDomain caps = SumList (FilterUiDomain caps)

data VoidF e where

-- Use a capability, if it exists.
withCap ::
  forall caps c b.
  ListMember caps c =>
  Proxy (c :: * -> Constraint) ->
  HieValue caps ->
  (forall a. c a => a -> b) ->
  Maybe b
withCap Proxy HieValue{ .. } f = do
  Dict <- DM.lookup (capabilityKey :: CapabilityKey caps a (c a)) hieCapabilites
  return $ f hieValue

-- The method that selects the appropriate ui of a HieValue, constrained by its recorded capabilities.
execUi ::
  forall t m caps.
  (MonadWidget t m) => HieValue caps -> m (Event t (HieValue caps))
execUi HieValue{ .. } = undefined

-- In order to properly record the stated capabilities of a HieValue, we need to
-- do list lookups and mapping over lists at the type level, which is
-- unfortunately very verbose (at least the way I know how to do it) and very
-- meticulous business.

-- A 'CapabilityKey' represents an index into the list of capabilities,
-- parametrised over the content type of the HieValue, yielding the final
-- constraint that the capability is instantiated at for this value.
data CapabilityKey (caps :: [* -> Constraint]) (a :: *) (c :: Constraint) where
  CapabilityKey :: ListIx caps c -> CapabilityKey caps a (c a)

-- Type level proof that an 'a' is a member of list 'l'
data ListIx (l :: [k]) (a :: k) where
  ListIxHead :: ListIx (a ': l) a
  ListIxTail :: ListIx l a -> ListIx (b ': l) a

deriving instance Show (ListIx caps c)
deriving instance Show (CapabilityKey caps a c)

-- Given evidence that 
capabilityKey :: ListMember caps c => CapabilityKey caps a (c a)
capabilityKey = CapabilityKey listIx

class ListMember (caps :: [k]) (c :: k) where
  listIx :: ListIx caps c

instance {-# OVERLAPS #-} ListMember (c ': cs) c where
  listIx = ListIxHead
  
instance {-# OVERLAPPABLE #-} ListMember cs c => ListMember (c' ': cs) c where
  listIx = ListIxTail listIx

instance GEq (ListIx caps) where
  geq ListIxHead ListIxHead = Just Refl
  geq (ListIxTail ix) (ListIxTail ix') = geq ix ix'
  geq _ _ = Nothing

instance GCompare (ListIx caps) where
  gcompare ListIxHead ListIxHead = GEQ
  gcompare (ListIxTail _) ListIxHead = GGT
  gcompare ListIxHead (ListIxTail _) = GLT
  gcompare (ListIxTail ix) (ListIxTail ix') = gcompare ix ix'

instance GEq (CapabilityKey caps a) where
  geq (CapabilityKey ix) (CapabilityKey ix') =
    do
      Refl <- geq ix ix'
      return Refl

instance GCompare (CapabilityKey caps a) where
  gcompare (CapabilityKey ix) (CapabilityKey ix') =
    case gcompare ix ix' of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT

-- Class that captures a single capability
class
  ListMember caps c =>
  CaptureCapability (caps :: [* -> Constraint]) (c :: * -> Constraint) (a :: *) where
  capability :: Proxy c -> Maybe (DM.DMap (CapabilityKey caps a) Dict)

instance
  (ListMember caps c,
  MaybeInstance c a) =>
  CaptureCapability (caps :: [* -> Constraint]) (c :: * -> Constraint) (a :: *) where
  capability Proxy = do
    inst <- maybeInstance
    return $ DM.singleton (capabilityKey :: CapabilityKey caps a (c a)) inst

-- Class that captures a whole list of capabilities, remembering the whole list
-- and counting towards the end.
class CaptureCapabilities' (caps :: [* -> Constraint]) (caps' :: [* -> Constraint]) (a :: *) where
  capabilities' :: Proxy caps' -> [DM.DMap (CapabilityKey caps a) Dict]

instance
  (CaptureCapability caps c a,
  CaptureCapabilities' caps cs (a :: *)) =>
  CaptureCapabilities' caps (c ': cs) (a :: *) where
  capabilities' Proxy =
    case capability (Proxy :: Proxy c) of
      Nothing -> capabilities' (Proxy :: Proxy cs)
      Just cap ->  cap : capabilities' (Proxy :: Proxy cs) 

instance
  CaptureCapabilities' caps '[] (a :: *) where
  capabilities' Proxy = []

class CaptureCapabilities (caps :: [* -> Constraint]) (a :: *) where
  capabilities :: DM.DMap (CapabilityKey caps a) Dict

instance
  (CaptureCapabilities' caps caps a) =>
  CaptureCapabilities (caps :: [* -> Constraint]) (a :: *) where
  capabilities = DM.unions (capabilities' (Proxy :: Proxy caps))

-- Creating a HieValue must happen in the context of given capabilities. That
-- is, it cannot happen in library code without propagating the
-- 'CaptureCapabilities caps a' constraint.
value :: CaptureCapabilities caps a => a -> HieValue caps
value x = HieValue x Nothing capabilities
  -}

  {-
valueUi :: CaptureCapabilities caps a => a -> Term (UiDomain caps) -> HieValue caps
valueUi x ui' = HieValue x (Just ui') capabilities
-}

-- Ui lib

data UiFunc e
data UiRichText e
data Markdown

instance Ui UiRichText Markdown where
  --ui = undefined

-- This is morally the contents of 'instance Ui UiFunc (a -> b)', but the
-- 'CaptureCapabilities caps b' constraint cannot be put on the 'Ui'-instance,
-- as the 'caps' variable doesn't appear in the instance head. Nor could we
-- possibly make it so, because refering to capabilities from inside a
-- capability (a user of the libarary would have to define "type AppCapabilities
-- = [.. , Ui UiFunc AppCapabilities, ..]", which is a no-go.
--
-- However, given the concrete "AppCapabilities" a user is free to write:
--
--     instance (CaptureCapabilities AppCapabilities b) => Ui UiFunc (a -> b) where
--       ui = uiFunc
--
uiFunc :: (MonadWidget t m{-, CaptureCapabilities caps b-}) => Term UiFunc -> (a -> b) -> m (Event t (HieValue caps))
uiFunc = undefined

-- TODO: An improved 'uiFunc':
-- uiFunc ::
--   (MonadWidget t m, CaptureCapabilities caps b, ListMember caps c) =>
--   Term UiFunc ->
--   (c a => a -> b) ->
--   m (Event t (HieValue caps))
-- TODO: how do we encode multiple constraints nicely?

-- Example app

type AppCaps = '[Ui UiFunc, Ui UiRichText]

-- instance (CaptureCapabilities AppCaps b) => Ui UiFunc (a -> b) where
  -- ui = uiFunc
