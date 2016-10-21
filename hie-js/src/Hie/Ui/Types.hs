{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Hie.Ui.Types where

import Reflex
import Reflex.Dom
import Data.Comp.Term
import Data.Constraint
import Data.Proxy
import qualified Data.Map as M


data HieSessionActions t caps =
  HieSessionActions {
    updateBindings :: Event t (CrudAction caps)
    }

data CrudAction caps =
  CAInsert HieRef (HieValue caps)
  | CADelete HieRef

-- | Data type to identify hie values. Usable for UI in eg. function application
-- and reactive updates.
data HieRef = HieRef Integer
  -- TODO? cases for unique opaque id, friendly unique name, and...?
  deriving (Eq, Ord, Show)

-- * About the structure of a hie document.
--
-- Though we'd like to be able to organise HieValues in various ways depending
-- on the application that the library serves, there is no dedicated structure
-- that holds HieValues. Instead, HieVales that have the 'HieDocument'
-- capability provide means to nest values inside them.
--
-- Then we can better serve more diverse use cases, eg:
--   * A HIE terminal console where values by default scroll up like text in a
--     terminal emulator (needs scrolling vertical list and free floating windows)
--   * An interactive spreadsheet-like document ( needs grids )
--   * An interactive article/report (needs vertical lists, horizontal lists
--     with line breaking)
--
-- The only thing that the 'HieDocument' capability adds to a HieValue is means
-- to localise the and interact with the values it contains. Different ways to
-- render a container is provided by implementing 'Ui' capabilities.
class HieDocument v where
  -- methods to localise and manipulate nested values.
  nestedValues :: v -> M.Map HieRef (Dynamic t (HieValue caps))

class MaybeInstance (cap :: * -> Constraint) a where
  maybeInstance :: Proxy cap -> Proxy a -> Maybe (Dict (cap a))
  default maybeInstance :: (cap a) => Proxy cap -> Proxy a -> Maybe (Dict (cap a))
  maybeInstance _ _= Just Dict

instance {-# OVERLAPPABLE #-} MaybeInstance cap a where
  maybeInstance _ _ = Nothing

class MaybeCaps (caps :: [* -> Constraint]) (a :: *) where
  maybeCaps :: Proxy caps -> Proxy a -> Capabilities caps a

instance MaybeCaps '[] a where
  maybeCaps _ _ = HNil

instance (MaybeInstance c a, MaybeCaps cs a) => MaybeCaps (c ': cs) a where
  maybeCaps (Proxy :: Proxy (c ': cs)) pa =
    HCons
      (maybeInstance (Proxy :: Proxy c) pa)
      (maybeCaps (Proxy :: Proxy cs) pa)

{-
instance (Show a, Show (HList as)) => Show (HList (a ': as)) where
 show (HCons x xs) = show x ++ " : " ++ show xs 

instance Show (HList '[]) where
  show HNil = "[]"
-}

-- To opt in a type for capability discovery, simply declare an instance like the one below.
instance MaybeInstance Show Int where
instance MaybeInstance Read Int where
instance MaybeInstance Show Double where
instance MaybeInstance Read Double where

type Capabilities caps a = HList (MapMaybeDict caps a)
type UiDomain uidomain caps = (uidomain ~ FilterUiDomain caps, AllUiInCaps uidomain uidomain caps)

class Injectable (f :: * -> *) (l :: * -> *) where
  -- type Injected l :: * -> *
  inject :: f a -> l a

instance (ListMember f l) => Injectable f (Sum l) where
  -- type Injected (Sum l) = Sum l
  inject :: forall  a. f a -> (Sum l) a
  inject f = go (listIx :: ListIx l f)
    where
      go :: forall l' . ListIx l' f -> Sum l' a
      go ListIxHead = SumHere f
      go (ListIxTail l) = SumThere (go l)

class Projectable (a :: k) l where
  type Projected l a :: *
  project :: Proxy a -> l -> Projected l a

instance (ListMember a l) => Projectable (a :: *) (HList (l :: [*])) where
  type Projected (HList l) a = a
  project Proxy l = go l (listIx :: ListIx l a)
    where
      go :: forall (l' :: [*]). HList l' -> ListIx l' a -> a
      go (HCons _ l') (ListIxTail ix) = go l' ix
      go (HCons x _) ListIxHead = x
      go HNil _ = error "truly impossible"

instance (ListMember f l) => Projectable f (Sum l a) where
  type Projected (Sum l a) f = Maybe (f a)
  project Proxy l = go l (listIx :: ListIx l f)
    where
      go :: forall l'. Sum l' a -> ListIx l' f -> Maybe (f a)
      go (SumThere l') (ListIxTail ix) = go l' ix
      go (SumHere x) ListIxHead = Just x
      go _ _ = Nothing

type family AllUiInCaps
  (uidomainInd :: [* -> *])
  (uidomain :: [* -> *])
  (caps :: [* -> Constraint])
  :: Constraint where
  AllUiInCaps (ui ': uidom) uidomain caps = (ListMember ui uidomain, ListMember (Ui ui) caps, AllUiInCaps uidom uidomain caps)
  AllUiInCaps '[] uidomain caps = ()

type family MapMaybeDict (fs :: [* -> Constraint]) (x :: *) :: [*] where
  MapMaybeDict (f ': l) x = Maybe (Dict (f x)) ': MapMaybeDict l x
  MapMaybeDict '[]      x = '[]

data Sum (l :: [* -> *]) a where
  SumHere  :: f a -> Sum (f ': l) a
  SumThere :: Sum l a -> Sum (f ': l) a

-- Proof that an 'a' is a member of list 'l'
data ListIx (l :: [k]) (a :: k) where
  ListIxHead :: ListIx (a ': l) a
  ListIxTail :: ListIx l a -> ListIx (b ': l) a

deriving instance Show (ListIx caps c)

class ListMember (x :: k) (l :: [k]) where
  listIx :: ListIx l x

instance {-# OVERLAPS #-} ListMember c (c ': cs) where
  listIx = ListIxHead
  
instance {-# OVERLAPPABLE #-} ListMember c cs => ListMember c (c' ': cs) where
  listIx = ListIxTail listIx

type family FilterUiDomain
   (caps :: [* -> Constraint]) :: [* -> *] where
  FilterUiDomain ((Ui ui) ': cs) = ui ': (FilterUiDomain cs)
  FilterUiDomain (x ': cs) = FilterUiDomain cs
  FilterUiDomain '[]  = '[]

-- Live HieValues are rendered by means of the Ui-capabilities they implement.
-- A Ui capability can specify reactive dependencies to other values, and they
-- are capable of producing new HieValues. It's up to the containing HieValue to
-- decide what to do about newly produced values.
class Ui (ui :: * -> *) (a :: *) where
  ui ::
    (
      ListMember ui uidomain
    , UiDomain uidomain caps
    , MonadWidget t m
    ) =>
    ui (Term (Sum uidomain)) ->
    a ->
    Capabilities caps a ->
    Dynamic t (M.Map HieRef (Dynamic t (HieValue caps))) ->
    m (ReactiveHieValue t caps uidomain)

-- | A 'ReactiveHieValue' captures the reactive outputs of a live hie value,
-- that is, a HieValue that is connected to the rest of the document/session
-- through the 'ui' method.
data ReactiveHieValue t caps uidomain =
  ReactiveHieValue {
    rhvCreateValues :: Event t (HieValue caps),
    -- ^ Should the Ui of a HieValue wish to create new HieValues, it does so
    -- through this event.
    rhvUiState      :: Behavior t (Term (Sum uidomain))
    -- ^ The Ui should maintain its state in this behavior, in the event that
    -- other values want to read it (eg. in the event of the document being
    -- serialized)
    }

-- Hie Values. The values that can be shown in the user interface. It is
-- parametrised by the universe of capabilies (that is, witnesses of type class
-- memberships) that the user interface may exploit.

-- Examples of capabilites include the types of Ui that may be used to interact
-- with the value,

-- Capabilities are statically tracked at the point where a value is admitted to
-- the user interface.
data HieValue (caps :: [* -> Constraint]) =
  forall a. HieValue
  {
    hieVal :: a
  , hieUi  ::
      -- TODO: Is it justified to keep the selected ui as a built in part of a
      -- HieValue? Would it maybe be better if it were simply a part of the
      -- dynamic state in a Hie document? Then keeping track of the currently
      -- selected ui for a HieValue and its state becomes the responsibility of
      -- the containing HieValue. Hm. Interesting...
      forall uidomain.
      UiDomain uidomain caps =>
      Maybe (Term (Sum uidomain))
  , hieCapabilites :: Capabilities caps a
  }

valueMaybeCaps :: 
  forall caps a uidomain.
  (MaybeCaps caps a, UiDomain uidomain caps) =>
  Proxy caps -> a -> Maybe (Term (Sum uidomain)) -> HieValue caps
valueMaybeCaps Proxy x ui' = HieValue x ui' (maybeCaps (Proxy :: Proxy caps) (Proxy :: Proxy a))

value ::
  forall a uidomain caps.
  UiDomain uidomain caps =>
  a -> Maybe (Term (Sum uidomain)) -> Capabilities caps a -> HieValue caps
value = HieValue

valueNoUi :: forall a caps. a -> Capabilities caps a -> HieValue caps
valueNoUi x caps = HieValue x Nothing caps

-- Ui instances

data RichText e = RichText

-- smart constructor
uiRichText ::
  forall uidomain. ListMember RichText uidomain =>
  Term (Sum uidomain)
uiRichText = Term $ (inject RichText)

data MarkDown =
  MDHeading String |
  MDList [MarkDown] |
  MDParagraph String

instance Ui RichText MarkDown where
  ui u@RichText md _ _ = do
    renderMD md
    return $ ReactiveHieValue never (pure $ Term $ inject u)
    where
      renderMD :: (MonadWidget t m) => MarkDown -> m ()
      renderMD (MDHeading h) = el "h1" $ text h
      renderMD (MDParagraph p) = el "p" $ text p
      renderMD (MDList l) = el "ul" $ mapM_ renderMD l

instance MaybeInstance (Ui RichText) MarkDown where

data ShowString e = ShowString

uiShowString :: ListMember ShowString uidomain => Term (Sum uidomain)
uiShowString = Term $ inject ShowString

instance Show a => Ui ShowString a where
  ui u@ShowString x _ _ = do
    text $ show x
    return $ ReactiveHieValue never (pure (Term $ inject u))

data ReactiveFunc e = ReactiveFunc HieRef e

uiReactiveFunc ::
  ListMember ReactiveFunc uidomain =>
  HieRef ->
  Term (Sum uidomain) ->
  Term (Sum uidomain)
uiReactiveFunc = undefined

instance Ui ReactiveFunc (a -> b) where
  ui u@(ReactiveFunc _ _) _ _ _ = do
    text $ "todo..."
    return $ ReactiveHieValue never (pure (Term $ inject u))

-- UI that presents the values of a Hie Document as freely floating windows
data FloatingWindows e = FloatingWindows

uiFloatingWindows :: ListMember FloatingWindows uidomain => Term (Sum uidomain)
uiFloatingWindows = Term $ inject FloatingWindows

-- UI that presents the values of a Hie Document as a textual flow of boxes,
-- like in HTML.
data FlowingBoxes e = FlowingBoxes

uiFlowingBoxes :: ListMember FlowingBoxes uidomain => Term (Sum uidomain)
uiFlowingBoxes = Term $ inject FlowingBoxes

-- UI that lays out the values of a Hie Document in a grid
data Grid e = Grid

uiGrid :: ListMember Grid uidomain => Term (Sum uidomain)
uiGrid = undefined
