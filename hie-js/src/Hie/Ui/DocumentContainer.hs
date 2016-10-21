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

module Hie.Ui.DocumentContainer where

import qualified Data.Map as M
import Control.Monad
import Reflex.Dom
import Data.Comp.Term
import Hie.Ui.Types
import Lib
import Data.Proxy

data HieDocumentContainer caps =
  HieDocumentContainer [(HieRef, HieValue caps)]

-- | The 'documentContainer' is a HieValue, which distinguishing feature is its
-- ability to contain other HieValues. It maintains an ordered set of nested
-- values, and various choices of UI gives rise to different renderings of the
-- contents.
documentContainer ::
  (MaybeCaps caps (HieDocumentContainer caps), UiDomain uidomain caps) =>
  Term (Sum uidomain) ->
  [(HieRef, (HieValue caps))] ->
  HieValue caps
documentContainer ui' elements =
  valueMaybeCaps Proxy (HieDocumentContainer elements) (Just ui')

-- The Ui of 'FlowingBoxes' presents a collection of HieValues as full-width
-- boxes in succession from top to bottom.
instance Ui FlowingBoxes (HieDocumentContainer caps) where
  -- NEXT UP: do 'runHieValueUi' on each of 'values'. The information contained
  -- in the ordering of the elements should really be kept in the 'FlowingBoxes'
  -- datatype. Then it's up to the surrounding ui selection-framework to do
  -- something sensible about not throwing away a carefully designed ui when the
  -- user explores his options....
  {-ui ::
    (
      UiDomain uidomain caps
    , MonadWidget t m
    ) =>
    FlowingBoxes (Term (Sum uidomain))
     -> HieDocumentContainer caps
     -> Capabilities caps (HieDocumentContainer caps)
     -> Dynamic t (M.Map HieRef (Dynamic t (HieValue caps)))
     -> m (ReactiveHieValue t caps uidomain)
  -}
  ui u@FlowingBoxes (HieDocumentContainer values) _ _ = do
    text $ "flowing," ++ show (length values)
    forM_ values $ \(_,v) -> do
      undefined -- runHieValueUi v
    return $ undefined -- ReactiveHieValue never (pure $ Term $ inject u)

instance MaybeInstance (Ui FlowingBoxes) (HieDocumentContainer caps) where

instance Ui FloatingWindows (HieDocumentContainer caps) where
  ui = undefined

instance MaybeInstance (Ui FloatingWindows) (HieDocumentContainer caps) where

instance Ui Grid (HieDocumentContainer caps) where
  ui = undefined


class A a where
  m :: B a b => a -> b -> Int

class B a b where
