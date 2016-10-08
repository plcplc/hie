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

import Data.Comp.Term
import Hie.Ui.Types
import qualified Data.Map as M

data HieDocumentContainer caps = HieDocumentContainer (M.Map HieRef (HieValue caps))

documentContainer ::
  (UiDomain uidomain caps) =>
  Term (Sum uidomain) ->
  M.Map HieRef (HieValue caps) ->
  HieValue caps
documentContainer ui elements = undefined

-- UI that lays out the values of a Hie Document in a grid
data Grid e = Grid

uiGrid :: ListMember Grid uidomain => Term (Sum uidomain)
uiGrid = undefined

instance Ui FloatingWindows (HieDocumentContainer caps) where
  ui = undefined

instance MaybeInstance (Ui FloatingWindows) (HieDocumentContainer caps) where

instance Ui FlowingBoxes (HieDocumentContainer caps) where
  ui = undefined

instance MaybeInstance (Ui FlowingBoxes) (HieDocumentContainer caps) where

instance Ui Grid (HieDocumentContainer caps) where
  ui = undefined
