{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
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

module Hie.Ui.List where

import Data.Comp
import Data.Comp.Show
import Data.Dynamic.PolyDyn
import Data.Proxy
import Data.Typeable
import Hie.Ui.Types
import Reflex.Dom
import qualified Data.List.NonEmpty as NE

data UiList e = UiList e
  deriving (Eq, Functor, Foldable, Traversable)

instance EqF UiList where
  eqF _ _ = True

instance ShowF UiList where
  showF _ = "UiList"

instance UiSelectable UiList where

  uiIdentifier _ = "List"
  enumerateUi = [UiList ()]

uiList :: (UiList :<: uidomain) => Term uidomain -> Term uidomain
uiList x = Term $ inj (UiList x)

uiListImpl ::
  forall t m uidomain.
  (Monad m, MonadWidget t m) =>
  (UiList :<: uidomain) =>
  UiImpl uidomain t m
uiListImpl = UiImpl go
  where
    go :: UiImplK t uidomain m UiList [FreeVar "a"]
    go lookupHieVal lookupUiEnv (UiList innerUi) xsDyn = do

      pdListDyn <- mapDyn unMatchT xsDyn  
      
      -- Map each element to a div.
      innerResDyn <- simpleList pdListDyn
        (\pdDyn -> do
          pd <- sample (current $ pdDyn)
          -- TODO: abstract this 'lookupUiEnv' call.
          case lookupUiEnv innerUi (pd, updated pdDyn) lookupHieVal of
            Nothing -> el "div" $ do
              text "Designated UI is nonexistant or incompatible"
              return (noAction, never)
            Just m -> el "div" m
        )

      -- To allow changes made by the ui of the elements of the list to propagate,
      -- wire up their change-events to those of this list.
      -- TODO: Test this. Shouldn't we rather use Dynamic to ensure
      -- instantaneous propagation?
      let innerRes = unzip <$> current innerResDyn
      let innerSesActs =
            HieSessionActions $ switch $
            updateBindings <$> (mconcat . fst) <$> innerRes
      let innerEvs = switch $ dynListEvents (current pdListDyn) . snd <$> innerRes

      let polyInnerEvs =
            (\l -> runPolyM
              ((do
                 -- Note that this ["a"] is not tied to that of the
                 -- signature of 'uiListImpl'.
                 pVals :: [PolyVal s "a"] <- mapM unbox l
                 pvsDyn <- box pVals
                 polyMatch' pvsDyn (Proxy :: Proxy [FreeVar "a"])
              ) :: forall s. Typeable s => PolyM s (PolyMatch [FreeVar "a"])))
            <$> innerEvs
      let polyInnerEvs' = fmapMaybe (either (const Nothing) Just) polyInnerEvs
      let polyInnerErrorEvs' = fmapMaybe (either Just (const Nothing)) polyInnerEvs

      -- TODO: Do something more sensible than this error handling........
      errorTextDyn <- holdDyn "" (renderPolyError <$> polyInnerErrorEvs')
      el "div" $ dynText errorTextDyn

      return (innerSesActs, polyInnerEvs')

-- Given a behavior of a list ('bl') and a list of events ('es'), construct an
-- event of lists which occurs whenever an event with index 'ix' in 'es'
-- occurs. The value of the resulting event is that of 'bl' with the element at
-- index 'ix' substituted for the value of the occuring event.
--
-- Note that no guarantees are given with respect to the coherency of 'bl' and
-- 'es'. Events with no corresponding index 'bl' are silently dropped.
dynListEvents ::
  forall t a.
  Reflex t =>
  Behavior t [a] ->
  [Event t a] ->
  Event t [a]
dynListEvents bl es = attachWith (foldr updateList) bl listIxes
  where

    updateList :: (Int, a) -> [a]-> [a]
    updateList (0, x) (_:ls) = x:ls
    updateList (n, x) (l:ls) = l : updateList (pred n, x) ls
    updateList _      [] = []

    listIxes :: Event t (NE.NonEmpty (Int, a))
    listIxes = NE.zip (0 NE.:| [1..]) <$> mergeList es
