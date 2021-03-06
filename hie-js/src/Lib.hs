{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Comp.Term
import Data.Constraint
import Reflex.Dom
import Hie.Ui.Types
import Data.Proxy
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M

hieJsMain ::
  forall uidomain caps.
  (UiDomain uidomain caps) =>
  HieValue caps ->
  IO ()
hieJsMain val = mainWidgetWithCss
  (BS8.pack "") $ do
    _ <- runHieValueUi val
    return ()

runHieValueUi ::
  forall caps t m uidomain.
  (Reflex t, MonadWidget t m, UiDomain uidomain caps) => 
  -- Proxy caps ->
  HieValue caps ->
  m (Maybe (ReactiveHieValue t caps uidomain))
runHieValueUi
  -- pcaps
  HieValue {
    hieVal = val :: a,
    hieUi = uiMaybe,
    hieCapabilites = caps
    } =
  case uiMaybe of
    Nothing -> text "No ui specified" >> return Nothing
    Just (Term uiTerm) ->
      case extractUiCap (Proxy :: Proxy caps) uiTerm caps of
        Nothing -> text "Specified ui not supported (impossible)" >> return Nothing
        Just WUI { wuiV = ui', wuiDict = (Dict :: Dict (ListMember ui uidomain, Ui ui a)) } ->
          Just <$> ui ui' val caps (unsafeDynamic (pure M.empty) never) 

extractUiCap ::
  forall subuidomain uidomain caps a.
  (AllUiInCaps uidomain uidomain caps, AllUiInCaps subuidomain uidomain caps) =>
  Proxy caps ->
  (Sum subuidomain) (Term (Sum uidomain)) ->
  Capabilities caps a ->
  Maybe (WrappedUi uidomain a)
extractUiCap
  pcaps
  (SumThere uiSum) cs
  = extractUiCap pcaps uiSum cs
extractUiCap
  _
  (SumHere (ui' :: ui (Term (Sum uidomain)))) cs
  = do
    Dict <- project'
    return $ WUI ui' Dict

  where

    -- We should use 'Projectable', but it needs a refactoring to be usable it
    -- seems.
    project' :: Maybe (Dict (Ui ui a))
    project'  = lookupCap cs (listIx :: ListIx caps (Ui ui))

    lookupCap :: Capabilities cs a -> ListIx cs (Ui ui) -> Maybe (Dict (Ui ui a))
    lookupCap (HCons d _)  ListIxHead     = d
    lookupCap (HCons _ xs) (ListIxTail t) = lookupCap xs t
    lookupCap HNil         _              = error "Truly impossible"

data WrappedUi uidomain a = forall (ui :: * -> *). WUI {
    wuiV :: ui (Term (Sum uidomain)),
    wuiDict :: Dict (ListMember ui uidomain, Ui ui a)
  } 

  {-
lookupCap ::
  forall (c :: * -> Constraint) caps a.
  ListIx caps c ->
  Capabilities caps a ->
  Maybe (Dict (c a))
lookupCap = undefined
-}

{-
import Data.Comp
import Data.Dynamic.PolyDyn
import Data.Serialize
import Hie.Session
import Hie.Ui.DownloadLink
import Hie.Ui.Func
import Hie.Ui.List
import Hie.Ui.NonShowable
import Hie.Ui.TextLabel
import Hie.Ui.Types
import Reflex.Dom
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M

hieJsMain :: IO ()
hieJsMain = mainWidgetWithCss
             (BS8.pack "button {\
              \  color: black;\
              \  border: 1px solid black;\
              \}\
              \input {\
              \  border: 1px solid black;\
              \}\
              \.bindingWidget {\
              \  background: rgb(240, 240, 240);\
              \}\
              \.bindingWidget:hover .uiSelector {\
              \  display: block;\
              \}\
              \.uiSelector {\
              \  display: none;\
              \  background-color: rgb(240, 240, 240);\
              \  border: 1px solid rgb(100, 100, 150);\
              \  padding: 10px;\
              \  float: right;\
              \}\
              \.uiSelector ul {\
              \  display: block;\
              \  margin: 0px;\
              \  padding: 0px;\
              \}\
              \.uiSelector ul li {\
              \  display: inline;\
              \  font-style: italic;\
              \  font-size: small;\
              \}\
              \.uiSelector ul li:after {\
              \  content: \", \";\
              \}\
              \.uiSelector ul li + li {\
              \  margin-left: 5px;\
              \}\
              \.uiSelector ul li:last-child:after {\
              \  content: \"\";\
              \}\
              \.uiSelector > .uiSelectorStep {\
              \  display: none;\
              \}\
              \.uiSelector:hover > .uiSelectorStep {\
              \  display: block;\
              \}\
              \.uiSelectorStep {\
              \  background-color: rgb(240, 240, 240);\
              \  mix-blend-mode: multiply;\
              \}\
              \.uiSelectorStep > .uiSelectorStep {\
              \  margin-left: 20px;\
              \}\
              \") $ mdo

  el "h1" (text "HIE Document")

  -- Initial test bindings
  addBindingEvent <-
    (M.fromList
     [
       ("Introduction", Just $ HieValue (
         polyDyn
           "This is a document in an instance of the \"Haskell Interactive\
           \Environment\". Like in a REPL-session, names are bound to \
           \values of some type. New bindings can be added by applying \
           \bound function values interactively, or by manipulating the map \
           \of bindings through the API. Polymorphic types are supported \
           \by of (unsafely but carefully) monomorphising them to \"FreeVar\". \
           \The Ui of a binding can be changed dynamically.")
         uiTextLabel),
       ("foo", Just $ HieValue (polyDyn (42 :: Int)) uiTextLabel),
       ("bar", Just $ HieValue (polyDyn ([1,2,3,4,5] :: [Int])) (uiList uiTextLabel)),
       ("map", Just $ HieValue (polyDyn (map :: (FreeVar "a" -> FreeVar "b") -> [FreeVar "a"] -> [FreeVar "b"])) (uiFunc (uiFunc (uiList uiTextLabel)))),
       ("compose", Just $ HieValue (polyDyn ((.) :: (FreeVar "b" -> FreeVar "c") -> (FreeVar "a" -> FreeVar "b") -> FreeVar "a" -> FreeVar "c")) (uiFunc (uiFunc (uiFunc (uiTextLabel))))),
       ("id", Just $ HieValue (polyDyn (id :: FreeVar "a" -> FreeVar "a")) (uiFunc (uiTextLabel))),
       ("plus3", Just $ HieValue (polyDyn ( (+3) :: Int -> Int)) (uiFunc (uiTextLabel))),
       ("download", Just $ HieValue (polyDyn (encode :: Serialize (FreeVar "a") => FreeVar "a" -> BS.ByteString)) (uiFunc uiDownloadLink))
       -- ("downloadDyn", Just $ HieValue (polyDynTC (encode :: Serialize a => a -> BS.ByteString)) (uiDynamic uiDownloadLink))
     ] <$) <$> getPostBuild
  
  uiEnv' <- uiEnv
  wireSession uiEnv' addBindingEvent
  return ()

type UiDomain = UiDownloadLink :+: UiList :+: UiTextLabel :+: UiNonShowable :+: UiFunc

uiEnv :: forall t m . (MonadWidget t m) => m (UiEnv UiDomain t m)
uiEnv = return
  [
    (uiTextLabelImpl (show :: Double -> String)),
    (uiTextLabelImpl (show :: Int -> String)),
    (uiTextLabelImpl (show :: Char -> String)),
    (uiTextLabelImpl (id :: String -> String)),
    uiListImpl,
    uiNonShowableImpl,
    uiFuncImpl
  ]

-}
