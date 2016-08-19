{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Comp
import Data.Dynamic.PolyDyn
import Hie.Session
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
       ("Introduction", Just $ HieValue (polyDyn ("This is a document in an instance of the \"Haskell Interactive Environment\". Like in a REPL-session, names are bound to values of some type. New bindings can be added by applying bound function values interactively, or by manipulating the map of bindings through the API. Polymorphic types are supported by of (unsafely but carefully) monomorphising them to \"FreeVar\". The Ui of a binding can be changed dynamically.")) uiTextLabel),
       ("foo", Just $ HieValue (polyDyn (42 :: Int)) uiTextLabel),
       ("bar", Just $ HieValue (polyDyn ([1,2,3,4,5] :: [Int])) (uiList uiTextLabel)),
       ("map", Just $ HieValue (polyDyn (map :: (FreeVar "a" -> FreeVar "b") -> [FreeVar "a"] -> [FreeVar "b"])) (uiFunc (uiFunc (uiList uiTextLabel)))),
       ("compose", Just $ HieValue (polyDyn ((.) :: (FreeVar "b" -> FreeVar "c") -> (FreeVar "a" -> FreeVar "b") -> FreeVar "a" -> FreeVar "c")) (uiFunc (uiFunc (uiFunc (uiTextLabel))))),
       ("id", Just $ HieValue (polyDyn (id :: FreeVar "a" -> FreeVar "a")) (uiFunc (uiTextLabel))),
       ("plus3", Just $ HieValue (polyDyn ( (+3) :: Int -> Int)) (uiFunc (uiTextLabel)))
     ] <$) <$> getPostBuild
  
  uiEnv' <- uiEnv
  wireSession uiEnv' addBindingEvent
  return ()

type UiDomain = UiList :+: UiTextLabel :+: UiNonShowable :+: UiFunc

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

serializedDocument :: Reflex t => UiSession t uidomain -> m (Behavior t BS.ByteString)
serializedDocument = undefined
