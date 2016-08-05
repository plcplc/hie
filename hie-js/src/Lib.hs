{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Comp
import Data.Dynamic.PolyDyn
import Hie.Ui.Types
import Hie.Ui.NonShowable
import Hie.Ui.TextLabel
import Hie.Ui.List
import Hie.Ui.Func
import Hie.Session
import Reflex.Dom
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
              \") $ mdo

  el "h1" (text "HIE Document")

  -- Initial test bindings
  addBindingEvent <-
    (M.fromList
     [
       ("foo", Just $ HieValue (polyDyn (42 :: Int)) uiTextLabel),
       ("bar", Just $ HieValue (polyDyn ([42, 43] :: [Int])) (uiList uiTextLabel)),
       ("id", Just $ HieValue (polyDyn (id :: FreeVar "a" -> FreeVar "a")) (uiFunc "id" (uiTextLabel))),
       ("plus3", Just $ HieValue (polyDyn ( (+3) :: Int -> Int)) (uiFunc "+3" (uiTextLabel)))
     ] <$) <$> getPostBuild
  
  uiEnv' <- uiEnv
  wireSession uiEnv' uiSelector addBindingEvent
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

-- TODO: Generate this ui by means of a type class constraint on 'uidomain'.
uiSelector ::
  (Reflex t, MonadWidget t m) =>
  (UiNonShowable :<: uidomain) =>
  (UiTextLabel :<: uidomain) =>
  (UiList :<: uidomain) =>
  (UiFunc :<: uidomain) =>
  m (Event t (Term uidomain))
uiSelector = do

  (uiListTextLabelBtn, _) <- el' "button" (text "List[TextLabel]")
  (uiTextLabelBtn, _) <- el' "button" (text "TextLabel")
  (uiNonShowableBtn, _) <- el' "button" (text "NonShowable")
  (uiFuncTextLabelBtn, _) <- el' "button" (text "Func[TextLabel]")

  return $ leftmost $
    [
      (uiNonShowable) <$ domEvent Click uiNonShowableBtn,
      (uiTextLabel) <$ domEvent Click uiTextLabelBtn,
      (uiList uiTextLabel) <$ domEvent Click uiListTextLabelBtn,
      (uiFunc "f" uiTextLabel) <$ domEvent Click uiFuncTextLabelBtn
    ]
