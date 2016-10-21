{-# LANGUAGE DataKinds #-}
module Main where

import Hie.Ui.DocumentContainer
import Hie.Ui.Types
import Data.Proxy
import Lib

type Caps =
  '[Ui FlowingBoxes,
    Ui RichText,
    Ui ShowString,
    Ui ReactiveFunc]

pcaps :: Proxy Caps
pcaps = Proxy

main :: IO ()
main = mainReactiveSum

mainSingleValue :: IO ()
mainSingleValue = hieJsMain $ do
  let txt1 = valueMaybeCaps pcaps
               (MDHeading "A single HieValue.")
               (Just uiRichText)
  txt1

mainReactiveSum :: IO ()
mainReactiveSum = hieJsMain $ do
  let
    txt1 = valueMaybeCaps pcaps
           (MDHeading "A sampling of features in a HIE document")
           (Just uiRichText)
    txt1Ref = HieRef 0
    txt2 = valueMaybeCaps pcaps
           (MDParagraph "Two constant Int values, and a derived value. Changing\
           \ the input updates the derived value.")
           (Just uiRichText)
    txt2Ref = HieRef 0
    x = valueMaybeCaps pcaps
        (32 :: Int)
        (Just uiShowString)
    xRef = HieRef 1
    y = valueMaybeCaps pcaps
        (12 :: Int)
        (Just uiShowString)
    yRef = HieRef 2
    -- Would it ever be possible to actually support '(Num a) => a -> a -> a' ?
    -- -> Yes, with "Num" in capabilities, together with wrapped "+" that
    -- -> somehow lifts or selects the "Num" constraint for the Ui instance to
    -- -> satisfy when executing function application.
    sum' = valueMaybeCaps pcaps
           ((+) :: Int -> Int -> Int)
           (Just $ uiReactiveFunc xRef (uiReactiveFunc yRef uiShowString))
    sum'Ref = HieRef 3
    doc = documentContainer uiFlowingBoxes
          [
            (txt1Ref, txt1),
            (txt2Ref, txt2),
            (xRef, x),
            (yRef, y),
            (sum'Ref, sum')
          ]
  doc
