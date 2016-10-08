{-# LANGUAGE DataKinds #-}
module Main where

--import Hie.Ui.DocumentContainer
import Hie.Ui.Types
--import Reflex.Dom hiding (value)
--import qualified Data.Map as M
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
main = mainSingleValue

mainSingleValue :: IO ()
mainSingleValue = hieJsMain pcaps $ do
  let txt1 = valueMaybeCaps
               (MDHeading "A single HieValue.")
               (Just uiRichText)
  txt1

  {-
mainReactiveSum :: IO ()
mainReactiveSum = hieJsMain caps $ do
  let
    txt1 = valueMaybeCaps
           (MDHeading "A sampling of features in a HIE document")
           (Just uiRichText)
    txtRef = HieRef 0
    x = valueMaybeCaps
        (32 :: Int)
        (Just uiShowString)
    xRef = HieRef 1
    y = valueMaybeCaps
        (12 :: Int)
        (Just uiShowString)
    yRef = HieRef 2
    -- Would it ever be possible to actually support '(Num a) => a -> a -> a' ?
    -- -> Yes, with "Num" in capabilities, together with wrapped "+" that
    -- -> somehow lifts or selects the "Num" constraint for the Ui instance to
    -- -> satisfy when executing function application.
    sum' = valueMaybeCaps
           ((+) :: Int -> Int -> Int)
           (Just $ uiReactiveFunc xRef (uiReactiveFunc yRef uiShowString))
    sum'Ref = HieRef 3
    doc = documentContainer uiFlowingBoxes (
            M.fromList
            [
            (txtRef, txt1),
            (xRef, x),
            (yRef, y),
            (sum'Ref, sum')
            ])
  return doc

-}
