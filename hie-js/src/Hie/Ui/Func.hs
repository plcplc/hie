{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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

module Hie.Ui.Func where

import Data.Comp
import Data.Dynamic.PolyDyn
import Hie.Ui.Types
import Reflex.Dom
import qualified Data.Map.Lazy as LM

uiFuncImpl ::
  forall uidomain t m.
  (UiFunc :<: uidomain, MonadWidget t m, Reflex t) =>
  UiImpl uidomain t m
uiFuncImpl = UiImpl go
  where

    go :: UiImplK t uidomain m UiFunc (FreeVar "a" -> FreeVar "b")
    go lookupHieVal _ (UiFunc _ resUi) fPmDyn = mdo

      -- Designating an argument to apply the function on:

      -- If 'argumentInput' identifies a value of a type compatible
      -- with 'FreeVar "a"', 'argumentInput' gets a light green
      -- background.
      
      -- NOTE: It seems that the WebKitGTK backend occasionally
      -- segfaults when we use 'textInput'. At least it does so on my
      -- machine.
      argumentInputConfig <-
            (\tic -> tic{
              _textInputConfig_attributes = argumentInputStyleDyn,
              _textInputConfig_setValue = ("" <$ applyAndBindEvent)
            }) <$> defTextInputConfig

      argumentInput <- el "div" $ do
        el "span" (text "argument:")
        textInput argumentInputConfig

      argumentDyn <- joinDyn <$> mapDyn lookupHieVal (value argumentInput)
      argumentApplicationDyn <- mapDynM
        ( \hvMay ->
            sample (current fPmDyn) >>= \fPm -> return $ do
            hv <- hvMay
            return $ runPolyM $ do
              f' <- unboxMatch fPm
              x  <- unbox $ hieValue hv
              box $ f' x
        )
        argumentDyn

      argumentInputStyleDyn <- mapDyn
        (\(argText, arg) -> LM.singleton "style" $
            case (argText, arg) of
              ("", _) -> "background: white;"
              (_, Nothing) -> "background: rgb(255, 153, 102);"
              (_, Just (Left _)) ->  "background: rgb(255, 51, 0);"
              (_, Just (Right _)) ->  "background: rgb(51, 204, 51);"
              )
        =<< collectDyn (value argumentInput, argumentApplicationDyn)

      -- Designating a name to bind the result of the application with

      targetNameInput <- el "div" $ do
        el "span" (text "target:")
        cfg <- (\tic -> tic
                 { _textInputConfig_setValue = "" <$ applyAndBindEvent })
               <$> defTextInputConfig
        textInput cfg

      (applyBtn, _) <- elDynAttr' "button" applyBtnAttrsDyn (text "apply and bind")

      applyGateDyn <- mapDyn 
        (\(appResult, target) -> 
          case (appResult, target) of
            (_, "") -> True
            (Just (Right _), _) -> False
            (_, _) -> True
        )
        =<< collectDyn (argumentApplicationDyn, value targetNameInput)

      applyBtnAttrsDyn <- mapDyn
        (\c -> 
          case c of
            True -> LM.singleton "disabled" "true"
            False -> LM.empty
        ) applyGateDyn
      
      -- Commanding the function application be carried out.

      let applyAndBindEvent
            = flip push (domEvent Click applyBtn)
           (\_ -> do
               appResult <- sample $ current argumentApplicationDyn
               target <- sample $ current $ value targetNameInput
               case (appResult, target) of
                 (_, "") -> return Nothing
                 (Just (Right x), t) ->
                   return $ Just $ LM.singleton t (Just $ HieValue x resUi)
                 (_, _) -> return Nothing
           )

      return (HieSessionActions {
               updateBindings = applyAndBindEvent
                                }, never)

-- For some reason ghc has trouble inferring 't' when defining a
-- TextInputConfig.
defTextInputConfig :: (MonadWidget t m, Reflex t) => m (TextInputConfig t)
defTextInputConfig = return def
