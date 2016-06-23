{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.HIE.Gtk where

import Control.Monad.Reader
import qualified Graphics.UI.Gtk as G
import qualified Data.Map.Strict as M

import Control.Concurrent.STM

import Graphics.HIE

data UIState = UIState {
  uiHieValues :: HieValues,
  uiWindows :: M.Map Name G.Window
}

type HieGtkM m = (MonadReader (TVar UIState) m, MonadIO m)

-- De her skal laves om til free-monad fortolkere... Eller noget lignende..
labelGtk :: HieGtkM m => Name -> HieE -> G.Window -> m ()
labelGtk name (_) w = do
  l <- liftIO $ G.labelNew (Just $ name)
  liftIO $ G.containerAdd w l

buttonGtk :: HieGtkM m => Name -> HieE -> G.Window -> m ()
buttonGtk = error "Button not implemented"

functionGtk :: HieGtkM m => UI -> Name -> HieE -> G.Window -> m ()
functionGtk resultUI n _ w = do
  (appVarEntry, appBtn) <- liftIO $ do
    vbox <- G.vBoxNew True 10
    l <- G.labelNew (Just n)
    G.boxPackStart vbox l G.PackGrow 0

    hbox <- G.hBoxNew False 5
    appVarEntry <- G.entryNew
    appBtn <- G.buttonNewWithLabel "Apply"

    G.boxPackStart hbox appVarEntry G.PackNatural 0
    G.boxPackStart hbox appBtn G.PackNatural 0

    G.boxPackStart vbox hbox G.PackNatural 0

    G.containerAdd w vbox

    return (appVarEntry, appBtn)

  -- Når knappen appBtn trykkes ind, læs da et variabelnavn fra appVar, og
  -- evaluer "Application n appVar-text", og dan en ny hieValue med et friskt
  -- navn.
  s <- ask
  _ <- liftIO $ G.on appBtn G.buttonActivated (flip runGtkM s $ do
    appVar <- liftIO $ G.entryGetText appVarEntry
    let ex1 = Application (Variable n) (Variable appVar)
    resVar <- freshVar appVar

    bind resVar ex1
    bindUI resVar resultUI
    showUI resVar
    )

  return ()

freshVar :: HieGtkM m => Name -> m Name
freshVar = return . ("#" ++) -- TODO

{- TODO:
eval :: HieGtkM m => HieE -> m HieE
eval e1 = do
  env <- getSts (hieValues . uiHieValues)
  let value = runReaderT (evaluateHieE e1) env
  -}

bind :: HieGtkM m => Name -> HieE -> m ()
bind var val = do
  values <- getSts (hieValues . uiHieValues)
  let values' = M.insert var val values
  modifySt (\s -> s{uiHieValues = (uiHieValues s){hieValues = values' }})

showUI :: HieGtkM m => Name -> m ()
showUI n = do
  ws <- getSts uiWindows
  let Just w = M.lookup n ws
  liftIO $ G.widgetShowAll w

interpUI :: HieGtkM m => UI -> Name -> HieE -> G.Window -> m ()
interpUI ShowString = labelGtk
interpUI Button = buttonGtk
interpUI (Replacing (Function _)) = labelGtk
interpUI (Function resUI) = functionGtk resUI
interpUI v = error $ "interpUI: unhandled case for " ++ show v

hieGtkMain :: HieSession -> IO ()
hieGtkMain :: HieValues -> IO ()
hieGtkMain values = do
  initialSt <- newTVarIO UIState{
    uiHieValues = values,
    uiWindows = M.empty
  }
  runReaderT go initialSt

  where
    go = do
      lift $ mapM_ putStrLn =<< G.initGUI
      --runGtkM (initializeUi values) initialSt
      initializeUi
      lift $ G.mainGUI

runGtkM :: ReaderT (TVar UIState) IO a -> TVar UIState -> IO a
runGtkM = runReaderT

initializeUi :: HieGtkM m => m ()
initializeUi = do
  -- Create windows for the values that have been designated UIs.
  mapM_
    (\ (name, ui) -> bindUI name ui)
    =<< getSts (M.assocs . hieUi . uiHieValues)

  showAllValues

bindUI :: HieGtkM m => Name -> UI -> m ()
bindUI name ui = do
    w <- liftIO $ G.windowNew
    -- build the ui for the window
    Just v <- getSts (M.lookup name . hieValues . uiHieValues)
    interpUI ui name v w
    modifySt (\uiSt -> uiSt{
      uiWindows = M.insert name w (uiWindows uiSt)
      })

showAllValues :: HieGtkM m => m ()
showAllValues =
  ((mapM_ (liftIO . G.widgetShowAll)) . M.elems) =<< getSts uiWindows

getSts :: HieGtkM m => (UIState -> a) -> m a
getSts f = do
  s <- ask
  f <$> (liftIO $ atomically $ readTVar s)

modifySt :: HieGtkM m => (UIState -> UIState) -> m ()
modifySt f = do
  s <- ask
  liftIO $ atomically $ modifyTVar s f
