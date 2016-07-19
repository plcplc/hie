-- in the browser, evaluate: "Haskell.log.enable(true)" to see debugging info..
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A graphical frontend for a Hie Session, using the library
-- 'threepenny-gui' for rendering.
module Graphics.HIE.ThreePenny where

import Graphics.HIE.Session

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Dynamic
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified Graphics.UI.Threepenny as TP
import Graphics.UI.Threepenny.Core as TP hiding (UI, value)
import qualified Reactive.Threepenny as Reactive

type TPUI = TP.UI

data ValueWidget = ValueWidget {
  vwElement :: Element
  }

instance Widget ValueWidget where
  getElement = vwElement

data UIWidget = UIWidget {
  uiElement :: Element
  }
  
instance Widget UIWidget where
  getElement = uiElement

data TPSession = TPSession {
  tpAddEvent    :: Event Identifier,
  tpUpdateEvent :: Event Identifier,
  tpRemoveEvent :: Event Identifier,
  tpCurrentBindings :: Behavior (M.Map Identifier (Dynamic, UI))
  }
 
-- | Wire a 'Session' to ThreePenny by starting listener threads on
-- the channels in the session. When a client disconnects, the listener
-- threads are killed.
tpSession :: Session -> TPUI TPSession
tpSession session = do

  addEvent <- liftTChanEvent (sessionAddBindingEvents session)
  updateEvent <- liftTChanEvent (sessionUpdateBindingEvents session)
  removeEvent <- liftTChanEvent (sessionRemoveBindingEvents session)

  currentBindings <- liftIO $ atomically $ sessionCurrentBindings session
  bindingsB <- accumB currentBindings $
    concatenate <$> unions
    [
      (\(ident, value, ui) -> M.insert ident (value, ui)) <$> addEvent,
      (\(ident, value, ui) -> M.insert ident (value, ui)) <$> updateEvent,
      M.delete <$> removeEvent
    ]

  let getIdent = ((\(i,_,_) -> i) <$>)

  return $ TPSession (getIdent addEvent) (getIdent updateEvent) removeEvent bindingsB

  where

    -- lift a 'TChan' to an 'Event', by reading in a concurrent thread.
    liftTChanEvent :: TChan a -> TPUI (Event a)
    liftTChanEvent ch = do

      (event, eventThread) <- liftIO $ do
        eventThreadReady <- newEmptyMVar
        (event, fireEvent) <- newEvent

        eventThread <- forkIO $ do
          ch' <- atomically $ dupTChan ch
          putMVar eventThreadReady ()
          forever $ do
            newOccurrence <- atomically (readTChan ch')
            fireEvent newOccurrence

        takeMVar eventThreadReady
        return (event, eventThread)

      w <- askWindow
      on disconnect w $ \_ -> liftIO $ do
        killThread eventThread

      return event

bindingValueBehavior :: TPSession -> Identifier -> Behavior (Maybe (Dynamic, UI))
bindingValueBehavior (TPSession{..}) identifier = (M.lookup identifier) <$> tpCurrentBindings

tpMain :: IO ()
tpMain = do
  -- Let's build a cute toy session.
  -- We still need a proper way to way register values that are already present..
  hieSession <- newEmptySession
  atomically $ do
    sessionAddBinding hieSession "foo" (toDyn (42 :: Int)) ShowString
    sessionAddBinding hieSession "bar" (toDyn ([42,43] :: [Int])) ShowString
    -- This should display an error
    sessionAddBinding hieSession "baz" (toDyn (id :: Int -> Int)) ShowString

  startGUI defaultConfig (clientMain hieSession)

clientMain :: Session -> Window -> TPUI ()
clientMain hieSession w = void $ mdo
  session <- tpSession hieSession

  getBody w #+ [TP.h1 # set text "HIE Session"]

  let addValueWidget identifier = runUI w $ do
        --vW <- valueWidget identifier (valueBehavior session identifier)
        valueW <- maybeWidget
          (bindingValueBehavior session identifier)
          (valueWidget identifier)
        getBody w #+ [ element valueW ]
        return ()

  -- Add the values initially present in the session.
  liftIO $ do
    currentBindings <- currentValue (M.keys <$> tpCurrentBindings session)
    mapM_ addValueWidget currentBindings

  -- Whenever an 'add'-event occurs, append a representative element to the body.
  liftIO $ register (tpAddEvent session) addValueWidget

  -- Next up: Overvej om det er værd at fortsætte med threepenny.
-- | Show a widget when a behavior is 'Just'.
maybeWidget :: Widget w => Behavior (Maybe a) -> (Behavior a -> TPUI w) -> TPUI Element
maybeWidget maybeBehavior widgetConstructor = do
  window <- askWindow
  containerElm <- TP.div
  (isJustEv, fireIsJust) <- liftIO newEvent

  liftIO $ Reactive.onChange maybeBehavior $
    \maybeValue -> do
      case maybeValue of
        Nothing -> fireIsJust False
        Just _  -> fireIsJust True

  

  {-
  (valueChangedEv, fireValueChanged) <- liftIO newEvent
  valueBehaviorTVar <- liftIO $ newTVarIO Nothing
  widgetTVar <- liftIO $ newTVarIO Nothing

  -- TODO: similar for currentvalue?
  liftIO $ Reactive.onChange maybeBehavior $
    \maybeValue -> runUI window $
      case maybeValue of
      Nothing -> do
        -- When the value has disappeared, remove the element
        return containerElm # set children []
        return ()
      Just value -> do
        maybeValueBehavior <- liftIO $ readTVarIO valueBehaviorTVar
        valueBehavior <- case maybeValueBehavior of
          -- construct the behavior if it isn't already
          Nothing -> do 
            valueBehavior <- stepper value valueChangedEv
            liftIO $ atomically $ writeTVar valueBehaviorTVar (Just valueBehavior)
            return valueBehavior
          Just valueBehavior -> return valueBehavior
        maybeWidget' <- liftIO $ readTVarIO widgetTVar
        w <- case maybeWidget' of
          -- construct the widget if it isn't already
          Nothing -> do
            w <- widgetConstructor valueBehavior
            liftIO $ atomically $ writeTVar widgetTVar (Just w)
            return w
          Just w -> return w
        return containerElm # set children [ getElement w ]
        liftIO $ fireValueChanged value
  -}

  return containerElm

-- | This widget represents a bound value that is present in the session.
valueWidget :: Identifier -> Behavior (Dynamic, UI) -> TPUI ValueWidget
valueWidget (Identifier name) bindingB = mdo

  nameElm <- TP.span  # set text (T.unpack name)

  uiElm <- uiWidget bindingB

  e <- TP.div # set style
    [ ("background", "#E0E0E0")
    , ("padding", "5px")
    , ("margin", "5px")
    ]
    #+
    [ element nameElm
    , element uiElm
    ]

  w <- askWindow
  liftIO $ Reactive.onChange bindingB $ \v -> do runUI w $ debug (show v)

  return $ ValueWidget e

-- Next up: define this function
uiWidget :: Behavior (Dynamic, UI) -> TPUI UIWidget
uiWidget = do
  undefined
