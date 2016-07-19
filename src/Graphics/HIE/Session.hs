{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- This module defines the Hie Session object, which the ui frontends use to
-- interact with the state of a single hie session.
module Graphics.HIE.Session
  (
    Identifier(..),
    Session,
    UI(..),

    newEmptySession,

    sessionCurrentBindings,

    sessionAddBinding,
    sessionAddBindingEvents,

    sessionRemoveBinding,
    sessionRemoveBindingEvents,

    sessionUpdateBinding,
    sessionUpdateBindingValue,
    sessionUpdateBindingUI,
    sessionUpdateBindingEvents,
  ) where

import Control.Monad
import Control.Concurrent.STM
import Data.Maybe
import Data.Map.Strict as M
import Data.Text as Text
import Data.Dynamic
import Data.String

newtype Identifier = Identifier Text
  deriving (Eq, Ord, IsString)


data Session = Session {
  sessionMap                 :: TVar (M.Map Identifier (Dynamic, UI)),
  sessionAddBindingEvents    :: TChan (Identifier, Dynamic, UI),
  sessionUpdateBindingEvents :: TChan (Identifier, Dynamic, UI),
  sessionRemoveBindingEvents :: TChan Identifier
  }

-- | This should be some sort of extensible union. (Not all combinations
-- necessarily make sense.)
data UI =
  ShowString |
  VerticalList UI |
  Columns [UI] |
  Date
  deriving Show

newEmptySession :: IO (Session)
newEmptySession =
  Session <$>
    newTVarIO (M.empty) <*>
    newBroadcastTChanIO <*>
    newBroadcastTChanIO <*>
    newBroadcastTChanIO

sessionCurrentBindings :: Session -> STM (M.Map Identifier (Dynamic, UI))
sessionCurrentBindings (Session{..}) = readTVar sessionMap

sessionAddBinding :: Session -> Identifier -> Dynamic -> UI -> STM ()
sessionAddBinding (Session{..}) identifier value ui = void $
  do
    store <- readTVar sessionMap
    let (oldVal, store') = M.insertLookupWithKey (\_ _ v -> v) identifier (value, ui) store
    (if isJust oldVal
      then writeTChan sessionUpdateBindingEvents
      else writeTChan sessionAddBindingEvents)
      (identifier, value, ui)
    writeTVar sessionMap store'

sessionRemoveBinding :: Session -> Identifier -> STM ()
sessionRemoveBinding (Session{..}) identifier = void $ do
  do
    store <- readTVar sessionMap
    -- What if the element is not present in the store?
    let store' = M.delete identifier store
    writeTChan sessionRemoveBindingEvents identifier
    writeTVar sessionMap store'

sessionUpdateBinding :: Session -> Identifier -> (Dynamic, UI) -> STM ()
sessionUpdateBinding = undefined

sessionUpdateBindingValue :: Session -> Identifier -> Dynamic -> STM ()
sessionUpdateBindingValue = undefined

sessionUpdateBindingUI :: Session -> Identifier -> UI -> STM ()
sessionUpdateBindingUI = undefined

  {-
sessionAddBindingEvents    :: Session -> STM (TChan (Identifier, Dynamic, UI))
sessionAddBindingEvents (Session{..}) = dupTChan sessionAddBindingEvents'
 
sessionRemoveBindingEvents :: Session -> STM (TChan Identifier)
sessionRemoveBindingEvents (Session{..}) = dupTChan sessionRemoveBindingEvents'

sessionUpdateBindingEvents :: Session -> STM (TChan (Identifier, Dynamic, UI))
sessionUpdateBindingEvents (Session{..}) = dupTChan sessionUpdateBindingEvents'
-}
