{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- This module defines the Hie Session object, which the ui frontends use to
-- interact with the state of a single hie session.
module Graphics.HIE.Session
  (
  Document,
  Module,
  UIHandler,
  mkDocument,
  SimpleSessionM(..),
  newSimpleSession,
  runSimpleSession,
  MonadSession(..),
  UIAction(..)
  ) where

import Control.Concurrent.STM
import Control.Monad.Reader


import Graphics.HIE.AST

-- The type class that ui handlers should be written for.
class (Monad m) => MonadSession m where
  bind     :: Name -> Exp -> UI -> m ()
  lookupBinding :: Name -> m (Maybe (UI, Exp))
  evaluate :: Exp -> m EExp
  document :: m Document

{-

instance
  (MonadReader (TVar (SessionState m)) m, MonadIO m)
  => MonadSession m where

data SessionState m = SessionState {
  sessionDoc :: Document,
  sessionUIHandler :: UIHandler m
  }

newSessionState :: MonadSession m =>
  Document ->
  IO (TVar (SessionState m))
newSessionState doc ui =
  newTVarIO (SessionState {
    sessionDoc = doc,
    sessionUIHandler = ui
    })

type SimpleState m = TVar (SessionState (SimpleSessionM m))

newtype SimpleSessionM m a = SSM {
  unSSM :: ReaderT (SimpleState m) m a
  }
  deriving (
    Applicative,
    Functor,
    MonadReader (SimpleState m),
    Monad
  )

runSimpleSession ::
  SimpleState m ->
  SimpleSessionM m a ->
  m a
runSimpleSession st m = runReaderT (unSSM m) st
-}

{-

-- As an alternative to SimpleSessionM I have considered employing Control.Eff:

import Control.Eff

data SessionCommand =
  SessionBind Name Exp |
  SessionEvaluate Exp |
  SessionDocument |
  SessionSetUI Name UI |
  SessionTerminate

data Session a = Session SessionCommand a

instance (Member Session r) => MonadSession (Eff r) where

spawnSession :: MonadSession m => Document -> UIHandler m -> m ()
spawnSession = error "spawnSession: Not implemented yet"

-}
