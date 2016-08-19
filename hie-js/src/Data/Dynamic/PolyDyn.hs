{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}

module Data.Dynamic.PolyDyn
  (
    -- * Public types
    PolyDyn,
    FreeVar,
    RigidVar,
    PolyVal,
    PolyM,
    PolyError,
    TypeSig(..),
    PolyMatch(..),
    ToPolyVal,
    IsPolyDyn,
    HasTyVars,

    -- * Public functions
    polyDyn,
    polyDynTy,
    unbox,
    box,
    polyMatch,
    polyMatch',
    unboxMatch,
    unMatch,
    unMatchT,
    runPolyM,
    debugPolyM,
    unsafePolyM,
    renderPolyError,
    withPolyVal,
    var,
    castPoly
  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Comp
import Data.Comp.Derive
import Data.Comp.Render
import Data.Comp.Unification
import Data.Comp.Variables
import Data.Constraint
import Data.Proxy
import Data.Tree
import Data.Typeable
import Data.Typeable.Internal
import GHC.TypeLits
import System.IO.Unsafe
import Unsafe.Coerce
import qualified Control.Monad.State.Lazy as LS
import qualified Data.Map as M
import qualified Data.Map.Lazy as LM

-- * PolyDyn

-- Public types

data PolyDyn where
  PolyDyn :: TypeRep -> a -> PolyDyn

polyDynTy :: PolyDyn -> TypeRep
polyDynTy (PolyDyn t _) = t

instance Show PolyDyn where
  show (PolyDyn t _) = "<PolyDyn : " ++ show t ++ ">"

-- | Type variables that unify freely.
data FreeVar (name :: Symbol)

-- | Type variables that only unify with 'FreeVar's.
data RigidVar (name :: Symbol)

-- | Polymorphic values, only available local to the 'PolyM s' monad.
newtype PolyVal (s :: *) (name :: symbol) =
  MkPolyVal { unPolyVal :: forall a. a }
  deriving Typeable

type PolyError = UnifError TypeSig String

newtype PolyM (s :: *) (a :: *) =
  -- Private constructor
  MkPolyM { unPolyM :: UnifyM' a }

instance Functor (PolyM s) where
  fmap f (MkPolyM s) = MkPolyM (fmap f s)

instance Applicative (PolyM s) where
  pure x = MkPolyM (pure x)
  (MkPolyM f) <*> (MkPolyM x) = MkPolyM (f <*> x)

instance Monad (PolyM s) where
  (MkPolyM x) >>= f = MkPolyM (x >>= unPolyM . f)

-- Private types and functions

type UnifyM' = LS.StateT (UnifyState TypeSig String) Except'
type Except' = (ExceptT (UnifError TypeSig String)) LoggerM
type LoggerM = WriterT [PolyMLog] FreshScopeM
type FreshScopeM = StateT (M.Map String String) FreshVarM
type FreshVarM = State Int

data PolyMLog = PMLogRegion [PolyMLog] | PMLogMessage String
  deriving Show

ppPMLog :: PolyMLog -> String
ppPMLog (PMLogMessage msg) = msg
ppPMLog (PMLogRegion logs) =
  unlines $ zipWith (++) (repeat "  ") (concatMap (lines . ppPMLog) logs)

evalUnifyM' :: UnifyM' a -> LoggerM (Either (UnifError TypeSig String) a)
evalUnifyM' act = (runExceptT $ LS.evalStateT act (UnifyState [] LM.empty))

runUnifyM' ::
  UnifyM' a ->
  LoggerM
    (Either (UnifError TypeSig String)
    (a, UnifyState TypeSig String))
runUnifyM' act = runExceptT $ LS.runStateT act (UnifyState [] LM.empty)

runLoggerM :: LoggerM a -> FreshScopeM (a, [PolyMLog])
runLoggerM = runWriterT

logRegion :: forall s a. PolyM s a -> PolyM s a
logRegion act = MkPolyM $ censor ((:[]).PMLogRegion) (unPolyM act)

logMessage :: forall s. String -> PolyM s ()
logMessage msg = MkPolyM $ tell [PMLogMessage msg]

evalFreshScopeM :: FreshScopeM a -> a
evalFreshScopeM act = evalState (evalStateT act M.empty) 0

runFreshScopeM :: FreshScopeM a -> (a, Int)
runFreshScopeM act = runState (evalStateT act M.empty) 0

freshScope :: FreshScopeM a -> PolyM s a
freshScope act = MkPolyM $ lift $ lift $ lift $ do
  s <- get
  put LM.empty
  r <- act
  put s
  return r

-- | Introduce a name in a scope. If the name does not exist in the scope
-- already it is suffixed a unique identifier. If the name has already been
-- introduced, the previously drawn suffix for the name is reused.
introduceName :: String -> FreshScopeM String
introduceName v = do
  subst <- get
  case M.lookup v subst of
    Nothing -> do
      n <- lift $ do
          n <- get
          put (succ n)
          return n
      let var' = ((takeWhile (/='\'') v) ++ "'" ++ show n)
      modify (M.insert v var')
      return  var'
    Just var' -> return var'

data TypeSig a where
  TypeSigConstr   :: TyCon -> [a] -> TypeSig a
  TypeSigFreeVar  :: String -> TypeSig a
  TypeSigRigidVar :: String -> TypeSig a
  deriving (Eq, Functor, Foldable, Traversable, Show)

$(derive [makeEqF, makeOrdF] [''TypeSig])

instance ShowConstr TypeSig where

  showConstr (TypeSigConstr tc _) = show tc
  showConstr (TypeSigRigidVar s) = "rigid(" ++ s ++ ")"
  showConstr (TypeSigFreeVar s) = "free(" ++ s ++ ")"

instance Render TypeSig where

  stringTreeAlg (TypeSigFreeVar s) = Node ("rigid(" ++ s ++")") []
  stringTreeAlg (TypeSigRigidVar s) = Node ("free(" ++ s ++")") []
  stringTreeAlg (TypeSigConstr tc args) = Node (show tc) args

deriving instance (Functor f, Show v, ShowF f) => Show (UnifError f v)
deriving instance Show (UnifyState TypeSig String)

instance ShowF TypeSig where

  showF (TypeSigConstr tc args) = show tc ++ " " ++ (unwords $ args)
  showF (TypeSigRigidVar s) = "rigid(" ++ s ++")"
  showF (TypeSigFreeVar s) = "free(" ++ s ++")"

instance HasVars TypeSig String where

  isVar (TypeSigFreeVar v) = Just v
  isVar _                  = Nothing

  bindsVars _ = empty

freeVarTyCon :: TyCon
freeVarTyCon = typeRepTyCon (typeRep ( Proxy :: Proxy (FreeVar "")))

rigidVarTyCon :: TyCon
rigidVarTyCon = typeRepTyCon (typeRep ( Proxy :: Proxy (RigidVar "")))

tyVarTyCon :: TyCon
tyVarTyCon = typeRepTyCon (typeRep (Proxy :: Proxy (PolyVal S "")))

toTypeSig :: TypeRep -> FreshScopeM (Term TypeSig)
toTypeSig (RigidVar' v) = do
  v' <- introduceName v
  return $ Term (TypeSigRigidVar v')
toTypeSig (FreeVar' v) = do
  v' <- introduceName v
  return $ Term (TypeSigFreeVar v')
toTypeSig (PolyVal' v) = return $ Term (TypeSigFreeVar v)
toTypeSig (Ty' tc ts) = Term . TypeSigConstr tc <$> mapM toTypeSig ts
toTypeSig _ = error "'toTypeSig': impossible"

-- non-fresh version of 'toTypeSig'.
toTypeSig' :: TypeRep -> Term TypeSig
toTypeSig' (RigidVar' v) = Term (TypeSigRigidVar v)
toTypeSig' (FreeVar' v) = Term (TypeSigFreeVar v)
toTypeSig' (PolyVal' v) = Term (TypeSigFreeVar v)
toTypeSig' (Ty' tc ts) = Term (TypeSigConstr tc (map toTypeSig' ts))
toTypeSig' _ = error "'toTypeSig'': impossible"

fromTypeSig :: Term TypeSig -> TypeRep
fromTypeSig (Term (TypeSigConstr c args)) =
  mkTyConApp c (map fromTypeSig args)
fromTypeSig (Term (TypeSigFreeVar v)) =
  mkTyConApp freeVarTyCon [typeLitTypeRep ("\"" ++ v ++ "\"")]
fromTypeSig (Term (TypeSigRigidVar v)) =
  mkTyConApp rigidVarTyCon [typeLitTypeRep ("\"" ++ v ++ "\"")]

pattern RigidVar' v <-
  Ty'
    ((== rigidVarTyCon) -> True)
    [Ty' (tail . init . tyConName -> v) []]

pattern FreeVar' v <-
  Ty'
    ((== freeVarTyCon) -> True)
    [Ty' (tail . init . tyConName -> v) []]

-- | Pattern matching for TypeRep and TySigRep.
pattern Ty' con args <- (splitTyConApp -> (con, args))
pattern PolyVal' v <-
  Ty'
    ((== tyVarTyCon) -> True)
    [_, Ty' (tail . init . tyConName -> v) []]

-- primitive operations

-- | Introduce a 'PolyDyn' to 'PolyM', attempting to unify its type with
-- 'sig', binding occurrences of 'PolyVal' in 'sig' in the recorded type
-- environment.
-- Occurrences of 'FreeVar' in the 'PolyDyn' are always fresh.
-- TODO: Disallow FreeVar in 'sig'. In GHC8: Use CustomTypeErrors
unbox :: forall s sig. Typeable sig => PolyDyn -> PolyM s sig
unbox (PolyDyn t x) = do
  tySigT <- freshScope $ toTypeSig t
  let sigSig = typeRep (Proxy :: Proxy sig)
  tySigSig <- freshScope $ toTypeSig sigSig
  MkPolyM $ do
    when (hasVars sigSig) $ do
      throwError (
        UnifError $ "Type " ++ show sigSig ++
        " contains 'FreeVar' or 'RigidVar' (Must use only 'PolyVal s')")
    subst <- gets usSubst
    putEqs [(appSubstEq subst (tySigT, tySigSig))]
    runUnify
  logMessage $ "unbox: unsafeCoercing x :: "
      ++ show t ++ " to " ++ show sigSig
  return $ unsafeCoerce x

  where

    hasVars :: TypeRep -> Bool
    hasVars (FreeVar' _) = True
    hasVars (RigidVar' _) = True
    hasVars (Ty' _ args) = or (map hasVars args)
    hasVars _ = error "'unbox (hasVars)': impossible."

-- | Unsafely box an 'a', under the guise of being a 'sig'.
unsafeBox :: Typeable sig => a -> Proxy sig -> PolyM s PolyDyn
unsafeBox x psig = do
  t <- freshScope $ toTypeSig (typeRep psig)
  MkPolyM $ do
    subst <- gets usSubst
    return $ PolyDyn (fromTypeSig (appSubst subst t)) x

-- | Box a value as a 'PolyDyn'. Occurrences of 'PolyVal' in the
-- type of 'sig' are replaced their bindings, which may be 'FreeVar'.
-- Occurrences of 'FreeVar' in the type of 'sig' are fresh
-- wrt. occurring 'PolyVal's.
box ::
  forall sig s.
  (IsPolyDyn sig ~ 'False, Typeable sig) =>
  sig ->
  PolyM s PolyDyn
box x = unsafeBox x (Proxy :: Proxy sig)

type family ToRigidVar (sig :: k) :: k where
  ToRigidVar (FreeVar n) = RigidVar n
  -- Change to ((c :: j -> k) a) in GHC 8.0.1, bug due to
  -- https://ghc.haskell.org/trac/ghc/ticket/11699
  ToRigidVar ((c :: * -> k) a) = (ToRigidVar c) (ToRigidVar a)
  ToRigidVar a = a

newtype PolyMatch sig = MkPolyMatch { eraseMatch :: PolyDyn }
  deriving Show

-- Yield a Just PolyMatch if 'sig' unifies with the type of the value
-- contained in the PolyDyn.
polyMatch ::
  forall (sig :: *).
  (Typeable sig) =>
  PolyDyn ->
  Maybe (PolyMatch sig) -- 'sig' can use 'FreeVar's as type wildcards.
polyMatch pd =
  either (const Nothing) Just
  (runPolyM $ polyMatch' pd (Proxy :: Proxy sig))

polyMatch' ::
  forall sig s.
  (Typeable sig) =>
  PolyDyn ->
  Proxy sig ->
  PolyM s (PolyMatch sig)
polyMatch' pd@(PolyDyn t _) pSig = do
  tySigT <- freshScope $ toTypeSig t
  let sigSig = typeRep pSig
  tySigSig <- freshScope $ toTypeSig sigSig
  MkPolyM $ do
    subst <- gets usSubst
    putEqs [(appSubstEq subst (tySigT, tySigSig))]
    runUnify
    return $ MkPolyMatch pd

type family ToPolyVal (s :: *) (sig :: k) :: k where
  ToPolyVal s (FreeVar name) = PolyVal s name
  -- Change to ((c :: j -> k) a) in GHC 8.0.1, bug due to
  -- https://ghc.haskell.org/trac/ghc/ticket/11699
  ToPolyVal s ((c :: * -> k) a) = (ToPolyVal s c) (ToPolyVal s a)
  ToPolyVal s a = a

unboxMatch ::
  (Typeable sig, Typeable (ToPolyVal s sig)) =>
  PolyMatch sig ->
  PolyM s (ToPolyVal s sig)
unboxMatch (MkPolyMatch pd) = unbox pd

type family HasTyVars (sig :: k) :: Bool where
  HasTyVars (FreeVar n) = 'True
  HasTyVars (RigidVar n) = 'True
  HasTyVars ((c :: * -> k) a) = Or (HasTyVars c) (HasTyVars a)
  HasTyVars a = 'False

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'False 'False = 'False
  Or 'False 'True  = 'True
  Or 'True  'False = 'True
  Or 'True  'True  = 'True

unMatch :: forall sig.
  (Typeable sig, HasTyVars sig ~ 'False) =>
  PolyMatch sig ->
  sig
unMatch pm = unsafePolyM $ go
  where
    -- NOTE: Because of (HasTyVars sig ~ False) we know that (sig ~
    -- ToPolyVal s sig) for any 's', hence the unsafeCoerce.
    go :: forall s. PolyM s sig
    go = case (unsafeCoerce Refl :: sig :~: ToPolyVal s sig) of
           Refl -> unboxMatch pm

unMatchT ::
  forall t name.
  (Typeable t, Typeable name, HasTyVars t ~ 'False, Traversable t) =>
  PolyMatch (t (FreeVar name)) ->
  t PolyDyn
unMatchT pMatch = unsafePolyM $ go

  where
    -- NOTE: Because of (HasTyVars t ~ False) we know that (sig ~
    -- ToPolyVal s t) for any 's', hence the unsafeCoerce.
    go :: forall s. Typeable s => PolyM s (t PolyDyn)
    go = case (unsafeCoerce Refl :: t :~: ToPolyVal s t) of
      Refl -> do
        t <- unboxMatch pMatch
        traverse box t

-- The concrete 's' used in 'runPolyM'.
data S
  deriving (Typeable)

runPolyM :: (forall s. Typeable s => PolyM s a) -> Either PolyError a
runPolyM (MkPolyM unifyM :: PolyM S a) =
  let
    loggerM = evalUnifyM' unifyM
    freshM = runLoggerM loggerM
    (res, _) = evalFreshScopeM freshM
  in res

debugPolyM ::
  PolyM S a ->
  (
    Either
      (UnifError TypeSig String)
      (a, UnifyState TypeSig String),
    [PolyMLog],
    Int
  )
debugPolyM  (MkPolyM unifyM :: PolyM S a) =
  let
    loggerM = runUnifyM' unifyM
    freshM = runLoggerM loggerM
    ((res, log), nextFresh) = runFreshScopeM freshM
  in (res, log, nextFresh)

unsafePolyM :: (forall s. Typeable s => PolyM s a) -> a
unsafePolyM x = either (error . renderPolyError) id $ runPolyM x

type family UnPolyVal (s :: *) (name :: Symbol) (val :: k) (a :: *) :: k where
  UnPolyVal s name (PolyVal s name) a = a
  UnPolyVal s name ((c :: * -> k) b) a =
    (UnPolyVal s name c a) (UnPolyVal s name b a)
  UnPolyVal s name b a = b

-- | Use a value that contains 'PolyVal's like it contained rigid type
-- variables instead. Necessary in order to pattern match on GADTs
-- that have been 'PolyVal'ed in their type arguments.
withPolyVal ::
  forall s name val b.
  Proxy name ->
  (forall a. UnPolyVal s name val a -> PolyM s b) ->
  val ->
  PolyM s b
withPolyVal _ f v = f (unsafeCoerce v)

{-
type family UnFreeVar (name :: Symbol) (val :: k) (a :: *) :: k where
  UnFreeVar name (FreeVar name) a = a
  UnFreeVar name ((c :: * -> k) b) a = (UnFreeVar name c a) (UnFreeVar name b a)
  UnFreeVar name b a = b


If only we could (unsafely) recover a Typeable instance from the value
within a polydyn I believe that we could dispense with all our PolyM
stuff and insted use combinators a la those below:

-- | See 'withPolyVal'. Usable together with 'unMatch' for 'PolyMatch'.
withFreeVar ::
  forall name val b.
  Proxy name ->
  (forall a. UnFreeVar name val a -> b) ->
  val ->
  b

bindFreeVar ::
  forall name val a b.
  Proxy name ->
  (UnFreeVar name val a -> b) ->
  val ->
  b

-}

-- derived operations

-- | Introduce a value as an instance of a 'PolyVal', binding the name of
-- the 'PolyVal' to the type of 'sig'.
var ::
  (Typeable (PolyVal s name), Typeable sig, IsPolyDyn sig ~ 'False) =>
  Proxy name ->
  sig ->
  PolyM s (PolyVal s name)
var _ x = do
  b <- box x
  unbox b

castPoly ::
  (Typeable sig, Typeable sig', IsPolyDyn sig ~ 'False) =>
  sig ->
  PolyM s sig'
castPoly x = box x >>= unbox

polyDyn :: (IsPolyDyn a ~ 'False, Typeable a) => a -> PolyDyn
polyDyn x = either (error . renderPolyError) id $ (runPolyM $ box x)

unsafePolyDyn ::
  (IsPolyDyn a ~ 'False, Typeable sig) =>
  a ->
  Proxy sig ->
  PolyDyn
unsafePolyDyn x psig =
  either (error . renderPolyError) id $ (runPolyM $ unsafeBox x psig)

type family IsPolyDyn (a :: *) :: Bool where
  IsPolyDyn PolyDyn = 'True
  IsPolyDyn a       = 'False

-- | Apply the variable substitutions in the type environment of a PolyM session
-- to a TypeRep. Occurrences of "PolyVal" and "FreeVar" are replaced with
-- corresponding bindings, if any.
applyTypeEnv :: forall s. Term TypeSig -> PolyM s (Term TypeSig)
applyTypeEnv tySig = do
  subst <- MkPolyM $ gets usSubst
  return $ appSubst subst tySig

-- Auxiliary operations

renderPolyError :: PolyError -> String
renderPolyError = show

-- Tests

test1 ::
  forall s a.
  (IsPolyDyn a ~ 'False, Typeable a, Typeable s) =>
  a ->
  PolyM s PolyDyn
test1 x =
  let idBox = polyDyn (id :: FreeVar "a" -> FreeVar "a")
  in do
    id' :: (PolyVal s "b" -> PolyVal s "c") <- unbox idBox
    x'  <- var (Proxy :: Proxy "b") x
    box $ id' x'

-- Should succeed
test2 :: Either PolyError String
test2 = runPolyM $ do
  t1 <- test1 ("string" :: String)
  idApp :: String <- unbox t1
  return idApp

-- Should fail
test3 :: Either PolyError Int
test3 = runPolyM $ do
  t1 <- test1 ("string" :: String)
  idApp :: Int <- unbox t1
  return idApp

--test4' :: Either PolyError [TypeRep]
test4' = debugPolyM $ ((do
  _ :: PolyVal s "a" <- castPoly (42 :: Int)
  let ty1 = toTypeSig' $ typeRep (Proxy :: Proxy (PolyVal s "a"))
  let ty2 = toTypeSig' $ typeRep (Proxy :: Proxy (FreeVar "a"))
  ty1'1 <- applyTypeEnv ty1
  ty1'2 <- applyTypeEnv ty1
  ty2'1 <- applyTypeEnv ty2
  ty2'2 <- applyTypeEnv ty2
  return [ty1'1, ty1'2, ty2'1, ty2'2])
  :: forall s. Typeable s => PolyM s [Term TypeSig])

-- * Type classes

type TypeClassStore = M.Map TypeClassInstanceKey TDict

-- | In a 'TypeClassInstanceKey' we use a canonical representation of type
-- variables (FreeVar) as integers, numbered from zero 'from left to right'.
type TypeClassInstanceKey = Cxt Hole TypeSig Int

-- | Construct the key into a TypeClassStore that corresponds directly to a
-- given constraint.
typeClassInstanceKey :: Term TypeSig -> TypeClassInstanceKey
typeClassInstanceKey tySig =
  evalState (mapM go (varsToHoles tySig)) M.empty
  where
    go :: String -> State (M.Map String Int) Int
    go n = do
      subst <- get
      case M.lookup n subst of
        Just ix -> return ix
        Nothing -> do
          let ix = M.size subst
          put (M.insert n ix subst)
          return ix

-- | Produce successively more parameteterised versions of a
-- 'TypeClassInstanceKey'. When resolving instances, a request for eg
-- 'Show (Either Int String)' can potentially be satisfied by instances of the
-- form:
--
-- * instance Show (Either Int String)
-- * instance Show (Either a String)
-- * instance Show (Either Int [b])
-- * instance Show (Either Int [b])
-- * instance Show (Either Int b)
-- * instance Show (Either a b)
-- * instance Show a
--
-- Note that we neglect parameterising over other kinds than '*'.
-- We won't consider eg:
-- * instance Show (a Int String)
-- The is that I think that the solution above will suffice until I know the
-- direction of further development.
generaliseKey :: TypeClassInstanceKey -> [TypeClassInstanceKey]
generaliseKey key =
  -- Note that we discard the last element, as it represents the completely
  -- abstract 'instance a', which is beyond what can be expressed with type
  -- classes.
  init $ map (flip evalState 0) (go key)
  where
    go :: TypeClassInstanceKey -> [State Int TypeClassInstanceKey]
    go (Hole _) = [freshHole]
    go t@(Term (TypeSigConstr _ [])) = [return t, freshHole]
    go t@(Term (TypeSigConstr tycon args)) = (do
      args' <- allCombinations $ map go args
      return $ (Term . (TypeSigConstr tycon)) <$> sequence args') ++ [freshHole]

    freshHole :: forall f. State Int (Context f Int)
    freshHole = do
      i <- get
      put $ succ i
      return $ Hole i

allCombinations :: [[a]] -> [[a]]
allCombinations [] = []
allCombinations [b] = [[x] | x <- b]
allCombinations (b:buckets) = do
  x <- b
  xs <- allCombinations buckets
  return (x:xs)

data TDict where
  TSub :: (Typeable h, Typeable b) => SubDecomposed (h :- b) -> TDict

-- A poor mans means to structurally decompose a Constraint tuple.
data SubDecomposed c where
  Sub1 :: (Typeable h1, Typeable b) => h1 :- b -> SubDecomposed (h1 :- b)
  Sub2 ::
    (
      Typeable h1,
      Typeable h2,
      Typeable b
    ) =>
    (h1, h2) :- b -> SubDecomposed ((h1, h2) :- b)
  Sub3 ::
    (
      Typeable h1,
      Typeable h2,
      Typeable h3,
      Typeable b
    ) =>
    (h1, h2, h3) :- b -> SubDecomposed ((h1, h2, h3) :- b)

instance Show TDict where
  showsPrec d (TSub (_ :: SubDecomposed (h :- b))) =
    showParen
      (d > 10)
      (showString "TSub (Sub Dict :: " .
       shows (typeRep (Proxy :: Proxy h)) .
       showString " :- " .
       shows (typeRep (Proxy :: Proxy b)) .
       showString ")")

-- The big fat store of all recorded type class instances. Perhaps we could one
-- day produce this by means of a ghc plugin?
-- With TemplateHaskell, we can.
tcStore :: TypeClassStore
tcStore = M.fromList
  [
    recordTC1 (Sub Dict :: () :- Show Bool),
    recordTC1 (Sub Dict :: () :- Show Int),
    recordTC1 (Sub Dict :: () :- Show Double),
    recordTC1 (Sub Dict :: Show (FreeVar "a") :- Show ((Maybe (FreeVar "a")))),
    recordTC2 (Sub Dict ::
                  (Show (FreeVar "a"), Show (FreeVar "b"))
                  :- Show ((Either (FreeVar "a") (FreeVar "b"))))
  ]

recordTC1 ::
  forall h b.
  (Typeable h, Typeable b) =>
  h :- b ->
  (TypeClassInstanceKey, TDict)
recordTC1 sub =
  (
    typeClassInstanceKey $ (toTypeSig' $ typeRep (Proxy :: Proxy b)),
    TSub (Sub1 sub)
  )

recordTC2 ::
  forall h1 h2 b.
  (Typeable h1, Typeable h2, Typeable b) =>
  (h1, h2) :- b ->
  (TypeClassInstanceKey, TDict)
recordTC2 sub =
  (
    typeClassInstanceKey $ (toTypeSig' $ typeRep (Proxy :: Proxy b)),
    TSub (Sub2 sub)
  )

recordTC3 ::
  forall h1 h2 h3 b.
  (Typeable h1, Typeable h2, Typeable h3, Typeable b) =>
  (h1, h2, h3) :- b ->
  (TypeClassInstanceKey, TDict)
recordTC3 sub =
  (
    typeClassInstanceKey $ (toTypeSig' $ typeRep (Proxy :: Proxy b)),
    TSub (Sub3 sub)
  )

lookupInstanceMaybeT ::
  forall s c.
  (Typeable c) =>
  TypeClassStore ->
  Term TypeSig ->
  MaybeT (PolyM s) (Dict c)
lookupInstanceMaybeT m bodyTySig = mapMaybeT logRegion $ do

  {-
  Now, implementing this was quite an effort. A study of confusion and
  'unsafePerformIO' for debugging.

  I guess a bit of commentary would be appropriate.

  In order to figure out how to recursively do instance lookups, I resorted to
  'unsafePerformIO' to output debug statements.

  IMHO this is a symptom of bad form. Debug aides (like 'putStrLn') are only
  transient tools that allow one to navigate complexity in the moment of
  implementing. Afterwards there's just a complex bulb left, with no indications
  as to what it supposedly does and how it does it.

  Now, what should I have done instead? I'm not sure, but I do have some
  observations:

  * 'MaybeT' doesn't leave much room for error reporting.
  * 'PolyM' only has 'UnifError'
  * Tracing in 'PolyM' (using 'MonadWriter') could be useful to supplement
    unification errors.
  * Adhering to Unit/Property tests rather than log tracing could inform a more
    elegant/approachable/inspectable design.

  Epilogue: In the end I added a logging facility to PolyM, but have yet to do
  testing.

  -}

  -- Lookup the various keys that would satisfy the constraint of 'p', matching
  -- the most specific first.
  lift $ logMessage ("Resolving type class instance:\n" ++ showTerm bodyTySig)
  let keys = generaliseKey (typeClassInstanceKey $ bodyTySig)
  let First match = mconcat $ map (First . flip M.lookup m) keys
  (TSub (subDecomposed :: SubDecomposed (h :- c'))) <- MaybeT (return match)

  Refl :: c' :~: c <- return $ unsafeCoerce Refl

  case eqT of
    Just (Refl :: (() :: Constraint) :~: h) -> do
      Sub1 (Sub d) <- return subDecomposed
      return d
    Nothing ->

      -- For each constraint in the head of the located instance, unify the type
      -- variables that occur in them with those of bodyTySig.
      -- That is: unify( h' :- c' === $a :- bodyTySig)
      -- substituting $a yields the heads we're interested in looking up
      -- recursively.

      case subDecomposed of

        Sub1 (sub :: (h1') :- c') -> do

          h1Sig <- lift $ freshSig (Proxy :: Proxy (FreeVar "a"))

          let subTySig = subSig h1Sig bodyTySig
          subTySig' <- lift $ freshSig (Proxy :: Proxy ((h1') :- c'))

          lift $ MkPolyM $ putEqs [(subTySig, subTySig')]
          lift $ MkPolyM $ runUnify

          h1Subst <- lift (applyTypeEnv h1Sig)

          {-
          subst <- lift $ MkPolyM get
          seq (unsafePerformIO (putStrLn $ "bodyTySig = " ++ show bodyTySig)) (return ())
          seq (unsafePerformIO (putStrLn $ "keys = " ++ show keys)) (return ())
          seq (unsafePerformIO (putStrLn $ "Sub1 :: " ++ show (typeOf sub))) (return ())
          seq (unsafePerformIO (putStrLn $ "subTySig = " ++ show (subTySig))) (return ())
          seq (unsafePerformIO (putStrLn $ "subTySig' = " ++ show (subTySig'))) (return ())
          --let nextKey = typeClassInstanceKey subTySig
          --seq (unsafePerformIO (putStrLn $ "nextKey = " ++ show (nextKey))) (return ())
          seq (unsafePerformIO (putStrLn $ "subst = " ++ show (subst))) (return ())
            -}

          Dict :: Dict h1' <- lookupInstanceMaybeT m h1Subst
          Sub d <- return sub
          return d

        Sub2 (sub :: (h1', h2') :- c') -> do

          [h1Subst, h2Subst] <- lift $ unifySub (proxyOf sub)
          Dict :: Dict h1' <- lookupInstanceMaybeT m h1Subst
          Dict :: Dict h2' <- lookupInstanceMaybeT m h2Subst
          Sub d <- return sub
          return d

        Sub3 (sub :: (h1', h2', h3') :- c') -> do

          [h1Subst, h2Subst, h3Subst] <- lift $ unifySub (proxyOf sub)
          Dict :: Dict h1' <- lookupInstanceMaybeT m h1Subst
          Dict :: Dict h2' <- lookupInstanceMaybeT m h2Subst
          Dict :: Dict h3' <- lookupInstanceMaybeT m h3Subst
          Sub d <- return sub
          return d

  where

    unifySub ::
      forall h' b'.
      (Typeable h', Typeable b') =>
      Proxy (h' :- b') ->
      PolyM s [Term TypeSig]
    unifySub pSub' = do
      sub'Sig@(Term (TypeSigConstr _ sub'Args)) <- freshSig pSub'
      let [h'Sig, b'Sig] = sub'Args
      let Term (TypeSigConstr hTyCon h'Components) = h'Sig
      hComponents <- sequence $ replicate (length h'Components)
                    (freshSig (Proxy :: Proxy (FreeVar "a")))

      let subSig_ = subSig (Term (TypeSigConstr hTyCon hComponents)) bodyTySig

      MkPolyM $ putEqs [(subSig_, sub'Sig)]
      MkPolyM $ runUnify

      mapM applyTypeEnv hComponents

    freshSig :: forall a. Typeable a => Proxy a -> PolyM s (Term TypeSig)
    freshSig = freshScope . toTypeSig . typeRep

proxyOf :: Typeable a => a -> Proxy a
proxyOf _ = Proxy

subSig :: Term TypeSig -> Term TypeSig -> Term TypeSig
subSig h b =
  let
    Term (TypeSigConstr tyCon []) = toTypeSig' (typeRep (Proxy :: Proxy (:-)))
  in Term (TypeSigConstr tyCon [h, b])

lookupInstanceMaybe ::
  forall s c.
  (Typeable c) =>
  TypeClassStore ->
  Term TypeSig ->
  PolyM s (Maybe (Dict c))
lookupInstanceMaybe m p = runMaybeT $ lookupInstanceMaybeT m p

-- | Lookup an instance for the constraint 'c' in a given typeclass store.
lookupInstance ::
  forall s (c :: Constraint).
  (Typeable c) =>
  TypeClassStore ->
  Proxy c ->
  PolyM s (Dict c)
lookupInstance m p = do
  tySig <- applyTypeEnv =<< freshScope (toTypeSig $ typeRep p)
  res <- lookupInstanceMaybe m tySig
  case res of
    Just res' -> return res'
    Nothing ->
      MkPolyM $ throwError $ UnifError $ "No instance for " ++ show (tySig)

test5 :: forall s. Typeable s => TypeClassStore -> PolyDyn -> PolyM s (String)
test5 tc pd = do
  v :: PolyVal s "a" <- unbox pd
  Dict <- lookupInstance tc (Proxy :: Proxy (Show (PolyVal s "a")))
  return (show v)
