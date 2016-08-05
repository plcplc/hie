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
import Data.Comp
import Data.Comp.Derive
import Data.Comp.Render
import Data.Comp.Unification
import Data.Comp.Variables
import Data.Proxy
import Data.Tree
import Data.Typeable
import Data.Typeable.Internal
import GHC.TypeLits
import Unsafe.Coerce
import System.IO.Unsafe
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
type Except' = (ExceptT (UnifError TypeSig String)) FreshScopeM
type FreshScopeM = StateT (M.Map String String) (State Int)

freshScope :: FreshScopeM a -> PolyM s a
freshScope act = MkPolyM $ lift $ lift $ do
  s <- get
  put LM.empty
  r <- act
  put s
  return r

freshName :: String -> FreshScopeM String
freshName v = do
  -- If var exists in the scope substitution, use that.
  -- Otherwise, draw a fresh name and add it to the substitution.
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
  isVar _              = Nothing

  bindsVars _ = empty

freeVarTyCon :: TyCon
freeVarTyCon = typeRepTyCon (typeRep ( Proxy :: Proxy (FreeVar "")))

rigidVarTyCon :: TyCon
rigidVarTyCon = typeRepTyCon (typeRep ( Proxy :: Proxy (RigidVar "")))

tyVarTyCon :: TyCon
tyVarTyCon = typeRepTyCon (typeRep (Proxy :: Proxy (PolyVal S "")))

toTypeSig :: TypeRep -> FreshScopeM (Term TypeSig)
toTypeSig (RigidVar' v) = do
  v' <- freshName v
  return $ Term (TypeSigRigidVar v')
toTypeSig (FreeVar' v) = do
  v' <- freshName v
  return $ Term (TypeSigFreeVar v')
toTypeSig (PolyVal' v) = return $ Term (TypeSigFreeVar v)
toTypeSig (Ty' tc ts) = Term . TypeSigConstr tc <$> mapM toTypeSig ts
toTypeSig _ = error "'toTypeSig': impossible"

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
-- 'sig', binding occurrences of 'PolyVal' in 'sig'.
-- Occurrences of 'FreeVar' in the 'PolyDyn' are always fresh.
-- TODO: Disallow FreeVar in 'sig'. In GHC8: Use CustomTypeErrors
unbox :: forall s sig. Typeable sig => PolyDyn -> PolyM s sig
unbox (PolyDyn t x) = do
  tySigT <- freshScope $ toTypeSig t
  let sigSig = typeRep (Proxy :: Proxy sig)
  tySigSig <- freshScope $ toTypeSig sigSig
  MkPolyM $ do
    when (hasNonPolyVal sigSig) $ do
      throwError (
        UnifError $ "Type " ++ show sigSig ++
        " contains 'FreeVar' or 'RigidVar' (Must use only 'PolyVal s')")
    subst <- gets usSubst
    putEqs [(appSubstEq subst (tySigT, tySigSig))]
    runUnify
    return $
      seq
        (unsafePerformIO
         (putStrLn $ "unsafeCoercing x :: " ++ show t ++ " to " ++ show sigSig ))
         unsafeCoerce x

  where

    hasNonPolyVal :: TypeRep -> Bool
    hasNonPolyVal (FreeVar' _) = True
    hasNonPolyVal (RigidVar' _) = True
    hasNonPolyVal (Ty' _ args) = or (map hasNonPolyVal args)
    hasNonPolyVal _ = error "'unbox (hasNonPolyVal)': impossible."

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
box :: (IsPolyDyn sig ~ 'False, Typeable sig) => sig -> PolyM s PolyDyn
box x = do
  t <- freshScope $ toTypeSig (typeOf x)
  MkPolyM $ do
    subst <- gets usSubst
    return $ PolyDyn (fromTypeSig (appSubst subst t)) x

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
  evalState
    (evalStateT
      (runExceptT $ LS.evalStateT unifyM (UnifyState [] LM.empty))
      (LM.empty))
    0

debugPolyM ::
  (forall s. Typeable s => PolyM s a) ->
  ((Either (UnifError TypeSig String) a, LM.Map String String), Int)
debugPolyM  (MkPolyM unifyM :: PolyM S a) =
  runState
    (runStateT
      (runExceptT $ LS.evalStateT unifyM (UnifyState [] LM.empty))
      (LM.empty))
    0

unsafePolyM :: (forall s. Typeable s => PolyM s a) -> a
unsafePolyM x = either (error . renderPolyError) id $ runPolyM x

type family UnPolyVal (s :: *) (name :: Symbol) (val :: k) (a :: *) :: k where
  UnPolyVal s name (PolyVal s name) a = a
  UnPolyVal s name ((c :: * -> k) b) a = (UnPolyVal s name c a) (UnPolyVal s name b a)
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

unsafePolyDyn :: (IsPolyDyn a ~ 'False, Typeable sig) => a -> Proxy sig -> PolyDyn
unsafePolyDyn x psig =
  either (error . renderPolyError) id $ (runPolyM $ unsafeBox x psig)

type family IsPolyDyn (a :: *) :: Bool where
  IsPolyDyn PolyDyn = 'True
  IsPolyDyn a       = 'False

-- Auxiliary operations
 
renderPolyError :: PolyError -> String
renderPolyError = show

-- Tests

test1 :: forall s a. (IsPolyDyn a ~ 'False, Typeable a, Typeable s) => a -> PolyM s PolyDyn
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
