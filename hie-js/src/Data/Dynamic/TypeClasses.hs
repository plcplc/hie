{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}

-- | This module enables dynamic resolution of type class instances.
module Data.Dynamic.TypeClasses
  (
    -- * Public types
    TypeClassStore,

    -- * Public functions
    lookupInstance,
    lookupInstanceMaybe,
    recordTC1,
    recordTC2,
    recordTC3,

  ) where

import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Comp
import Data.Comp.Render
import Data.Comp.Variables
import Data.Constraint
import Data.Dynamic.PolyDyn
import Data.Proxy
import Data.Typeable
import Unsafe.Coerce
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Data.Map as M

newtype TypeClassStore =
  MkTypeClassStore {unTypeClassStore :: TypeClassStore' }
type TypeClassStore' = M.Map TypeClassInstanceKey TDict

instance Monoid TypeClassStore where
  mempty = MkTypeClassStore M.empty
  mappend (MkTypeClassStore tcs1) (MkTypeClassStore tcs2) =
    MkTypeClassStore (M.union tcs1 tcs2)
  mconcat = MkTypeClassStore . M.unions . map unTypeClassStore

data TDict where
  TSub :: (Typeable h, Typeable b) => SubDecomposed (h :- b) -> TDict

-- | In a 'TypeClassInstanceKey' we use a canonical representation of type
-- variables (FreeVar) as integers, numbered from zero 'from left to right'.
newtype TypeClassInstanceKey =
  MkTypeClassInstanceKey { unTypeClassInstanceKey :: TypeClassInstanceKey' }
  deriving (Ord, Eq)
type TypeClassInstanceKey' = Data.Comp.Cxt Hole TypeSig Int

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

-- | Construct the key into a TypeClassStore that corresponds directly to a
-- given constraint.
typeClassInstanceKey :: Term TypeSig -> TypeClassInstanceKey
typeClassInstanceKey tySig =
  MkTypeClassInstanceKey $ evalState (mapM go (varsToHoles tySig)) M.empty
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
generaliseKey (MkTypeClassInstanceKey key) =
  -- Note that we discard the last element, as it represents the completely
  -- abstract 'instance a', which is beyond what can be expressed with type
  -- classes.
  init $ map (MkTypeClassInstanceKey . flip evalState 0) (go key)
  where
    go :: TypeClassInstanceKey' -> [State Int TypeClassInstanceKey']
    go (Hole _) = [freshHole]
    go t@(Term (TypeSigConstr _ [])) = [return t, freshHole]
    go (Term (TypeSigConstr tycon args)) = (do
      args' <- allCombinations $ map go args
      return $ (Term . (TypeSigConstr tycon)) <$> sequence args') ++ [freshHole]
    go (Term t) = error $ "impossible (generaliseKey) case: " ++ show t

    freshHole :: forall f. State Int (Context f Int)
    freshHole = do
      i <- get
      put $ succ i
      return $ Hole i

-- | Given list 'buckets = [xs, ys, zs, ..]' of lists, produce a list of all the
-- possible outcomes of choosing one element from each of xs, ys, zs, etc..
--
-- The number of combinations equal 'product (map length buckets)'.
allCombinations :: [[a]] -> [[a]]
allCombinations [] = []
allCombinations [b] = [[x] | x <- b]
allCombinations (b:buckets) = do
  x <- b
  xs <- allCombinations buckets
  return (x:xs)

recordTC1 ::
  forall h b.
  (Typeable h, Typeable b) =>
  h :- b ->
  TypeClassStore
recordTC1 sub =
  MkTypeClassStore $ M.singleton
    (typeClassInstanceKey $ (toTypeSig' $ typeRep (Proxy :: Proxy b)))
    (TSub (Sub1 sub))

recordTC2 ::
  forall h1 h2 b.
  (Typeable h1, Typeable h2, Typeable b) =>
  (h1, h2) :- b ->
  TypeClassStore
recordTC2 sub =
  MkTypeClassStore $ M.singleton
    (typeClassInstanceKey $ (toTypeSig' $ typeRep (Proxy :: Proxy b)))
    (TSub (Sub2 sub))

recordTC3 ::
  forall h1 h2 h3 b.
  (Typeable h1, Typeable h2, Typeable h3, Typeable b) =>
  (h1, h2, h3) :- b ->
  TypeClassStore
recordTC3 sub =
  MkTypeClassStore $ M.singleton
    (typeClassInstanceKey $ (toTypeSig' $ typeRep (Proxy :: Proxy b)))
    (TSub (Sub3 sub))

lookupInstanceMaybeT ::
  forall s c.
  (Typeable c) =>
  TypeClassStore' ->
  Term TypeSig ->
  MaybeT (PolyM s) (Dict c)
lookupInstanceMaybeT m bodyTySig = mapMaybeT logRegion $ do

  -- Lookup the various keys that would satisfy the constraint of 'p', matching
  -- the most specific first.
  lift $ logMessage ("Resolving type class instance:\n" ++ showTerm bodyTySig)
  let keys = generaliseKey (typeClassInstanceKey $ bodyTySig)
  let First match' = mconcat $ map (First . flip M.lookup m) keys
  (TSub (subDecomposed :: SubDecomposed (h :- c'))) <- MaybeT (return match')

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

          -- Annoyingly, we need a special case for 'Sub1', as 'unifySub' has no
          -- (sensible) way to detect whether or not there is a tuple of
          -- constraints or just a single constraint.

          h1Sig <- lift $ freshSig (Proxy :: Proxy (FreeVar "a"))

          let subTySig = subSig h1Sig bodyTySig
          subTySig' <- lift $ freshSig (Proxy :: Proxy ((h1') :- c'))

          lift $ unify [(subTySig, subTySig')]

          h1Subst <- lift (applyTypeEnv h1Sig)

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

    -- | Unify a given 'h :- b' (which may contain free variables) with
    -- '[freshvar(a)] :- bodyTySig', yielding the unified components of 'h'. The
    -- number of components is determined by inspecting the arguments to the
    -- type constructor used at the top level of 'h'. This has the annoying side
    -- effect that 'unifySub' only works for a 'h' with at least two components.
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

      unify [(subSig_, sub'Sig)]

      mapM applyTypeEnv hComponents

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
lookupInstanceMaybe (MkTypeClassStore m) p = runMaybeT $ lookupInstanceMaybeT m p

-- | Lookup an instance for the constraint 'c' in a given typeclass store.
lookupInstance ::
  forall s (c :: Constraint).
  (Typeable c) =>
  TypeClassStore ->
  Proxy c ->
  PolyM s (Dict c)
lookupInstance m p = do
  tySig <- applyTypeEnv =<< freshSig p
  res <- lookupInstanceMaybe m tySig
  case res of
    Just res' -> return res'
    Nothing -> throwPolyM $ "No instance for " ++ show (tySig)

-- Template Haskell

getInstances :: Name -> Q [InstanceDec]
getInstances ty = do
  ClassI _ instances <- reify ty
  return instances

-- | '$(recordInstances ''Name)' produces an expression that captures all
-- instances of ''Name, which are in scope. The resulting expression has type
-- 'TypeClassStore'
--
-- Example: '$(recordInstances ''Show)'.
recordInstances :: Name -> Q Exp
recordInstances clsName = do
  inst <- getInstances clsName
  -- TODO: Handle type variables of other kinds than '*'.
  is <- mapM recordInstance inst
  [e| mconcat $(return . ListE $ is) |]

-- | Given a reified instance, produce the corresponding 'recordTC_n' expression
-- that captures the instance in a 'TypeClassStore'.
recordInstance :: InstanceDec -> Q Exp
recordInstance (InstanceD cxts hd []) = do
  (ctxSigs, headSig) <- evalFreshScopeM $
        (,) <$> mapM thTySig cxts <*> thTySig hd
  -- TODO 'recordTC1 --> recordTCn'.
  [e| recordTC1 (Sub Dict :: $(return $ mkTupleType ctxSigs) :- $(return headSig)) |]

mkTupleType :: [Type] -> Type
mkTupleType ts = foldl (AppT) (TupleT (length ts)) ts

-- | Reflect a TemplateHaskell type to a tySig-type (i.e. a type that uses
-- 'FreeVar's in the place of proper type variables)
thTySig :: Type -> FreshScopeM Type
thTySig (AppT ty1 ty2) = AppT <$> thTySig ty1 <*> thTySig ty2
thTySig (ForallT _ _ _) = freshErr "thTySig: 'ForallT' not supported."
thTySig (SigT _ _) = freshErr "thTySig: 'SigT' not supported."
thTySig (VarT n) = do
  n' <- LitT . StrTyLit <$> translateName n
  liftQ [t|FreeVar $(return n') |]
thTySig t@(ConT _) = return t
thTySig t@(PromotedT _) = return t
thTySig t@(TupleT _) = return t
thTySig t@(UnboxedTupleT _) = return t
thTySig t@(ArrowT) = return t
thTySig t@(EqualityT) = return t
thTySig t@(ListT) = return t
thTySig t@(PromotedTupleT _) = return t
thTySig t@(PromotedNilT) = return t
thTySig t@(PromotedConsT) = return t
thTySig t@(StarT) = return t
thTySig t@(ConstraintT) = return t
thTySig t@(LitT _) = return t

type FreshScopeM = StateT (M.Map Name String) FreshVarM
type FreshVarM = StateT [String] Q
liftQ :: Q a -> FreshScopeM a
liftQ = lift . lift

evalFreshScopeM :: FreshScopeM a -> Q a
evalFreshScopeM = flip evalStateT varStore . flip evalStateT M.empty

varStore :: [String]
varStore = letterSeq 0

  where

    letters = map (:[]) $ take 26 ['a'..]

    letterSeq n = (map concat $ allCombinations (replicate n letters)) ++
                  (letterSeq (succ n))

freshErr :: String -> FreshScopeM a
freshErr = lift . lift . fail

-- | convert a type variable name to a 'FreeVar' name
translateName :: Name -> FreshScopeM String
translateName n = do
  subst <- get
  case M.lookup n subst of
    Just n' -> return n'
    Nothing -> do
      (nextFree:store) <- lift get
      modify (M.insert n nextFree)
      lift $ put store
      return nextFree

-- Tests

-- The big fat store of all recorded type class instances. Perhaps we could one
-- day produce this by means of a ghc plugin?
-- With TemplateHaskell, we can.
tcStore :: TypeClassStore
tcStore = mconcat
  [
    recordTC1 (Sub Dict :: () :- Show Bool),
    recordTC1 (Sub Dict :: () :- Show Int),
    recordTC1 (Sub Dict :: () :- Show Double),
    recordTC1 (Sub Dict :: Show (FreeVar "a") :- Show ((Maybe (FreeVar "a")))),
    recordTC2 (Sub Dict ::
                  (Show (FreeVar "a"), Show (FreeVar "b"))
                  :- Show ((Either (FreeVar "a") (FreeVar "b"))))
  ]

test5 :: forall s. Typeable s => TypeClassStore -> PolyDyn -> PolyM s (String)
test5 tc pd = do
  v :: PolyVal s "a" <- unbox pd
  Dict <- lookupInstance tc (Proxy :: Proxy (Show (PolyVal s "a")))
  return (show v)
