{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
    {-( hieJsMain
    )-} where

import Control.Monad.Except
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Comp
import Data.Comp.Decompose
import Data.Comp.Derive
import Data.Comp.Render
import Data.Comp.Unification
import Data.Comp.Variables
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Tree
import Data.Typeable
import Data.Typeable.Internal
import GHC.TypeLits
import Reflex.Dom
import Unsafe.Coerce
import qualified Control.Monad.State.Lazy as LS
import qualified Data.Map as M
import qualified Data.Map.Lazy as LM

-- Public types

data PolyDyn' where
  -- Private constructor
  PolyDyn' :: TypeRep -> a -> PolyDyn'

instance Show PolyDyn' where
  show (PolyDyn' t _) = "<PolyDyn : " ++ show t ++ ">"

data FreeVar (name :: Symbol)

  -- Private constructor
newtype PolyM (s :: *) (a :: *) =
  MkPolyM { unPolyM :: UnifyM' a }

instance Functor (PolyM s) where
  fmap f (MkPolyM s) = MkPolyM (fmap f s)

instance Applicative (PolyM s) where
  pure x = MkPolyM (pure x)
  (MkPolyM f) <*> (MkPolyM x) = MkPolyM (f <*> x)

instance Monad (PolyM s) where
  (MkPolyM x) >>= f = MkPolyM (x >>= unPolyM . f)

  -- Private constructor
newtype TyVar' (s :: *) (name :: symbol) = MkTyVar' { unTyVar' :: forall a. a }
  deriving Typeable

type PolyError = UnifError TypeSig String

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
freshName var = do
  -- If var exists in the scope substitution, use that.
  -- Otherwise, draw a fresh name and add it to the substitution.
  subst <- get
  case M.lookup var subst of
    Nothing -> do
      n <- lift $ do
          n <- get
          put (succ n)
          return n
      let var' = ((takeWhile (/='\'') var) ++ "'" ++ show n)
      modify (M.insert var var')
      return  var'
    Just var' -> return var'

data TypeSig a where
  TypeSigConstr :: TyCon -> [a] -> TypeSig a
  TypeSigVar    :: String -> TypeSig a
  deriving (Eq, Functor, Foldable, Traversable, Show)

instance ShowConstr TypeSig where

  showConstr (TypeSigConstr tc _) = show tc
  showConstr (TypeSigVar s) = s

instance Render TypeSig where

  stringTreeAlg (TypeSigVar s) = Node s []
  stringTreeAlg (TypeSigConstr tc args) = Node (show tc) args

deriving instance (Functor f, Show v, ShowF f) => Show (UnifError f v)
deriving instance Show (UnifyState TypeSig String)

instance ShowF TypeSig where
  showF (TypeSigConstr tc args) = show tc ++ " " ++ (unwords $ args)
  showF (TypeSigVar s) = "$"++s


instance HasVars TypeSig String where

  isVar (TypeSigVar v) = Just v
  isVar _              = Nothing

  bindsVars _ = empty

freeVarTyCon :: TyCon
freeVarTyCon = typeRepTyCon (typeRep ( Proxy :: Proxy (FreeVar "")))

tyVarTyCon :: TyCon
tyVarTyCon = typeRepTyCon (typeRep (Proxy :: Proxy (TyVar' S "")))

toTypeSig :: TypeRep -> FreshScopeM (Term TypeSig)
toTypeSig (FreeVar' v) = do
  v' <- freshName v
  return $ Term (TypeSigVar v')
toTypeSig (TyVar'' v) = return $ Term (TypeSigVar v)
toTypeSig (Ty' tc ts) = Term . TypeSigConstr tc <$> mapM toTypeSig ts

fromTypeSig :: Term TypeSig -> TypeRep
fromTypeSig (Term (TypeSigConstr c args)) = mkTyConApp c (map fromTypeSig args)
fromTypeSig (Term (TypeSigVar v)) = mkTyConApp freeVarTyCon [typeLitTypeRep ("\"" ++ v ++ "\"")]

pattern FreeVar' v <-
  Ty'
    ((== freeVarTyCon) -> True)
    [Ty' (tail . init . tyConName -> v) []]

pattern TyVar'' v <-
  Ty'
    ((== tyVarTyCon) -> True)
    [_, Ty' (tail . init . tyConName -> v) []]

data S
  deriving (Typeable)

type FreeVarName = String
type TyVarBindings = M.Map String (Either FreeVarName TypeRep)

-- primitive operations

-- Introduce a 'PolyDyn'' to 'PolyM', attempting to unify its type with
-- 'sig', binding occurrences of 'TyVar'' in 'sig'.
-- Occurrences of 'FreeVar' in the 'PolyDyn'' are always fresh.
-- TODO: Disallow FreeVar in 'sig'. In GHC8: Use CustomTypeErrors
unbox :: Typeable sig => PolyDyn' -> Proxy sig -> PolyM s sig
unbox (PolyDyn' t x) psig = do
  tySigT <- freshScope $ toTypeSig t
  tySigSig <- freshScope $ toTypeSig (typeRep psig)
  MkPolyM $ do
  subst <- gets usSubst
  putEqs [(appSubstEq subst (tySigT, tySigSig))]
  runUnify
  return $ unsafeCoerce x

-- Box a value as a 'PolyDyn''. Occurrences of 'TyVar'' in the
-- type of 'sig' are replaced their bindings, which may be 'FreeVar'.
-- Occurrences of 'FreeVar' in the type of 'sig' are fresh
-- wrt. occurring 'TyVar''s.
box :: Typeable sig => sig -> PolyM s PolyDyn'
box x = do
  t <- freshScope $ toTypeSig (typeOf x)
  MkPolyM $ do
    subst <- gets usSubst
    return $ PolyDyn' (fromTypeSig (appSubst subst t)) x

runPolyM :: (forall s. Typeable s => PolyM s a) -> Either PolyError a
runPolyM (MkPolyM unifyM :: PolyM S a) =
  evalState (evalStateT (runExceptT $ LS.evalStateT unifyM (UnifyState [] LM.empty)) (LM.empty)) 0

-- debugPolyM :: (forall s. Typeable s => PolyM s a) -> Either PolyError a
debugPolyM  (MkPolyM unifyM :: PolyM S a) =
  runState (runStateT (runExceptT $ LS.evalStateT unifyM (UnifyState [] LM.empty)) (LM.empty)) 0

unsafePolyM :: (forall s. Typeable s => PolyM s a) -> a
unsafePolyM x = either (error . printPolyError) id $ runPolyM x

-- derived operations

-- Introduce a value as an instance of a 'TyVar'', binding the name of
-- the 'TyVar'' to the type of 'sig'.
var ::
  (Typeable (TyVar' s name), Typeable sig) =>
  Proxy name ->
  sig ->
  PolyM s (TyVar' s name)
var _ x = do
  b <- box x
  unbox b (Proxy :: Proxy (TyVar' s name))

cast ::
  (Typeable sig, Typeable sig') =>
  sig ->
  Proxy sig' ->
  PolyM s sig'
cast x p = box x >>= flip unbox p

polyDyn' :: Typeable a => a -> PolyDyn'
polyDyn' x = either (error . printPolyError) id $ (runPolyM $ box x)

-- Auxiliary operations
 
printPolyError :: PolyError -> String
printPolyError = show

-- Tests

test1 :: (Typeable a, Typeable s) => a -> PolyM s PolyDyn'
test1 x =
  let idBox = polyDyn' (id :: FreeVar "a" -> FreeVar "a")
  in do
    id' <- unbox idBox (Proxy :: Proxy (TyVar' s "b" -> TyVar' s "c"))
    x'  <- var (Proxy :: Proxy "b") x
    box $ id' x'

-- Should succeed
test2 = runPolyM $ do
  t1 <- test1 "string"
  idApp <- unbox t1 (Proxy :: Proxy String)
  return idApp

-- Should fail
test3 = runPolyM $ do
  t1 <- test1 "string"
  idApp <- unbox t1 (Proxy :: Proxy Int)
  return idApp


-- * PolyDyn

data PolyDyn where
  PolyDyn :: TypeRep -> a -> PolyDyn

polyDyn :: Typeable a => a -> PolyDyn
polyDyn x = PolyDyn (typeOf x) x

polyDynTy :: PolyDyn -> TypeRep
polyDynTy (PolyDyn t _) = t

-- Type signatures for matching 'PolyDyn's. Use 'TyVar'
-- polymorphic 'UiImplementation's.
newtype TySigRep = TySigRep { unTySig :: TypeRep }

-- | Special, sanctioned type to use in place of free type variables.
newtype TyVar (name :: Symbol) = MkTyVar { unTyVar :: forall a. a }

-- | Construct a 'TySig'. It is restricted to kind Star. Use 'PolyVar'
-- in place of type variables.
tySig :: (Typeable (a :: *)) => Proxy a -> TySigRep
tySig = TySigRep . typeRep

-- | Pattern matching for TypeRep and TySigRep.
pattern Ty' con args <- (splitTyConApp -> (con, args))
pattern TySig' con args <- (fmap (map TySigRep) . splitTyConApp . unTySig -> (con, args))
pattern TyVar' v <-
  TySig'
    ((== typeRepTyCon (typeRep (Proxy :: Proxy (TyVar "")))) -> True)
    [TySig' (tyConName -> v) []]

-- | Match a TypeRep with a TySig. Yields a mapping from TyVar names to
-- corresponding TypeReps.
tySigMatch :: TypeRep -> TySigRep -> Maybe (M.Map String TypeRep)
tySigMatch ty           (TyVar' name) = Just (M.singleton name ty)
tySigMatch (Ty' c1 a1) (TySig' c2 a2)
  | c1 == c2 && length a1 == length a2
  -- TODO: Throw up if unifying the same name to different TypeReps.
  = foldr M.union M.empty <$> (sequence $ zipWith tySigMatch a1 a2)
tySigMatch _ _ = Nothing

-- | Deconstruct a 'PolyDyn'. Occurences of 'TyVar name' in 'sig' will
-- match anyhting. Values of type 'TyVar name' may be turned into
-- 'PolyDyn' by means of the supplied 'ToPolyDyn' function.
matchPolyDyn ::
  forall sig b. Typeable sig =>
  PolyDyn ->
  (ToPolyDyn -> sig -> b) ->
  Maybe b
matchPolyDyn (PolyDyn tx x) f 
  | Just typeRepEnv <- tySigMatch tx (tySig (Proxy :: Proxy sig)) = Just (f (toDyn typeRepEnv) (unsafeCoerce x))
  where
    toDyn :: M.Map String TypeRep -> (KnownSymbol name => TyVar name -> PolyDyn)
    toDyn env (MkTyVar x' :: TyVar name) =
      let
        pn = (Proxy :: Proxy name)
        name = symbolVal pn
      -- Not exactly pretty, but that's how they appear in TypeReps...
      in case M.lookup ("\"" ++ name ++ "\"") env of
        Just tx' -> PolyDyn tx' x'
        Nothing -> error $ "PolyVar " ++ name ++ " is not present in type map"
    
matchPolyDyn _ _ = Nothing

type ToPolyDyn = forall name. KnownSymbol name => TyVar name -> PolyDyn
newtype ToPolyDyn' = ToPolyDyn' ToPolyDyn

-- * UI framework

data UiImpl uidomain t m where
  UiImpl ::
    (Typeable sig, ui :<: uidomain,
     MonadWidget t m,
     Functor m,
     Reflex t) =>
    (
      (String -> Dynamic t (Maybe (HieValue UiDomain))) ->
      UiEnv uidomain t m ->
      ui (Term uidomain) ->
      ToPolyDyn' ->
      Dynamic t sig ->
      m (HieSessionActions t uidomain, Event t sig)
    ) ->
    UiImpl uidomain t m

type UiEnv uidomain t m = [UiImpl uidomain t m]

lookupUiEnv ::
  forall uidomain t m.
  MonadHold t m =>
  UiEnv uidomain t m ->
  Term uidomain -> 
  (PolyDyn, Event t PolyDyn) ->
  (String -> Dynamic t (Maybe (HieValue UiDomain))) ->
  Maybe (m (HieSessionActions t uidomain, Event t PolyDyn))
lookupUiEnv env (Term ui) (pd, pdEv) lookupHieVal = do
  getFirst . mconcat . map (\impl -> First $ liftUiImpl impl) $ env

  where
    liftUiImpl ::
      UiImpl uidomain t m ->
      Maybe (m (HieSessionActions t uidomain, Event t PolyDyn))
    liftUiImpl (UiImpl impl) = do
      ui' <- proj ui
      (toDyn, sig) <- matchPolyDyn pd (\toDyn d -> (ToPolyDyn' toDyn, d))
      let sigEvMaybe =
            (\pd' -> matchPolyDyn pd' (\_ d -> d)) <$>
            pdEv

      return $ do
        sigDyn <- holdDyn sig (fmapMaybe id sigEvMaybe)
        (sesActions, updateValEv) <- impl lookupHieVal env ui' toDyn sigDyn
        return (sesActions, polyDyn <$> updateValEv)

-- * Uidomains:

pattern Proj x <- (proj -> Just x)

data UiNonShowable e = UiNonShowable
  deriving (Eq, Functor)

instance EqF UiNonShowable where
  eqF _ _ = True

data UiTextLabel e = UiTextLabel
  deriving (Eq, Functor)

instance EqF UiTextLabel where
  eqF _ _ = True

data UiList e = UiList e
  deriving (Eq, Functor)

instance EqF UiList where
  eqF _ _ = True

data UiFunc e = UiFunc String e
  deriving (Eq, Functor)

instance EqF UiFunc where
  eqF _ _ = True

uiNonShowable :: (UiNonShowable :<: uidomain) => Term uidomain
uiNonShowable = Term $ inj UiNonShowable

uiTextLabel :: (UiTextLabel :<: uidomain) => Term uidomain
uiTextLabel = Term $ inj UiTextLabel

uiList :: (UiList :<: uidomain) => Term uidomain -> Term uidomain
uiList x = Term $ inj (UiList x)

uiFunc :: (UiFunc :<: uidomain) => String -> Term uidomain -> Term uidomain
uiFunc title resultui = Term $ inj (UiFunc title resultui)

-- * Reflex-based ui

data HieSessionActions t uidomain =
  HieSessionActions {
    updateBindings :: Event t (M.Map String (Maybe (HieValue uidomain)))
    }

noAction :: Reflex t => HieSessionActions t uidomain
noAction = HieSessionActions never

uiNonShowableImpl ::
  forall uidomain t m.
  (MonadWidget t m, UiNonShowable :<: uidomain) =>
  UiImpl uidomain t m
uiNonShowableImpl = UiImpl go
  where
    go ::
      (sig ~ TyVar "x") =>
      (String -> Dynamic t (Maybe (HieValue UiDomain))) ->
      UiEnv uidomain t m ->
      UiNonShowable (Term uidomain) ->
      ToPolyDyn' ->
      Dynamic t sig ->
      m (HieSessionActions t uidomain, Event t sig)
    go _ _ _ _ _ = text "<Nonshowable>" >> return (noAction, never)

uiTextLabelImpl ::
  forall a uidomain t m.
  (MonadWidget t m, Typeable a, UiTextLabel :<: uidomain) =>
  (a -> String) ->
  UiImpl uidomain t m
uiTextLabelImpl f = UiImpl go
  where
    go ::
      (String -> Dynamic t (Maybe (HieValue UiDomain))) ->
      UiEnv uidomain t m ->
      UiTextLabel (Term uidomain) ->
      ToPolyDyn' ->
      Dynamic t a ->
      m (HieSessionActions t uidomain, Event t a)
    go _ _ _ _ x = do
      strDyn <- mapDyn f x
      dynText strDyn
      return (noAction, never)

uiListImpl ::
  forall t m.
  (MonadWidget t m) =>
  UiImpl UiDomain t m
uiListImpl = UiImpl go
  where
    go ::
      (String -> Dynamic t (Maybe (HieValue UiDomain))) ->
      UiEnv uidomain t m ->
      UiList (Term uidomain) ->
      ToPolyDyn' ->
      Dynamic t [TyVar "x"] ->
      m (HieSessionActions t uidomain, Event t [TyVar "x"])
    go lookupHieVal env (UiList innerUi) (ToPolyDyn' d) xs = do

      -- TODO: map element changes of ui of (TyVar "x") to Event t [TyVar "x"].
      innerEvsDyn <- simpleList xs
        (\x -> do
           pdDyn <- mapDyn d x
           pd <- sample (current $ pdDyn)
           case lookupUiEnv env innerUi (pd, updated pdDyn) lookupHieVal of
             Nothing -> el "div" $ do
               text "Designated UI is nonexistant or incompatible"
               return (noAction, never)
             Just m -> el "div" m
        )

      {-
      (curInnerSessionActions, curInnerValEvs) <- unzip <$> (sample $ current innerEvsDyn)
      let (actionEvs, valueEvs) = splitE $ fmap unzip (updated innerEvsDyn)
      
      -- sesActions <- switchSessionActions (leftmost _)
      -}

      return (noAction, never)

uiFuncImpl ::
  forall uidomain t m.
  (UiFunc :<: uidomain, MonadWidget t m, Reflex t) =>
  UiImpl uidomain t m
uiFuncImpl = UiImpl go
  where

    go ::
      (sig ~ (TyVar "a" -> TyVar "b")) =>
      (String -> Dynamic t (Maybe (HieValue UiDomain))) ->
      UiEnv uidomain t m ->
      UiFunc (Term uidomain) ->
      ToPolyDyn' ->
      Dynamic t sig ->
      m (HieSessionActions t uidomain, Event t sig)

    go lookupHieVal env (UiFunc label resUi) (ToPolyDyn' toPD) f = do

      el "p" (text label)

      argInput <- el "div" $ do
        el "span" (text "argument:")
        textInput def

      targetInput <- el "div" $ do
        el "span" (text "target:")
        textInput def

      (applyBtn, _) <- el' "button" (text "apply")
      
      argVal <- joinDyn <$> mapDyn lookupHieVal (value argInput)
      mapDyn (show . isJust) argVal >>= dynText 
      let applyEvent = gate (fmap isJust (current argVal)) (domEvent Click applyBtn)

      -- Proceed with horribly unsafe actions:
      -- (This should be encapsulated safely in the PolyDyn libarary!)
      let addValue name val = M.singleton name val
      let applyUnsafe f' (HieValue { hieValue = PolyDyn t x }) = HieValue {
            hieUi = resUi,
            hieValue = toPD (f' (unsafeCoerce x))
            }
      let res = (\targetName valMaybe f' -> addValue targetName (applyUnsafe f' <$> valMaybe))
                <$> (current $ value targetInput) <*> current argVal <*> current f
    

      return (HieSessionActions {
               updateBindings = tag res applyEvent
                                }, never)

data HieValue uidomain =
  HieValue {
    hieValue :: PolyDyn,
    hieUi    :: Term uidomain
    }

data UiSession t uidomain =
  UiSession {
    sessionModel :: Dynamic t (M.Map String (HieValue uidomain))
    }

type UiDomain = UiList :+: UiTextLabel :+: UiNonShowable :+: UiFunc

hieJsMain :: IO ()
hieJsMain = mainWidget $ mdo

  el "h1" (text "HIE Document")

  -- Initial test bindings
  addBindingEvent <-
    (M.fromList
     [
       ("foo", Just $ HieValue (polyDyn ([42, 43] :: [Int])) uiTextLabel),
       ("bar", Just $ HieValue (polyDyn ([42, 43] :: [Int])) (uiList (uiTextLabel))),
       ("baz", Just $ HieValue (polyDyn (42 :: Int)) uiTextLabel),
       ("id", Just $ HieValue (polyDyn (id :: TyVar "a" -> TyVar "a")) (uiFunc "id" (uiTextLabel))),
       ("plus3", Just $ HieValue (polyDyn ( (+3) :: Int -> Int)) (uiFunc "+3" (uiTextLabel)))
     ] <$) <$> getPostBuild
  
  wireSession addBindingEvent
  return ()

uiEnv :: forall t m . (MonadWidget t m) => m (UiEnv UiDomain t m)
uiEnv = return
  [
    (uiTextLabelImpl (show :: Double -> String)),
    (uiTextLabelImpl (show :: Int -> String)),
    (uiTextLabelImpl (show :: Char -> String)),
    (uiTextLabelImpl (id :: String -> String)),
    uiListImpl,
    uiNonShowableImpl,
    uiFuncImpl
  ]

-- Note: untested code
selectMapShallow ::
  (Reflex t, Ord k) =>
  Dynamic t (M.Map k a) ->
  k ->
  Dynamic t (Maybe a)
selectMapShallow d k =
  unsafeDynamic b e

  where

    b = pull $ do
      cur <- sample $ current d
      return (M.lookup k cur)

    e = attachWithMaybe
        (\prev m -> case (prev, M.lookup k m) of
            (Nothing, Just v) -> Just (Just v) -- constructor changed: fire.
            (Just _, Nothing) -> Just Nothing -- constructor changed: fire.
            _                 -> Nothing -- constructor hasn't changed, so don't fire.
        )
        b
        (updated d)

-- Note: untested code
joinDynThroughMaybe ::
  forall t a.
  (Reflex t) =>
  Dynamic t (Maybe (Dynamic t a)) ->
  Dynamic t (Maybe a)
joinDynThroughMaybe d =
  unsafeDynamic b (leftmost [eBoth, eOuter, eInner])

  where

    b :: Behavior t (Maybe a)
    b = pull $ do
      cur <- sample $ current d
      case cur of
        Nothing -> return Nothing
        Just cur' -> Just <$> sample (current cur')

    eOuter :: Event t (Maybe a)
    eOuter = pushAlways
      (maybe (return Nothing)
      (fmap Just . sample . current))
      (updated d)

    innerMaybe :: Maybe (Dynamic t a) -> Event t (Maybe a)
    innerMaybe = (maybe never (fmap Just . updated))

    eInner :: Event t (Maybe a)
    eInner = switch (fmap innerMaybe $ current d)

    eBoth :: Event t (Maybe a)
    eBoth = coincidence (fmap innerMaybe $ updated d)

wireSession ::
  forall t m.
  MonadWidget t m =>
  MonadFix m =>
  MonadHold t m =>
  Event t (M.Map String (Maybe (HieValue UiDomain))) ->
  m ()
wireSession updateBindingsExo = mdo

  updateBindingsEndo <-
    switchPromptlyDyn <$>
    mapDyn (mergeWith M.union . M.elems . M.map (updateBindings . fst)) widgetModelDyn 
  let updateBindings' = mergeWith M.union [updateBindingsExo, updateBindingsEndo]

  widgetModelDyn <- listWithKeyShallowDiff M.empty updateBindings' (bindingWidget valuesModel)

  valuesModel' <- mapDyn (M.map snd) widgetModelDyn 
  let valuesModel k = joinDynThroughMaybe $ selectMapShallow valuesModel' k


  -- TODO: Construct a suitable session (data)model.
  return ()
  {-

  -- TODO: Consider merging HieSessionActions to a single, compound event.
  -- All this merging and extracting seems a little awkward..

  let switchUiAction actionDyn selector =
        switchPromptlyDyn <$>
        mapDyn (mergeMap . M.map selector) (joinDynThroughMap actionDyn)

  let updateEndo f = fmap (mconcat . map (Endo . f ) . M.toList)
  let updateValEndo set = updateEndo (\(name, val) -> M.update (Just . set val ) name)

  addValueEvents    <-
    updateEndo (uncurry M.insert . snd)
      <$> switchUiAction bindingEventsDyn addValue
  -}

  {-

  -- NOTE: 'updateValue' and 'updateUi' shouldn't make model updates
  -- in this function. Rather, they are wired at their use site. A use
  -- case for observing these events here could be to update a server.

  let switchUiAction actionDyn selector =
        switchPromptlyDyn <$>
        mapDyn (mergeMap . M.map selector) (joinDynThroughMap actionDyn)

  let updateEndo f = fmap (mconcat . map (Endo . f ) . M.toList)
  let updateValEndo set = updateEndo (\(name, val) -> M.update (Just . set val ) name)

  updateValueEvents <-
    updateValEndo (\val v -> v { hieValue = val })
    <$> switchUiAction bindingEventsDyn updateValue

  updateUiEvents    <-
    updateValEndo (\ui v -> v { hieUi = ui })
    <$> switchUiAction bindingEventsDyn updateUi

  let updateEvents = mergeWith mappend [addValueEvents, updateValueEvents, updateUiEvents]
  -}


bindingWidget ::
  (MonadWidget t m) =>
  (String -> Dynamic t (Maybe (HieValue UiDomain))) ->
  String ->
  HieValue UiDomain ->
  Event t (HieValue UiDomain)->
  m (HieSessionActions t UiDomain, Dynamic t (HieValue UiDomain))
bindingWidget lookupHieVal name val updateBindingEvent = el "div" $ mdo

  el "h2" $ do
    valTy <- holdDyn
             (polyDynTy . hieValue $ val)
             (polyDynTy . hieValue <$> updateBindingEvent)

    nameDyn <- mapDyn (\t -> name ++ " : "  ++ show t) valTy
    dynText nameDyn

  let valueUpdateEvent = leftmost
        [
          hieValue <$> updateBindingEvent
        ]
  let uiUpdateEvent = leftmost
        [
          hieUi <$> updateBindingEvent,
          selectUiEvent
        ]
    
  selectUiEvent <- el "div" uiSelector
  (actionEvents, bindingUpdatedEvent) <- el "div" $
    valueWidget lookupHieVal val valueUpdateEvent uiUpdateEvent

  bindingDyn <- holdDyn val bindingUpdatedEvent

  return (actionEvents, bindingDyn)

valueWidget ::
  forall t m.
  (MonadHold t m, MonadWidget t m) =>
  (String -> Dynamic t (Maybe (HieValue UiDomain))) ->
  HieValue UiDomain ->
  Event t (PolyDyn) ->
  Event t (Term UiDomain) ->
  m (HieSessionActions t UiDomain, Event t (HieValue UiDomain))
valueWidget lookupHieVal val valueUpdateEvent uiUpdateEvent = do

  uiDyn <- holdDyn (hieUi val) uiUpdateEvent

  x <- dyn =<< mapDyn (\ui -> do
                     uiEnv' <- uiEnv
                     case lookupUiEnv uiEnv' ui (hieValue val, valueUpdateEvent) lookupHieVal of
                         Nothing -> do
                           text "Designated UI is nonexistant or incompatible"
                           return (noAction, never)
                         Just es -> es
                         ) uiDyn

  sessionActions <- switchSessionActions (HieSessionActions never) (fmap (\(e,_) -> e) x)
  valUpdateEndoEvent <- switchPromptly never (fmap (\(_,e) -> e) x)
  return (
    sessionActions,
    (\(ui, pd) -> HieValue pd ui) <$> attachDyn uiDyn valUpdateEndoEvent)

switchSessionActions ::
  (MonadHold t m, Reflex t) =>
  HieSessionActions t uidomain ->
  Event t (HieSessionActions t uidomain) ->
  m (HieSessionActions t uidomain)
switchSessionActions acts ev = HieSessionActions <$> switchPromptly (updateBindings acts) ((fmap updateBindings) ev)

switchSessionActionsDyn ::
  (MonadHold t m, Reflex t) =>
  Dynamic t (HieSessionActions t uidomain) ->
  m (HieSessionActions t uidomain)
switchSessionActionsDyn actsDyn = do
  updateDyn <- mapDyn updateBindings actsDyn
  return (HieSessionActions $ switchPromptlyDyn updateDyn)

uiSelector ::
  (Reflex t, MonadWidget t m) =>
  m (Event t (Term UiDomain))
uiSelector = do

  (uiListTextLabelBtn, _) <- el' "button" (text "List[TextLabel]")
  (uiTextLabelBtn, _) <- el' "button" (text "TextLabel")
  (uiNonShowableBtn, _) <- el' "button" (text "NonShowable")
  (uiFuncTextLabelBtn, _) <- el' "button" (text "Func[TextLabel]")

  return $ leftmost $
    [
      (uiNonShowable) <$ domEvent Click uiNonShowableBtn,
      (uiTextLabel) <$ domEvent Click uiTextLabelBtn,
      (uiList uiTextLabel) <$ domEvent Click uiListTextLabelBtn,
      (uiFunc "f" uiTextLabel) <$ domEvent Click uiFuncTextLabelBtn
    ]
