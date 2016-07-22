{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
    {-( hieJsMain
    )-} where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Comp
import Data.Functor.Misc
import Data.Monoid
import Data.Proxy
import Data.Time
import Data.Typeable
import Data.Type.Equality
import GHC.TypeLits
import Reflex.Dom
import Safe
import Unsafe.Coerce
import qualified Data.Map as M

{-

* An experiment to encapsulate the dynamic matching with a typesig, to
  centralise the code that needs to use unsafeCoerce in a single,
  trusted place.

* It's an open question how to handle non-linear type variable occurences.
  Though I imagine we could build a type familiy that records the
  variables in a type signature in a list and generate equality
  constraints, eg '(PolyVar n a -> TyVar n b) ---> Forall (n : a, n :b)
  ---> a ~ b)
-}

data PolyDyn where
  PolyDyn :: TypeRep -> a -> PolyDyn

polyDyn :: Typeable a => a -> PolyDyn
polyDyn x = PolyDyn (typeOf x) x

data TyVar (name :: Symbol) = MkTyVar { unTyVar :: forall a. a }

{-
* The current TypeEnv-approach is not type safe, as you can pick any
  concrete type instead of existentially quantified types if you
  like. This is an attempt to fix this.
newtype PolyVar (name :: Symbol) a = MkPolyVar { unPolyVar :: a }

data TypeEnv where
  TypeEnv :: Symbol -> a -> TypeEnv

type family TypeSigType (sig :: k) (env :: [TypeEnv]) :: k where
  TypeSigType (TyVar name) env  = PolyVar name (LookupEnv name env)
  TypeSigType ((c :: * -> k) (a :: *)) env = (TypeSigType c env) (TypeSigType a env)
  TypeSigType a env = a

type family LookupEnv (name :: Symbol) (env :: [TypeEnv]) :: a where
  LookupEnv name ('TypeEnv name a ': e) = a
  LookupEnv name ('TypeEnv name' a ': e) = LookupEnv name e


forall' :: Proxy name -> (forall a. Proxy ('TypeEnv name a) -> b) -> b
forall' = undefined

andforall' ::
  Proxy name -> (forall a. Proxy ('TypeEnv name a ': env) -> b) -> Proxy env -> b
andforall' = undefined
-}

-- Type signatures for matching 'UiImplementation's. Use 'PolyVar'
-- polymorphic 'UiImplementation's.
newtype TySigRep = TySigRep { unTySig :: TypeRep }

-- | Construct a 'TySig'. It is restricted to kind Star. Use 'PolyVar'
-- in place of type variables.
tySig :: (Typeable (a :: *)) => Proxy a -> TySigRep
tySig = TySigRep . typeRep

-- Pattern matching for TypeRep and TySigRep.
pattern Ty' con args <- (splitTyConApp -> (con, args))
pattern TySig' con args <- (fmap (map TySigRep) . splitTyConApp . unTySig -> (con, args))
pattern TyVar' v <-
  TySig'
    ((== typeRepTyCon (typeRep (Proxy :: Proxy (TyVar "")))) -> True)
    [TySig' (tyConName -> v) []]

-- Match a TypeRep with a TySig. Yields a mapping from PolyVar names to
-- corresponding TypeReps.
tySigMatch :: TypeRep -> TySigRep -> Maybe (M.Map String TypeRep)
tySigMatch (Ty' c1 a1) (TySig' c2 a2)
  | c1 == c2 && length a1 == length a2
  -- TODO: Throw up if unifying the same name to different TypeReps.
  = foldr M.union M.empty <$> (sequence $ zipWith tySigMatch a1 a2)
tySigMatch ty           (TyVar' name) = Just (M.singleton name ty)
tySigMatch _ _ = Nothing

matchPolyDyn ::
  forall a b. Typeable a =>
  PolyDyn ->
  (ToPolyDyn -> a -> b) ->
  Maybe b
matchPolyDyn (PolyDyn tx x) f 
  | Just typeRepEnv <- tySigMatch tx (tySig (Proxy :: Proxy a)) = Just (f (toDyn typeRepEnv) (unsafeCoerce x))
  where
    toDyn :: M.Map String TypeRep -> (KnownSymbol name => TyVar name -> PolyDyn)
    toDyn env (MkTyVar x' :: TyVar name) =
      let
        pn = (Proxy :: Proxy name)
        name = symbolVal pn
      in case M.lookup name env of
        Just tx' -> PolyDyn tx' x'
        Nothing -> error $ "PolyVar " ++ name ++ " is not present in type map"
    
matchPolyDyn _ _ = Nothing

type ToPolyDyn = forall name. KnownSymbol name => TyVar name -> PolyDyn
newtype ToPolyDyn' = ToPolyDyn' ToPolyDyn

withPolyDynEnv ::
  (Typeable a) =>
  [PolyDyn] ->
  (ToPolyDyn -> a -> b) ->
  Maybe b
withPolyDynEnv ds f =
  getFirst $ mconcat $ map (\d -> First $ matchPolyDyn d f) ds

data UiImpl ui uidomain t m where
  UiImpl ::
    (Typeable sig) =>
    (
      ui (),
      ui (Term uidomain) ->
      ToPolyDyn' ->
      sig ->
      m ()
    ) -> UiImpl ui uidomain t m

type UiEnv uidomain t m =
  [Term uidomain -> PolyDyn -> Maybe (m ())]

lookupUiEnv ::
  Eq (Const uidomain) =>
  UiEnv uidomain t m ->
  Term uidomain -> 
  PolyDyn ->
  Maybe (m ())
lookupUiEnv env ui pd = getFirst $ mconcat $ map (\f -> First $ f ui pd) env

wrapUiImpl ::
  forall uidomain ui t m.
  (Eq (Const uidomain), Functor ui, EqF ui, Functor uidomain, ui :<: uidomain, MonadWidget t m) =>
  UiImpl ui uidomain t m -> 
  UiEnv uidomain t m
wrapUiImpl (UiImpl (key, impl)) =
  [(
  (\(Term ui) pd -> do
      ui' <- proj ui
      guard (eqF (() <$ ui') key)
      go ui' pd
  ))]

  where

    go :: ui (Term uidomain) -> PolyDyn -> Maybe (m ())
    go ui pd = matchPolyDyn pd (\toDyn d -> impl ui (ToPolyDyn' toDyn) d)
 
-- uidomains:
data UiNonShowable e = UiNonShowable
  deriving (Eq, Functor)

data UiTextLabel e = UiTextLabel
  deriving (Eq, Functor)

instance EqF UiTextLabel where
  eqF _ _ = True

data UiList e = UiList e
  deriving (Eq, Functor)

instance EqF UiList where
  eqF _ _ = True

data UiFunc e = UiFunc e
  deriving (Eq, Functor)

instance EqF UiFunc where
  eqF _ _ = True

uiNonShowable :: (UiNonShowable :<: ui) => Term ui
uiNonShowable = Term $ inj UiNonShowable

uiTextLabel :: (UiTextLabel :<: ui) => Term ui
uiTextLabel = Term $ inj UiTextLabel

uiTextLabelImpl ::
  forall uidomain t m.
  (Functor uidomain, UiTextLabel :<: uidomain, MonadWidget t m) =>
  UiImpl UiTextLabel uidomain t m
uiTextLabelImpl = UiImpl (UiTextLabel, go)
  where
    go :: UiTextLabel (Term uidomain) -> ToPolyDyn' -> String -> m ()
    go _ _ str = text str
    
pattern Proj x <- (proj -> Just x)

uiList :: (UiList :<: ui) => Term ui -> Term ui
uiList x = Term $ inj (UiList x)

  {-
uiListImpl ::
  (Functor uidomain, UiList :<: uidomain, MonadWidget t m) =>
  Term uidomain -> PolyDyn -> m ()
uiListImpl = undefined
-}

  {-
data UiImplementation uidomain where
  Instance ::
    Const uidomain -> TySigRep ->
    (
      forall t m a. MonadWidget t m =>
      Term uidomain -> PolyDyn -> m ()
    ) ->
    UiImplementation uidomain
-}

{-
data UiLookup uidomain where
  UiLookup :: 
    (Const uidomain -> TypeRep -> Maybe (UiImplementation uidomain)) -> UiLookup uidomain

type SessionT uidomain =
  ReaderT (UiLookup uidomain) IO 

uiTextLabelImpl ::
  (UiTextLabel :<: uidomain, Typeable a, Show a) =>
  Proxy a -> UiImplementation uidomain
uiTextLabelImpl (px@Proxy :: Proxy a) =
  Instance (inj UiTextLabel) (tySig $ px) $
    \_ (_, str) -> liftIO $ print (unsafeCoerce str :: a)

-}

{-
uiListImpl :: (Functor uidomain, UiList :<: uidomain) => UiImplementation uidomain
uiListImpl = Instance (inj $ UiList ()) (tySig (Proxy :: Proxy [PolyVar "a"])) $
  \((Term ui)) (ty, l) -> case proj ui of
    Nothing -> return ()
    Just (UiList innerUi@(Term innerUi')) ->
      do
        let [argTy] = typeRepArgs ty

        UiLookup look <- ask
        Just (Instance _ _ innerUiImpl) <- return $ look (() <$ innerUi') argTy
        liftIO $ putStrLn "["
        mapM_ (\x -> innerUiImpl innerUi (argTy, x)) (unsafeCoerce l :: [a])
        liftIO $ putStrLn "]"
-}

{-
runUi ::
  (Typeable a, Functor uidomain) =>
  String -> a -> Term uidomain -> SessionT uidomain ()
runUi name x ui@(Term ui') = do
  UiLookup look <- ask
  Just (Instance _ _ innerUiImpl) <- return $ look (() <$ ui') (typeOf x)
  liftIO $ putStrLn $ name ++ ":"
  innerUiImpl ui (typeOf x, x)

runSession ::
  (Eq (Const uidomain)) =>
  [UiImplementation uidomain] -> SessionT uidomain () -> IO ()
runSession impls act = runReaderT act (UiLookup uiLookup)
  where
    -- TODO: What's wrong with writing the signature below?
    --uiLookup :: Const uidomain -> TypeRep -> Maybe (UiImplementation uidomain)
    uiLookup ui ty =
      headMay $ filter (\(Instance ui' tyS _) -> ui' == ui && tySigMatch ty tyS) impls
-}

data HieValue uidomain where
  HieValue :: PolyDyn -> Term uidomain -> HieValue uidomain

-- * Reflex-based ui

data Session t uidomain = Session {
    sessionModel :: Dynamic t (M.Map String (Dynamic t (HieValue uidomain)))
    }

-- A toy session currently.
type UiDomain = UiList :+: UiTextLabel :+: UiNonShowable
connectSession ::
  MonadWidget t m =>
  MonadHold t m =>
  Dynamic t (M.Map String (Event t (Term UiDomain)))->
  m (Session t UiDomain)
connectSession bindingEventsDyn = do

  bindingEvents <- switchPromptlyDyn <$> mapDyn mergeMap bindingEventsDyn
  let fanBindings = fanMap bindingEvents

  incrementEvent <- tickLossy 1 (UTCTime (toEnum 0) 0)
  fooValDyn <- count incrementEvent >>= mapDyn (polyDyn . show :: Int -> PolyDyn)
  foo1UiDyn <- holdDyn uiTextLabel (select fanBindings (Const2 "foo1"))
  foo1BindingDyn <- combineDyn HieValue fooValDyn foo1UiDyn

  let barValDyn = constDyn (polyDyn (42 :: Int))
  barUiDyn <- holdDyn uiTextLabel (select fanBindings (Const2 "bar"))
  barBindingDyn <- combineDyn HieValue barValDyn barUiDyn

  {-
  foo2UiDyn <- holdDyn uiNonShowable (select fanBindings (Const2 "foo2"))
  foo2BindingDyn <- combineDyn HieValue fooValDyn foo2UiDyn
  -}

  Session <$> pure (constDyn $ M.fromList [
                       ("foo1", foo1BindingDyn),
                       ("bar", barBindingDyn)
                       ])


hieJsMain :: IO ()
hieJsMain = mainWidget $ mdo

  el "h1" (text "HIE Document")
  Session model <- connectSession bindingEvents
  bindingEvents <- listWithKey model (\name val -> renderBinding name (joinDyn val))
  return ()

uiEnv :: forall t m. MonadWidget t m => m (UiEnv UiDomain t m)
uiEnv = return $ concat [wrapUiImpl uiTextLabelImpl]

renderBinding ::
  (MonadWidget t m) =>
  String -> Dynamic t (HieValue UiDomain) -> m (Event t (Term UiDomain))
renderBinding name val = el "div" $ do
  
  el "h2" (text name)
  _ <- dyn =<< mapDyn renderUi val
  uiSelector

renderUi :: MonadWidget t m => HieValue UiDomain -> m ()
renderUi (HieValue pd ui) = do

  uiEnv' <- uiEnv
  case lookupUiEnv uiEnv' ui pd of
    Nothing -> text "Designated UI is nonexistant or incompatible"
    Just m -> m

uiSelector ::
  (Reflex t, MonadWidget t m) =>
  m (Event t (Term UiDomain))
uiSelector = do

  (uiShowBtn, _) <- el' "button" (text "Show")
  (uiBlackBoxBtn, _) <- el' "button" (text "Black Box")

  return $ leftmost
    [
      (uiTextLabel) <$ domEvent Click uiShowBtn,
      (uiNonShowable) <$ domEvent Click uiBlackBoxBtn
    ]
