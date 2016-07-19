{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
    ( hieJsMain
    ) where

import Data.Functor.Misc
import Data.Proxy
import Data.Time
import Reflex.Dom
import qualified Data.Map as M
import Data.Constraint

{-

Status:
* På vejen til det minimale, acceptable produkt mangler vi endnu:
  * typestærkt, at kunne indføre bindinger
  * typestærkt, at kunne tildele ui til bindinger
-}

{-
# Andet eksperiment ad 'ui til bindinger':
* vi vil angive et dynamisk, typedirigeret ui-univers
-}

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Comp
import Data.Typeable
import Data.Proxy
import Safe
import GHC.Prim
import GHC.TypeLits
import Unsafe.Coerce

-- uidomains:
data UiNonShowable e = UiNonShowable
  deriving (Eq, Functor)

data UiTextLabel e = UiTextLabel
  deriving (Eq, Functor)

uiTextLabel :: (UiTextLabel :<: ui) => Term ui
uiTextLabel = Term $ inj UiTextLabel

instance EqF UiTextLabel where
  eqF _ _ = True

data UiList e = UiList e
  deriving (Eq, Functor)

instance EqF UiList where
  eqF _ _ = True

uiList :: (UiList :<: ui) => Term ui -> Term ui
uiList x = Term $ inj (UiList x)

data UiFunc e = UiFunc e
  deriving (Eq, Functor)

instance EqF UiFunc where
  eqF _ _ = True

data UiImplementation uidomain where
  Instance ::
    Const uidomain -> TySigRep ->
    (
      forall a.
      Term uidomain -> (TypeRep, a) -> 
      ReaderT (UiLookup uidomain) IO ()
    ) ->
    UiImplementation uidomain

{-

* An experiment to encapsulate the dynamic matching with a typesig, to
  centralise the code that needs to use unsafeCoerce in a single,
  trusted place.

* It's an open question how to handle non-linear type variable occurences.
  Though I imagine we could build a type familiy that records the
  variables in a type signature in a list and generate equality
  constraints, eg '(TyVar n a -> TyVar n b) ---> Forall (n : a, n :b)
  ---> a ~ b)

data PolyDyn where
  PolyDyn :: TypeRep -> a -> PolyDyn

type family TypeSigType (sig :: *) :: * where
  TypeSigType (TyVar name a) = a
  TypeSigType (k a b c d) = k (TypeSigType a) (TypeSigType b) (TypeSigType c) (TypeSigType d)
  TypeSigType (k a b c) = k (TypeSigType a) (TypeSigType b) (TypeSigType c)
  TypeSigType (k a b) = k (TypeSigType a) (TypeSigType b)
  TypeSigType (k a) = k (TypeSigType a)

matchPolyDyn :: PolyDyn -> Proxy a -> (TypeSigType a -> b) -> Maybe b
matchPolyDyn (PolyDyn tx x) f =  undefined
newtype TyVarPoly (name :: Symbol) a = MkTyVar { unTyVar :: a }

-}

-- Match a TypeRep with a TySig. Occurences of TyVar in TySig
-- match anything. Though TyVar is equipped with a name, it is not
-- yet used. Thus, '(Int, String)' unifies with '(TyVar "a", TyVar
-- "a")'
tySigMatch :: TypeRep -> TySigRep -> Bool
tySigMatch (Ty' c1 as1) (TyS' c2 as2) | c1 == c2 = and (zipWith tySigMatch as1 as2)
tySigMatch ty           (TyVar' name) = True -- TODO: collect constraints (name : ty)

-- Pattern matching for TypeRep and TySig.
pattern Ty' con args <- (splitTyConApp -> (con, args))
pattern TyS' con args <- (fmap (map TySigRep) . splitTyConApp . unTySig -> (con, args))
pattern TyVar' v <-
  TyS'
    ((== typeRepTyCon (typeRep (Proxy :: Proxy (TyVar "")))) -> True)
    [TyS' (tyConName -> v) []]

-- Type signatures for matching 'UiImplementation's. Use 'TyVar'
-- polymorphic 'UiImplementation's.
newtype TySigRep = TySigRep { unTySig :: TypeRep }

  -- TODO: Der er et liiiidt kompliceret samspil mellem TyVar og TyVarPoly ...
data TyVar (name :: Symbol)
  deriving (Typeable)

-- | Construct a 'TySig'. It is restricted to kind Star. Use 'TyVar'
-- in place of type variables.
tySig :: (Typeable (a :: *)) => Proxy a -> TySigRep
tySig = TySigRep . typeRep

data UiLookup uidomain where
  UiLookup :: 
    (Const uidomain -> TypeRep -> Maybe (UiImplementation uidomain)) -> UiLookup uidomain

type SessionT uidomain =
  ReaderT (UiLookup uidomain) IO 

runUi ::
  (Typeable a, Functor uidomain) =>
  String -> a -> Term uidomain -> SessionT uidomain ()
runUi name x ui@(Term ui') = do
  UiLookup look <- ask
  Just (Instance _ _ innerUiImpl) <- return $ look (() <$ ui') (typeOf x)
  liftIO $ putStrLn $ name ++ ":"
  innerUiImpl ui (typeOf x, x)

uiTextLabelImpl ::
  (UiTextLabel :<: uidomain, Typeable a, Show a) =>
  Proxy a -> UiImplementation uidomain
uiTextLabelImpl (px@Proxy :: Proxy a) =
  Instance (inj UiTextLabel) (tySig $ px) $
    \_ (_, str) -> liftIO $ print (unsafeCoerce str :: a)

uiListImpl :: (Functor uidomain, UiList :<: uidomain) => UiImplementation uidomain
uiListImpl = Instance (inj $ UiList ()) (tySig (Proxy :: Proxy [TyVar "a"])) $
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

runSession ::
  (Eq (Const uidomain)) =>
  [UiImplementation uidomain] -> SessionT uidomain () -> IO ()
runSession impls act = runReaderT act (UiLookup uiLookup)
  where
    -- TODO: What's wrong with writing the signature below?
    --uiLookup :: Const uidomain -> TypeRep -> Maybe (UiImplementation uidomain)
    uiLookup ui ty =
      headMay $ filter (\(Instance ui' tyS _) -> ui' == ui && tySigMatch ty tyS) impls

test :: IO ()
test =
  runSession
    [
      uiTextLabelImpl (Proxy :: Proxy Int),
      uiListImpl :: UiImplementation (UiList :+: UiTextLabel)]
    (runUi "Foo" [42, 43, 44 :: Int] (uiList uiTextLabel))

{-
# Første eksperiment ad 'ui til bindinger':
* vi vil bruge typeklasser
* en typeklasse <-> en ui-variant
* instanser af en ui-typeklasse kan tegnes med den.

* typeklasser er valgt fremfor et 'Map Ui (Map TypeRep/DSL-ish (IO ()))'-ish 
  fordi det
* komponener automatisk på oversættelsestidspunktet,
* og er ret udtryksfuldt. Vi har fx support for polymorfiske værdier

* ... Desværre er det også temmelig svært, og har sine egne uigennemskuelige
  begrænsninger...


class ImplementsUi (b :: Bool) (ui :: * -> Constraint) (a :: *) where
  implementsUi :: Proxy (ui a) -> Maybe (Dict (ui a))

instance (ui a) => ImplementsUi 'True ui a where
  implementsUi _ = Just Dict
instance (b ~ 'False) => ImplementsUi b ui a where
  implementsUi _ = Nothing

type family EntireDomain (a :: *) (uidomain :: [* -> Constraint]) :: Constraint
type instance EntireDomain a (c ': '[]) = c a
type instance EntireDomain a (c ': c' ': cs) = (c a, EntireDomain a (c' ': cs))

{-
* Encoding ui type class constraints in HieValue seems to work fine,
  except for function types.
-}
data HieValue a (uidomain :: [* -> Constraint]) where
  HieBaseValue :: {- Ui?? -> -} Dict (EntireDomain a uidomain) -> a -> HieValue a uidomain

data SessionT (uidomain :: [* -> Constraint]) where
  SessionStub :: Maybe (HieValue a uidomain) -> SessionT uidomain

  {-
data FunRecDict a (uidomain :: [* -> Constraint]) where
  FunRecDictBase :: Dict (EntireDomain a uidomain) -> FunRecDict a uidomain
  FunRecDictRec  ::
    Dict (EntireDomain a uidomain) ->
    Dict (EntireDomain b uidomain) ->
    FunRecDict (a -> b) uidomain
-}
  
class Appliable a where
  -- applyUi :: (a -> b) -> a -> b

applyFuncAndBindUi ::
  HieValue (a -> b) uidomain -> HieValue a uidomain -> HieValue b uidomain
applyFuncAndBindUi = undefined

test :: IO ()
test = do
  s <- return undefined
  s1 <- addBindingToSession s "foo"
    (HieBaseValue (Dict :: Dict (EntireDomain Int '[ImplementsUi b Show])) 42)
  showableUi s1

addBindingToSession ::
  SessionT uidomain -> String -> HieValue a uidomain -> IO (SessionT uidomain)
addBindingToSession _ _ v = return $ SessionStub (Just v)

showableUi :: SessionT '[ImplementsUi b Show] -> IO ()
showableUi (SessionStub (Just (HieBaseValue Dict x))) = print x

-}

-- * Reflex-based ui, where HieValue ~ Int

data Session t = Session {
    sessionModel :: Dynamic t (M.Map String (Dynamic t (UiStyle, Int)))
    }

data UiStyle = UiShow | UiBlackBox

-- A toy session currently.
connectSession ::
  MonadWidget t m =>
  MonadHold t m =>
  Dynamic t (M.Map String (Event t UiStyle)) -> m (Session t)
connectSession bindingEventsDyn = do

  bindingEvents <- switchPromptlyDyn <$> mapDyn mergeMap bindingEventsDyn
  let fanBindings = fanMap bindingEvents

  incrementEvent <- tickLossy 1 (UTCTime (toEnum 0) 0)
  fooValDyn  <- count incrementEvent
  foo1UiDyn <- holdDyn UiShow (select fanBindings (Const2 "foo1"))
  foo1BindingDyn <- combineDyn (,) foo1UiDyn fooValDyn

  foo2UiDyn <- holdDyn UiBlackBox (select fanBindings (Const2 "foo2"))
  foo2BindingDyn <- combineDyn (,) foo2UiDyn fooValDyn

  Session <$> pure (constDyn $ M.fromList [
                       ("foo1", foo1BindingDyn),
                       ("foo2", foo2BindingDyn),
                       ("bar", constDyn (UiShow, 42))
                       ])

hieJsMain :: IO ()
hieJsMain = mainWidget $ mdo

  el "h1" (text "HIE Document")

  Session model <- connectSession bindingEvents

  bindingEvents <- listWithKey model (\name val -> renderBinding name (joinDyn val))

  return ()

renderBinding ::
  MonadWidget t m =>
  String -> Dynamic t (UiStyle, Int) -> m (Event t UiStyle)
renderBinding name val = el "div" $ do
  
  el "h2" (text name)
  _ <- dyn =<< mapDyn renderUi val
  uiSelector

renderUi :: MonadWidget t m => (UiStyle, Int) -> m ()
renderUi (UiShow, val) = el "div" (text $ show val)
renderUi (UiBlackBox, _) = elAttr "div"
                        (M.fromList
                            [
                            ("style","background: black; width: 200px; height: 20px;")
                            ]) blank


uiSelector :: (Reflex t, MonadWidget t m) => m (Event t UiStyle)
uiSelector = do

  (uiShowBtn,_) <- el' "button" (text "Show")
  (uiBlackBox, _) <- el' "button" (text "Black Box")

  return $ leftmost
    [
      UiShow <$ domEvent Click uiShowBtn,
      UiBlackBox <$ domEvent Click uiBlackBox
    ]
