module Graphics.HIE.Eval where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Data.Typeable
import Data.Dynamic

import Graphics.HIE.AST

-- Using only 'ReaderT' with 'local' we can't properly close over variables. We
-- need to do something like drawing fresh variables together with a garbage
-- collector.
type HieEval = ReaderT Environment (Except String)
type Environment = M.Map Name HieE

evaluateHieE :: HieE -> HieEval HieE
evaluateHieE (Application e1 e2) = do
  e1' <- evaluateHieE e1
  case e1' of
    Lambda v e3 -> do
      -- return $ Closure (v, e2) e3
      local (M.insert v e1') (evaluateHieE e3)
    Primitive v a xs -> evaluateHieE (Primitive v a (xs++[e2]))
    _ -> throwError "Type error in Application-case of evaluateHieE"

{-
evaluateHieE (Closure (v, e1) e2) = do
  local (M.insert v e1) (evaluateHieE e2)
  -}

evaluateHieE (Primitive p arity xs) | arity == length xs = do
  xs' <- mapM (interpHieE <=< evaluateHieE) xs
  res <- foldM (\f x -> t $ dynApply f x) p xs'
  return $ interpHs res
  where
    t = maybe (throwError "Type error in Primitive-case of evaluateHieE") return

evaluateHieE (Variable v) = evaluateHieE =<< t . M.lookup v =<< ask
  where
    t = maybe (throwError $ "Unknown variable: " ++ v) return

evaluateHieE e1 = return e1 -- already evaluated fully.

-- Interpret HieE values as Haskell values
interpHieE :: HieE -> HieEval Dynamic
interpHieE (Lambda _ _) = error "Interpreting hie-λ as haskell-λ is not implemented yet. See comment."
{-
interpHieE (Lambda _ _)@e1 = do
  env <- ask
  return $ toDyn $ \x -> -- <<-- We need a way to make 'Dynamic' functions, with TyArgs given as TypeReps dynamically. See below.
    case runReaderT (go x) env of
      Left e  -> error e
      Right r -> r
  where
    go :: Dynamic -> HieEval Dynamic
    go x = do
      x' <- interpHs x
      r <- evaluateHieE (Application e1 x')
      interpHieE r
{-

We need:

dynFun :: TypeRep -> (Dynamic -> Dynamic) -> Dynamic

where

dynTypeRep (dynFun ty f) === ty

and 'dynFun ty' is only defined when ty is an arrow type.

-}
-}

interpHieE (Primitive p _ _) = return $ toDyn p
interpHieE e1 = error $ "interpHieE: unhandled case for " ++ show e1

-- Interpret Haskell values as HieE values. Functions become primitives, as
-- they're not inspectable.
interpHs :: Dynamic -> HieE
interpHs v = Primitive v (funArity $ dynTypeRep v) []

funArity :: TypeRep -> Int
funArity ty | typeRepTyCon ty == arrowTyCon = 1 + funArity (typeRepArgs ty !! 1)
funArity _ = 0

arrowTyCon :: TyCon
arrowTyCon = typeRepTyCon (mkFunTy (typeOf ()) (typeOf ()))

intTy :: TypeRep
intTy = typeRep (Proxy :: Proxy Int)

prim :: Typeable a => a -> HieE
prim = interpHs . toDyn
