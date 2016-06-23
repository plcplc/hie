module Graphics.HIE.AST where

import qualified Data.Map.Strict as M
import Data.Dynamic

-- This should be some sort of extensible union. (Not all combinations
-- necessarily make sense.)
data UI =
  ExpLiteral |
  ShowString |
  VerticalList UI |
  Columns [UI] |
  Date
  deriving Show

type Variable = String
type Arity = Int
type Name = String
data Exp =
  Case Exp [(Pattern, Exp)] UI |
  Constructor Name [Exp] UI |
  Application Exp Exp UI |
  Lambda Name Exp UI |
  Primitive Dynamic Arity UI |
  Variable Name UI
  deriving Show

data Pattern =
  PConstructor Name [Pattern] |
  PVariable Name
  deriving Show

-- Evaluated Expressions
data EExp =
  EClosure !(M.Map Name EExp) EExp UI |
  EPrimitive Dynamic Arity [EExp]{-arguments, if value is a function.-} UI |
  ELambda Name Exp UI -- With no free variables in Exp

{-
data Document = Document {
  hieDocModules :: ![Module]
  }

data Module = Module {
  hieModName   :: !Name,
  hieModValues :: !(M.Map Name Exp)
  }

mkDocument :: [(Name, [(Name, Exp)])] -> Document
mkDocument values = Document (map mkModule values)
  where
    mkModule :: (Name, [(Name, Exp)]) -> Module
    mkModule (name, vs) = Module name (M.fromList vs)
    -}
