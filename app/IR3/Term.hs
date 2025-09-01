module IR3.Term where

import Common.Term
import Data.Fix
import GHC.Generics

type Func = FuncF Expr

data CurryF a
  = AppF a a
  | LamF (Ident IVar) Type Type a -- Arg type, ret type
  deriving (Foldable, Functor, Show, Traversable)

type ExprF = CurryF :+: CoreF

type Expr = Fix ExprF
