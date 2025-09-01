module IR1.Term where

import Common.Term
import Data.Fix
import GHC.Generics

-- Every function name must be unique. Functions are all mutually recursive.

type Prog = [Func]

type Func = FuncF Expr

type ExprF = CallF :+: CoreF

type Expr = Fix ExprF