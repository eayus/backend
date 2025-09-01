module IR2.Term where

import Common.Term
import Data.Fix
import GHC.Generics

type ExprF = LetFuncF :+: CallF :+: CoreF 

type Expr = Fix ExprF

type Func = FuncF Expr