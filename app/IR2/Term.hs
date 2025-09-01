module IR2.Term where

import Common.Term
import Data.Fix
import GHC.Generics

-- Mutually recursive top-level functions
-- Local functions may shadow other functions!

type Prog = [Func]

type Func = FuncF Expr

type ExprF = LetFuncF :+: CallF :+: CoreF 

type Expr = Fix ExprF