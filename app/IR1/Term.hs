module IR1.Term where

import Common.Term
import Data.Fix

type Expr = Fix ExprF

data Func = Func
  { name :: Ident,
    params :: [Ident],
    body :: Expr
  }

type Prog = [Func]