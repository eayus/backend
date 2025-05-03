module IR1.Term where

import Common.Term
import Data.Fix

type Expr = Fix ExprF

data Func = Func
  { name :: FuncIdent,
    params :: [VarIdent],
    body :: Expr
  }

-- Every function name must be unique. Functions are all mutually recursive.
type Prog = [Func]