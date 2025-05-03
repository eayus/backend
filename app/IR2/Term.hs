module IR2.Term where

import Common.Term

-- Mutually recursive top-level functions
-- Local functions may shadow other functions!

type Prog = [Func]

data Func = Func
  {name :: FuncIdent, params :: [VarIdent], body :: Expr}

data Expr
  = Expr (ExprF Expr)
  | ELetRec {func :: Func, cont :: Expr}
