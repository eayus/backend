module IR2.Term where

import Common.Term

-- Mutually recursive top-level functions
-- Local functions may shadow other functions!

type Prog = ProgF Func

data Func = Func
  {name :: Ident IFunc, params :: [(Ident IVar, Type)], returnType :: Type, body :: Expr}

data Expr
  = Expr (ExprF Expr)
  | ELetRec {func :: Func, cont :: Expr}
