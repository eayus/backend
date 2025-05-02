module IR1.Term where

import Common.Term

data Expr
  = Var Ident
  | Prim (Prim Expr)
  | If Expr Expr Expr
  | Let Ident Expr Expr
  | Call Ident [Expr]

data Func = Func
  { name :: Ident,
    params :: [Ident],
    body :: Expr
  }

type Prog = [Func]