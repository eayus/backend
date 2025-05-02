module IR0.Term where

import Common.Term

data Expr
  = Var Ident
  | If Expr Expr Expr
  | Let Ident Expr Expr
  | Call Ident [Expr]
  | Prim (Prim Expr)
  deriving (Show)

data Stmt
  = Set Ident Expr
  | SIf Expr [Stmt] [Stmt]
  | SLet Ident Expr
  | Loop [Stmt]
  | Ret Expr
  deriving (Show)

data Func = Func
  { name :: Ident,
    params :: [Ident], -- Params are mutable
    stmts :: [Stmt]
  }
  deriving (Show)

type Prog = [Func]