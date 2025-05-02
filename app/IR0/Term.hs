module IR0.Term where

import Common.Term
import Data.Fix

type Expr = Fix ExprF

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