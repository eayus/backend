module IR0.Term where

import Common.Term
import Data.Fix

type Expr = Fix ExprF

data Stmt
  = Set VarIdent Expr
  | SMatch Expr [(VarIdent, [Stmt])]
  | SLet VarIdent Expr
  | Loop [Stmt]
  | Ret Expr
  deriving (Show)

data Func = Func
  { name :: FuncIdent,
    params :: [VarIdent], -- Params are mutable
    stmts :: [Stmt]
  }
  deriving (Show)

-- Every function name must be unique. Functions are all mutually recursive.
type Prog = [Func]