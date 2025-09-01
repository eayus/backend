module IR0.Term where

import Common.Term
import Data.Fix

type Expr = Fix ExprF

data Stmt
  = Set (Ident IVar) Expr
  | SMatch Expr [ClauseF [Stmt]]
  | SLet (Ident IVar) Expr
  | Loop [Stmt]
  | Ret Expr
  deriving (Show)

data Func = Func
  { name :: Ident IFunc,
    params :: [(Ident IVar, Type)], -- Params are mutable
    returnType :: Type,
    stmts :: [Stmt]
  }
  deriving (Show)

-- Every function name must be unique. Functions are all mutually recursive.
type Prog = [Func]