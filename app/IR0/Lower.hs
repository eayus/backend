module IR0.Lower (lower) where

import Common.Term
import IR0.Term

lower :: Prog -> String
lower = concatMap genRust

genRust :: Func -> String
genRust (Func name params stmts) = do
  let sig = "fn " ++ name ++ "(" ++ concatMap (\v -> "mut " ++ v ++ " : isize, ") params ++ ") -> isize"
  let body = " {\n" ++ genStmts stmts ++ "}\n\n"
  sig ++ body

genStmts :: [Stmt] -> String
genStmts = concatMap genStmt

genStmt :: Stmt -> String
genStmt = \case
  Set v x -> v ++ " = " ++ genExpr x ++ ";\n"
  SIf x t f -> "if " ++ genExpr x ++ "{\n" ++ genStmts t ++ "} else {\n" ++ genStmts f ++ "}"
  SLet v x -> "let " ++ v ++ " : isize = " ++ genExpr x ++ ";\n"
  Loop ss -> "loop {\n " ++ genStmts ss ++ "}\n"
  Ret x -> "return " ++ genExpr x ++ ";\n"

genExpr :: Expr -> String
genExpr = \case
  Var v -> v
  Prim (Int n) -> show n
  Prim (Add x y) -> parens $ genExpr x ++ " + " ++ genExpr y
  Prim (Sub x y) -> parens $ genExpr x ++ " - " ++ genExpr y
  Prim (GreaterThan x y) -> parens $ genExpr x ++ " > " ++ genExpr y
  If x y z -> parens $ "if " ++ genExpr x ++ "{" ++ genExpr y ++ "} else {" ++ genExpr z ++ "}"
  Let v x y -> "{ let " ++ v ++ " = " ++ genExpr x ++ "; " ++ genExpr y ++ "}"
  Call f xs -> f ++ "(" ++ concatMap (\x -> genExpr x ++ ", ") xs ++ ")"

parens :: String -> String
parens s = "(" ++ s ++ ")"