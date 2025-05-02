module IR0.Lower (lower) where

import Common.Term
import Common.Pretty
import IR0.Term
import Prettyprinter
import Prettyprinter.Render.Terminal

lower :: Prog -> Doc AnsiStyle
lower = vsep . map genFunc

genFunc :: Func -> Doc AnsiStyle
genFunc (Func name params stmts) = do
  let sig = keyword "fn" <+> pretty name <> parens (commas (map (\v -> keyword "mut" <+> pretty v <> ":" <+> constant "isize") params)) <+> "->" <+> constant "isize"
  let body = "{\n" <> indent 4 (genStmts stmts) <> "\n}"
  sig <+> body <> "\n"

genStmts :: [Stmt] -> Doc AnsiStyle
genStmts = vsep . map genStmt

genStmt :: Stmt -> Doc AnsiStyle
genStmt = \case
  Set v x -> pretty v <+> "=" <+> genExpr x <> ";"
  SIf x t f -> keyword "if" <+> genExpr x <+> "{\n" <> indent 4 (genStmts t) <> "\n}" <+> keyword "else" <+> "{\n" <> indent 4 (genStmts f) <> "\n}"
  SLet v x -> keyword "let" <+> pretty v <> ":" <+> constant "isize" <+> "=" <+> genExpr x <> ";"
  Loop ss -> keyword "loop" <+> "{\n" <> indent 4 (genStmts ss) <> "\n}"
  Ret x -> keyword "return" <+> genExpr x <> ";"

genExpr :: Expr -> Doc AnsiStyle
genExpr = \case
  Var v -> pretty v
  Prim (Int n) -> literal n
  Prim (Add x y) -> parens $ genExpr x <+> "+" <+> genExpr y
  Prim (Sub x y) -> parens $ genExpr x <+> "-" <+> genExpr y
  Prim (GreaterThan x y) -> parens $ genExpr x <+> ">" <+> genExpr y
  If x y z -> parens $ keyword "if" <+> genExpr x <+> "{" <+> genExpr y <+> "}" <+> keyword "else" <+> "{" <+> genExpr z <+> "}"
  Let v x y -> "{ let " <+> pretty v <+> "=" <+> genExpr x <+> "; " <+> genExpr y <+> "}"
  Call f xs -> pretty f <+> "(" <+> commas (map genExpr xs) <+> ")"
