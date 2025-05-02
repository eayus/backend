module IR0.Lower (lower) where

import Common.Pretty
import Data.Fix
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
genExpr = foldFix prettyExprF