-- Produce Rust code from the AST.
module IR0.Lower (lower) where

import Common.Pretty
import Data.Bifunctor
import Data.Fix
import IR0.Term
import Prettyprinter
import Prettyprinter.Render.Terminal

lower :: Prog -> Doc AnsiStyle
lower prog = vsep $ annotate (italicized <> colorDull White) "// IR0 (Rust)" : map genFunc prog

genFunc :: Func -> Doc AnsiStyle
genFunc (Func name params stmts) = do
  let sig = keyword "fn" <+> prettyFuncIdent name <> parens (commas (map (\v -> keyword "mut" <+> prettyVarIdent v <> ":" <+> constant "isize") params)) <+> "->" <+> constant "isize"
  let body = braceBlock (genStmts stmts)
  sig <+> body <> "\n"

genStmts :: [Stmt] -> Doc AnsiStyle
genStmts = vsep . map genStmt

genStmt :: Stmt -> Doc AnsiStyle
genStmt = \case
  Set v x -> prettyVarIdent v <+> "=" <+> genExpr x <> ";"
  SMatch x cs -> keyword "match" <+> genExpr x <+> braceBlock (prettyClauses $ map (second (braceBlock . genStmts)) cs)
  SLet v x -> keyword "let" <+> prettyVarIdent v <> ":" <+> constant "isize" <+> "=" <+> genExpr x <> ";"
  Loop ss -> keyword "loop" <+> braceBlock (genStmts ss)
  Ret x -> keyword "return" <+> genExpr x <> ";"

genExpr :: Expr -> Doc AnsiStyle
genExpr = foldFix prettyExprF