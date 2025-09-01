-- Produce Rust code from the AST.
module IR0.Pretty (prettyProg) where

import Common.Pretty
import Data.Fix
import GHC.Generics
import IR0.Term
import Prettyprinter
import Prettyprinter.Render.Terminal

prettyProg :: Prog -> Doc AnsiStyle
prettyProg prog =
  vsep [annotate (italicized <> colorDull White) "// IR0 (Rust)", vsep (fmap prettyFunc prog)]

prettyFunc :: Func -> Doc AnsiStyle
prettyFunc (Func name params returnType stmts) = do
  let sig = keyword "fn" <+> prettyFuncIdent name <> parens (commas (map (\(v, a) -> keyword "mut" <+> prettyVarIdent v <> ":" <+> prettyType a) params)) <+> "->" <+> prettyType returnType
  let body = braceBlock (prettyStmts stmts)
  sig <+> body <> "\n"

prettyStmts :: [Stmt] -> Doc AnsiStyle
prettyStmts = vsep . map prettyStmt

prettyStmt :: Stmt -> Doc AnsiStyle
prettyStmt = \case
  Set v x -> prettyVarIdent v <+> "=" <+> prettyExpr x <> ";"
  SMatch x cs -> keyword "match" <+> prettyExpr x <+> braceBlock (prettyClauses $ map (fmap (braceBlock . prettyStmts)) cs)
  SLet v x -> keyword "let" <+> prettyVarIdent v <> ":" <+> constant "isize" <+> "=" <+> prettyExpr x <> ";"
  Loop ss -> keyword "loop" <+> braceBlock (prettyStmts ss)
  Ret x -> keyword "return" <+> prettyExpr x <> ";"

prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr = foldFix $ \case
  L1 x -> prettyCallF x
  R1 x -> prettyCoreF x