module IR1.Pretty where

import Common.Pretty
import Data.Fix
import IR1.Term
import Prettyprinter
import Prettyprinter.Render.Terminal

prettyProg :: Prog -> Doc AnsiStyle
prettyProg prog = vsep $ annotate (italicized <> colorDull White) "// IR1" : map prettyFunc prog

prettyFunc :: Func -> Doc AnsiStyle
prettyFunc (Func name params body) = do
  let sig = keyword "fn" <+> prettyFuncIdent name <> parens (commas (map (\v -> prettyVarIdent v <> ":" <+> constant "isize") params)) <+> "->" <+> constant "isize"
  let body' = "{\n" <> indent 4 (prettyExpr body) <> "\n}"
  sig <+> body' <> "\n"

prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr = foldFix prettyExprF