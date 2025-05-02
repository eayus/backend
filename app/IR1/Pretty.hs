module IR1.Pretty where

import Common.Pretty
import Common.Term
import IR1.Term
import Prettyprinter
import Prettyprinter.Render.Terminal

prettyProg :: Prog -> Doc AnsiStyle
prettyProg = vsep . map prettyFunc

prettyFunc :: Func -> Doc AnsiStyle
prettyFunc (Func name params body) = do
  let sig = keyword "fn" <+> pretty name <> parens (commas (map (\v -> keyword "mut" <+> pretty v <> ":" <+> constant "isize") params)) <+> "->" <+> constant "isize"
  let body' = "{\n" <> indent 4 (prettyExpr body) <> "\n}"
  sig <+> body' <> "\n"

prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr = \case
  Var v -> pretty v
  Prim (Int n) -> literal n
  Prim (Add x y) -> parens $ prettyExpr x <+> "+" <+> prettyExpr y
  Prim (Sub x y) -> parens $ prettyExpr x <+> "-" <+> prettyExpr y
  Prim (GreaterThan x y) -> parens $ prettyExpr x <+> ">" <+> prettyExpr y
  If x y z -> parens $ keyword "if" <+> prettyExpr x <+> "{" <+> prettyExpr y <+> "}" <+> keyword "else" <+> "{" <+> prettyExpr z <+> "}"
  Let v x y -> "{ let " <+> pretty v <+> "=" <+> prettyExpr x <+> "; " <+> prettyExpr y <+> "}"
  Call f xs -> pretty f <+> "(" <> commas (map prettyExpr xs) <> ")"