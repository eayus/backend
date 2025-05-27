module IR2.Pretty where

import Common.Pretty
import IR2.Term
import Prettyprinter
import Prettyprinter.Render.Terminal

prettyProg :: Prog -> Doc AnsiStyle
prettyProg prog =
  vsep [annotate (italicized <> colorDull White) "// IR2", prettyProgF (fmap prettyFunc prog)]

prettyFunc :: Func -> Doc AnsiStyle
prettyFunc (Func name params ret body) =
  keyword "let"
    <+> keyword "rec"
    <+> prettyFuncIdent name
    <> parens (commas (map (\(v, a) -> prettyVarIdent v <> ":" <+> prettyType a) params))
    <+> ":"
    <+> prettyType ret
    <+> "=\n"
    <> indent 4 (prettyExpr body)
    <> "\n"

prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr = \case
  Expr x -> prettyExprF (fmap prettyExpr x)
  ELetRec func cont -> prettyFunc func <> prettyExpr cont