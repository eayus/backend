module IR2.Pretty where

import Common.Pretty
import Common.Term
import Data.Fix
import GHC.Generics
import IR2.Term
import Prettyprinter
import Prettyprinter.Render.Terminal

prettyProg :: Expr -> Doc AnsiStyle
prettyProg prog =
  vsep [annotate (italicized <> colorDull White) "// IR2", prettyExpr prog]

prettyFunc :: Func -> Doc AnsiStyle
prettyFunc (FuncF name params ret body) =
  keyword "fn"
    <+> prettyFuncIdent name
    <> parens (commas (map (\(v, a) -> prettyVarIdent v <> ":" <+> prettyType a) params))
    <+> "->"
    <+> prettyType ret
    <+> "{\n"
    <> indent 4 (prettyExpr body)
    <> "\n}\n\n"

prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr (Fix e) = case e of
  L1 (LetFuncF func cont) -> prettyFunc func <> prettyExpr cont
  R1 (L1 x) -> prettyCallF $ fmap prettyExpr x
  R1 (R1 x) -> prettyCoreF $ fmap prettyExpr x