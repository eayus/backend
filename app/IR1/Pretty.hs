module IR1.Pretty where

import Common.Pretty
import Common.Term
import Data.Fix
import GHC.Generics
import IR1.Term
import Prettyprinter
import Prettyprinter.Render.Terminal

prettyProg :: Prog -> Doc AnsiStyle
prettyProg prog =
  vsep [annotate (italicized <> colorDull White) "// IR1", vsep (fmap prettyFunc prog)]

prettyFunc :: Func -> Doc AnsiStyle
prettyFunc (FuncF name params ret body) = do
  let sig = keyword "fn" <+> prettyFuncIdent name <> parens (commas (map (\(v, a) -> prettyVarIdent v <> ":" <+> prettyType a) params)) <+> "->" <+> prettyType ret
  let body' = "{\n" <> indent 4 (prettyExpr body) <> "\n}"
  sig <+> body' <> "\n"

prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr = foldFix $ \case
  L1 x -> prettyCallF x
  R1 x -> prettyCoreF x