module Common.Pretty where

import Common.Term
import Prettyprinter
import Prettyprinter.Render.Terminal

prettyExprF :: ExprF (Doc AnsiStyle) -> Doc AnsiStyle
prettyExprF = \case
  Var v -> prettyVarIdent v
  Prim (Int n) -> literal n
  Prim (Add x y) -> parens $ x <+> "+" <+> y
  Prim (Sub x y) -> parens $ x <+> "-" <+> y
  Prim (GreaterThan x y) -> parens $ x <+> ">" <+> y
  Let v x y -> "{ let " <+> prettyVarIdent v <+> "=" <+> x <+> "; " <+> y <+> "}"
  Call f xs -> prettyFuncIdent f <> "(" <> commas xs <> ")"
  Match x cs -> parens $ keyword "match" <+> x <+> "{\n" <> indent 4 (prettyClauses cs) <> "\n}"

prettyClauses :: [(VarIdent, Doc AnsiStyle)] -> Doc AnsiStyle
prettyClauses cs = vsep $ zipWith (curry (\((v, x), i) -> prettyClause v x i)) cs [0 ..]

prettyClause :: VarIdent -> Doc AnsiStyle -> Int -> Doc AnsiStyle
prettyClause v x i = pretty ("C" ++ show i) <+> prettyVarIdent v <+> "=>" <+> x <> ","

prettyVarIdent :: VarIdent -> Doc AnsiStyle
prettyVarIdent (VarIdent s) = pretty s

prettyFuncIdent :: FuncIdent -> Doc AnsiStyle
prettyFuncIdent (FuncIdent s) = annotate (color Green) $ pretty s

commas :: [Doc AnsiStyle] -> Doc AnsiStyle
commas = concatWith (\l r -> l <> "," <+> r)

keyword :: String -> Doc AnsiStyle
keyword = annotate (color Red) . pretty

constant :: String -> Doc AnsiStyle
constant = annotate (color Magenta) . pretty

literal :: (Pretty a) => a -> Doc AnsiStyle
literal = annotate (color Blue) . pretty

braceBlock :: Doc AnsiStyle -> Doc AnsiStyle
braceBlock x = "{\n" <> indent 4 x <> "\n}"