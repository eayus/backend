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
  If x y z -> parens $ keyword "if" <+> x <+> "{" <+> y <+> "}" <+> keyword "else" <+> "{" <+> z <+> "}"
  Let v x y -> "{ let " <+> prettyVarIdent v <+> "=" <+> x <+> "; " <+> y <+> "}"
  Call f xs -> prettyFuncIdent f <> "(" <> commas xs <> ")"

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