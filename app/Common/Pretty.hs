module Common.Pretty where

import Common.Term
import Data.Fix
import Prettyprinter
import Prettyprinter.Render.Terminal

prettyType :: Type -> Doc AnsiStyle
prettyType = \case
  TInt -> constant "isize"
  TBool -> constant "bool"
  TProd a b -> parens $ prettyType a <> "," <+> prettyType b
  TSum as -> parens $ concatWith (\l r -> l <+> "|" <+> r) $ map prettyType as

prettyExprF :: ExprF (Doc AnsiStyle) -> Doc AnsiStyle
prettyExprF = \case
  Var v _ -> prettyVarIdent v
  Prim (Lit l) -> prettyLit l
  Prim (Pair x y) -> parens $ x <> "," <+> y
  Prim (Inj n x) -> parens $ constant ("inj" ++ show n) <+> x
  Prim (Add x y) -> parens $ x <+> "+" <+> y
  Prim (Sub x y) -> parens $ x <+> "-" <+> y
  Prim (IGT x y) -> parens $ x <+> ">" <+> y
  Let v x y -> "{ let " <+> prettyVarIdent v <+> "=" <+> x <+> "; " <+> y <+> "}"
  Call f xs -> prettyFuncIdent f <> "(" <> commas xs <> ")"
  Match x cs -> parens $ keyword "match" <+> x <+> braceBlock (prettyClauses cs)

prettyClauses :: [ClauseF (Doc AnsiStyle)] -> Doc AnsiStyle
prettyClauses = vsep . map prettyClause

prettyClause :: ClauseF (Doc AnsiStyle) -> Doc AnsiStyle
prettyClause (ClauseF pat x) = prettyPattern pat <+> "=>" <+> x <> ","

prettyPattern :: Pat -> Doc AnsiStyle
prettyPattern = foldFix $ \case
  PLit l -> prettyLit l
  PVar v _ -> prettyVarIdent v
  PPair x y -> parens $ x <> "," <+> y
  PInj n x -> constant ("inj" ++ show n) <+> x

prettyVarIdent :: Ident IVar -> Doc AnsiStyle
prettyVarIdent (Ident s) = pretty s

prettyFuncIdent :: Ident IFunc -> Doc AnsiStyle
prettyFuncIdent (Ident s) = annotate (color Green) $ pretty s

prettyLit :: Lit -> Doc AnsiStyle
prettyLit = \case
  Int n -> literal n
  Bool b -> literal $ if b then "true" else "false" :: String

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