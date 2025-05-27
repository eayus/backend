module Common.Pretty where

import Common.Term
import Prettyprinter
import Prettyprinter.Render.Terminal

prettyProgF :: ProgF (Doc AnsiStyle) -> Doc AnsiStyle
prettyProgF (ProgF types funcs) = vsep $ map prettyTypeDef types ++ funcs

prettyTypeDef :: TypeDef -> Doc AnsiStyle
prettyTypeDef (TypeDef name cons) = keyword "enum" <+> prettyTypeIdent name <+> braceBlock (vsep $ map prettyConstructorDef cons)

prettyConstructorDef :: ConstructorDef -> Doc AnsiStyle
prettyConstructorDef (ConstructorDef name params) = prettyConstructorIdent name <> commas (map prettyType params) <> ","

prettyType :: Type -> Doc AnsiStyle
prettyType = \case
  TInt -> constant "isize"
  TNamed s -> prettyTypeIdent s

prettyExprF :: ExprF (Doc AnsiStyle) -> Doc AnsiStyle
prettyExprF = \case
  Var v _ -> prettyVarIdent v
  Prim (Lit l) -> prettyLit l
  Prim (Add x y) -> parens $ x <+> "+" <+> y
  Prim (Sub x y) -> parens $ x <+> "-" <+> y
  Prim (GreaterThan x y) -> parens $ x <+> ">" <+> y
  Let v x y -> "{ let " <+> prettyVarIdent v <+> "=" <+> x <+> "; " <+> y <+> "}"
  Call f xs -> prettyFuncIdent f <> "(" <> commas xs <> ")"
  Match x cs -> parens $ keyword "match" <+> x <+> braceBlock (prettyClauses cs)

prettyClauses :: [ClauseF (Doc AnsiStyle)] -> Doc AnsiStyle
prettyClauses = vsep . map prettyClause

prettyClause :: ClauseF (Doc AnsiStyle) -> Doc AnsiStyle
prettyClause (ClauseF pat x) = prettyPattern pat <+> "=>" <+> x <> ","

prettyPattern :: Pattern -> Doc AnsiStyle
prettyPattern = \case
  PLit l -> prettyLit l
  PCon con params -> prettyConstructorIdent con <> parens (commas $ map (prettyVarIdent . fst) params)

prettyConstructorIdent :: Ident IConstructor -> Doc AnsiStyle
prettyConstructorIdent (Ident s) = annotate (color Yellow) $ pretty s

prettyTypeIdent :: Ident IType -> Doc AnsiStyle
prettyTypeIdent (Ident s) = pretty s

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