module Common.Pretty where

import Common.Term
import Data.Fix
import GHC.Generics
import Prettyprinter
import Prettyprinter.Render.Terminal

-- Make pretty typeclass and use instances?

prettyType :: Type -> Doc AnsiStyle
prettyType = \case
  TInt -> constant "isize"
  TBool -> constant "bool"
  TProd a b -> parens $ prettyType a <> "," <+> prettyType b
  TSum as -> parens $ concatWith (\l r -> l <+> "|" <+> r) $ map prettyType as

prettyCallF :: CallF (Doc AnsiStyle) -> Doc AnsiStyle
prettyCallF (CallF f xs) = prettyFuncIdent f <> "(" <> commas xs <> ")"

prettyCoreF :: CoreF (Doc AnsiStyle) -> Doc AnsiStyle
prettyCoreF = \case
  L1 x -> prettyFlatF x
  R1 x -> prettyBindF x

prettyBindF :: BindF (Doc AnsiStyle) -> Doc AnsiStyle
prettyBindF = \case
  Let v x y -> "{ let " <+> prettyVarIdent v <+> "=" <+> x <+> "; " <+> y <+> "}"
  Match x cs -> parens $ keyword "match" <+> x <+> braceBlock (prettyClauses cs)

prettyFlatF :: FlatF (Doc AnsiStyle) -> Doc AnsiStyle
prettyFlatF = \case
  Var v _ -> prettyVarIdent v
  Lit l -> prettyLit l
  Pair x y -> parens $ x <> "," <+> y
  Inj n x -> parens $ constant ("inj" ++ show n) <+> x
  Add x y -> parens $ x <+> "+" <+> y
  Sub x y -> parens $ x <+> "-" <+> y
  IGT x y -> parens $ x <+> ">" <+> y

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