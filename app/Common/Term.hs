module Common.Term where

import Text.Show.Deriving

type Ident = String

data PrimF a
  = Int Integer
  | Add a a
  | Sub a a
  | GreaterThan a a
  deriving (Foldable, Functor, Show)

data ExprF a
  = Var Ident
  | If a a a
  | Let Ident a a
  | Call Ident [a]
  | Prim (PrimF a)
  deriving (Foldable, Functor, Show)

$(deriveShow1 ''PrimF)
$(deriveShow1 ''ExprF)