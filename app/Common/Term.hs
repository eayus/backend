module Common.Term where

import Data.Hashable
import Data.String
import Text.Show.Deriving

newtype VarIdent = VarIdent String
  deriving (Eq, Show, IsString, Hashable)

newtype FuncIdent = FuncIdent String
  deriving (Eq, Show, IsString, Hashable)

data PrimF a
  = Int Integer
  | Add a a
  | Sub a a
  | GreaterThan a a
  deriving (Foldable, Functor, Show, Traversable)

data ExprF a
  = Var VarIdent
  | Let VarIdent a a
  | Call FuncIdent [a]
  | Prim (PrimF a)
  | Match a [(VarIdent, a)]
  deriving (Foldable, Functor, Show, Traversable)

$(deriveShow1 ''PrimF)
$(deriveShow1 ''ExprF)