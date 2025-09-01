module Common.Term where

import Data.Fix
import Data.Hashable
import Data.String
import Text.Show.Deriving

data IdentKind
  = IVar
  | IFunc

newtype Ident (k :: IdentKind) = Ident String
  deriving (Eq, Show, IsString, Hashable)

data Type
  = TInt
  | TBool
  | TProd Type Type
  | TSum [Type]
  deriving (Show)

data Lit
  = Int Integer
  | Bool Bool
  deriving (Show)

data PrimF a
  = Lit Lit
  | Pair a a
  | Inj Integer a
  | Add a a
  | Sub a a
  | IGT a a -- Integer greater than (>)
  deriving (Foldable, Functor, Show, Traversable)

data PatF r
  = PLit Lit
  | PVar (Ident IVar) Type
  | PPair r r
  | PInj Int r
  deriving (Foldable, Functor, Show)

$(deriveShow1 ''PatF)

type Pat = Fix PatF

data ClauseF a = ClauseF
  { pattern :: Pat,
    body :: a
  }
  deriving (Foldable, Functor, Show, Traversable)

data ExprF a
  = Var (Ident IVar) Type
  | Let (Ident IVar) a a
  | Call (Ident IFunc) [a]
  | Prim (PrimF a)
  | Match a [ClauseF a]
  deriving (Foldable, Functor, Show, Traversable)

$(deriveShow1 ''PrimF)
$(deriveShow1 ''ClauseF)
$(deriveShow1 ''ExprF)