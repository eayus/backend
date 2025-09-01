module Common.Term where

import Data.Fix
import Data.Hashable
import Data.String
import GHC.Generics
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
  = Int Int
  | Bool Bool
  deriving (Show)

data PatF r
  = PLit Lit
  | PVar (Ident IVar) Type
  | PPair r r
  | PInj Int r
  deriving (Foldable, Functor, Show)

type Pat = Fix PatF

$(deriveShow1 ''PatF)

data ClauseF a = ClauseF
  { pattern :: Pat,
    body :: a
  }
  deriving (Foldable, Functor, Show, Traversable)

$(deriveShow1 ''ClauseF)

-- Expressions which contain binders
data BindF a
  = Let (Ident IVar) a a
  | Match a [ClauseF a]
  deriving (Foldable, Functor, Show, Traversable)

$(deriveShow1 ''BindF)

-- Expressions which don't contain binders
data FlatF a
  = Var (Ident IVar) Type
  | Lit Lit
  | Inj Int a
  | Pair a a
  | Add a a
  | Sub a a
  | IGT a a
  deriving (Foldable, Functor, Show, Traversable)

$(deriveShow1 ''FlatF)

type CoreF = FlatF :+: BindF

data CallF a
  = CallF (Ident IFunc) [a]
  deriving (Foldable, Functor, Show, Traversable)

$(deriveShow1 ''CallF)

data FuncF a = FuncF
  { name :: Ident IFunc,
    params :: [(Ident IVar, Type)],
    returnType :: Type,
    body :: a
  }
  deriving (Foldable, Functor, Show, Traversable)

data LetFuncF a = LetFuncF (FuncF a) a
  deriving (Foldable, Functor, Show, Traversable)
