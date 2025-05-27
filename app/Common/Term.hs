module Common.Term where

import Data.Hashable
import Data.String
import Text.Show.Deriving

data IdentKind
  = IVar
  | IFunc
  | IType
  | IConstructor

newtype Ident (k :: IdentKind) = Ident String
  deriving (Eq, Show, IsString, Hashable)

data Lit
  = Int Integer
  | Bool Bool
  deriving (Show)

data PrimF a
  = Lit Lit
  | Add a a
  | Sub a a
  | GreaterThan a a
  deriving (Foldable, Functor, Show, Traversable)

data ClauseF a = ClauseF
  { pattern :: Pattern,
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

data Type
  = TNamed (Ident IType)
  | TInt
  deriving (Show)

data Pattern
  = PLit Lit
  | PCon {constructor :: Ident IConstructor, params :: [(Ident IVar, Type)]}
  deriving (Show)

data TypeDef = TypeDef
  { name :: Ident IType,
    constructors :: [ConstructorDef]
  }
  deriving (Show)

data ConstructorDef = ConstructorDef
  { name :: Ident IConstructor,
    params :: [Type]
  }
  deriving (Show)

data ProgF f = ProgF
  { types :: [TypeDef],
    funcs :: [f]
  }
  deriving (Foldable, Functor, Traversable)

$(deriveShow1 ''PrimF)
$(deriveShow1 ''ClauseF)
$(deriveShow1 ''ExprF)
$(deriveShow1 ''ProgF)