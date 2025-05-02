module Common.Term where

type Ident = String

data Prim a
  = Int Integer
  | Add a a
  | Sub a a
  | GreaterThan a a
  deriving (Foldable, Functor, Show)