module IR3.Lower (lower) where

import Common.Term
import Data.Fix
import GHC.Generics
import IR2.Term qualified as B
import IR3.Term qualified as A

-- Uncurrying

lower :: A.Expr -> B.Expr
lower (Fix e) = case e of
  L1 (A.AppF x y) -> case lower x of
    Fix (R1 (R1 (L1 (Var (Ident v) _)))) -> Fix $ R1 $ L1 $ CallF (Ident v) [lower y]
    _ -> undefined -- All functions applications should begin with a variable
  L1 (A.LamF {}) -> undefined -- Lambdas can only appear as argument to a let-binding.
  R1 (R1 (Let (Ident v) x y)) | Just (params, ret, body) <- collectLams x -> Fix $ L1 (LetFuncF (FuncF (Ident v) params ret (lower body)) (lower y))
  R1 x -> Fix $ R1 $ R1 $ fmap lower x

collectLams :: A.Expr -> Maybe ([(Ident IVar, Type)], Type, A.Expr)
collectLams = \case
  Fix (L1 (A.LamF v a b x)) -> case collectLams x of
    Nothing -> pure ([(v, a)], b, x)
    Just (ps, ret, y) -> pure ((v, a) : ps, ret, y)
  _ -> Nothing