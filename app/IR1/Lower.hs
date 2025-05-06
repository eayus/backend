-- Perform tail call optimisation while converting to an imperative
-- representation.
module IR1.Lower (lower) where

import Common.Term
import Data.Fix
import IR0.Term qualified as I
import IR1.Term qualified as F

lower :: F.Prog -> I.Prog
lower = fmap tryTCO

-- Attempt to tail call optimise a function, else naively convert.
tryTCO :: F.Func -> I.Func
tryTCO func
  | eligble func = tco func
  | otherwise = convert func

-- Apply TCO if we know it is eligible.
tco :: F.Func -> I.Func
tco (F.Func name params ret body) = I.Func name params ret stmts
  where
    stmts :: [I.Stmt]
    stmts = [I.Loop $ removeTailCalls body]

    tmpVar :: Ident IVar -> Ident IVar
    tmpVar (Ident s) = Ident ("_tmp_" ++ s)

    removeTailCalls :: F.Expr -> [I.Stmt]
    removeTailCalls e = case unFix e of
      Var {} -> [I.Ret e]
      Prim {} -> [I.Ret e]
      Match x cs -> [I.SMatch x $ map (fmap removeTailCalls) cs]
      Let v x y -> I.SLet v x : removeTailCalls y
      Call f xs
        | f == name -> do
            -- To ensure updating the mutable variables does not change their evaluation,
            -- we first compute values for all the params and give them temporary bindings.
            let pnames = map fst params
            let setTmps = zipWith (I.SLet . tmpVar) pnames xs
            let updMuts = map (\v -> I.Set v (Fix $ Var $ tmpVar v)) pnames
            setTmps ++ updMuts
        | otherwise -> [I.Ret e]

-- Convert a functional func into an imperative one without TCO.
convert :: F.Func -> I.Func
convert (F.Func name params ret body) = I.Func name params ret [I.Ret body]

-- Check whether every recursive call is in tail position.
eligble :: F.Func -> Bool
eligble (F.Func name _ _ body) = onlyTailCalls body
  where
    onlyTailCalls :: F.Expr -> Bool
    onlyTailCalls e = case unFix e of
      Var _ -> True
      Prim p -> all noRecursion p
      Match x cs -> noRecursion x && all (onlyTailCalls . (.body)) cs
      Let _ x y -> noRecursion x && onlyTailCalls y
      Call _ xs -> all noRecursion xs

    noRecursion :: F.Expr -> Bool
    noRecursion e = case unFix e of
      Call f _ | f == name -> False
      x -> all noRecursion x