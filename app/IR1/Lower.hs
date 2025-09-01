-- Perform tail call optimisation while converting to an imperative
-- representation.
module IR1.Lower (lower) where

import Common.Term
import Data.Fix
import GHC.Generics
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
tco (FuncF name params ret body) = I.Func name params ret stmts
  where
    stmts :: [I.Stmt]
    stmts = [I.Loop $ removeTailCalls body]

    tmpVar :: Ident IVar -> Ident IVar
    tmpVar (Ident s) = Ident ("_tmp_" ++ s)

    removeTailCalls :: F.Expr -> [I.Stmt]
    removeTailCalls e = case unFix e of
      (R1 c) -> case c of
        R1 (Match x cs) -> [I.SMatch x $ map (fmap removeTailCalls) cs]
        R1 (Let v x y) -> I.SLet v x : removeTailCalls y
        _ -> [I.Ret e]
      L1 (CallF f xs)
        | f == name -> do
            -- To ensure updating the mutable variables does not change their evaluation,
            -- we first compute values for all the params and give them temporary bindings.
            let setTmps = zipWith (I.SLet . tmpVar) (map fst params) xs
            let updMuts = map (\(v, a) -> I.Set v (Fix $ R1 $ L1 $ Var (tmpVar v) a)) params
            setTmps ++ updMuts
        | otherwise -> [I.Ret e]

-- Convert a functional func into an imperative one without TCO.
convert :: F.Func -> I.Func
convert (FuncF name params ret body) = I.Func name params ret [I.Ret body]

-- Check whether every recursive call is in tail position.
eligble :: F.Func -> Bool
eligble (FuncF name _ _ body) = onlyTailCalls body
  where
    onlyTailCalls :: F.Expr -> Bool
    onlyTailCalls (Fix e) = case e of
      R1 (R1 (Match x cs)) -> noRecursion x && all (onlyTailCalls . (.body)) cs
      R1 (R1 (Let _ x y)) -> noRecursion x && onlyTailCalls y
      _ -> all noRecursion e

    noRecursion :: F.Expr -> Bool
    noRecursion (Fix e) = case e of
      L1 (CallF f _) | f == name -> False
      x -> all noRecursion x