module IR1.Lower (lower) where

import Common.Term
import IR0.Term qualified as I
import IR1.Term qualified as F

lower :: F.Prog -> I.Prog
lower = map tryTCO

-- Attempt to tail call optimise a function, else naively convert.
tryTCO :: F.Func -> I.Func
tryTCO func
  | eligble func = tco func
  | otherwise = convert func

-- Apply TCO if we know it is eligible.
tco :: F.Func -> I.Func
tco (F.Func name params body) = I.Func name params stmts
  where
    stmts :: [I.Stmt]
    stmts = [I.Loop $ removeTailCalls body]

    tmpVar :: Ident -> Ident
    tmpVar = ("_tmp_" ++)

    removeTailCalls :: F.Expr -> [I.Stmt]
    removeTailCalls = \case
      e@(F.Var {}) -> [I.Ret $ convExpr e]
      e@(F.Prim {}) -> [I.Ret $ convExpr e]
      F.If x y z -> [I.SIf (convExpr x) (removeTailCalls y) (removeTailCalls z)]
      F.Let v x y -> I.SLet v (convExpr x) : removeTailCalls y
      e@(F.Call f xs)
        | f == name -> do
            -- To ensure updating the mutable variables does not change their evaluation,
            -- we first compute values for all the params and give them temporary bindings.
            let setTmps = zipWith (\v x -> I.SLet (tmpVar v) (convExpr x)) params xs
            let updMuts = map (\v -> I.Set v (I.Var $ tmpVar v)) params
            setTmps ++ updMuts
        | otherwise -> [I.Ret $ convExpr e]

-- Convert a functional func into an imperative one without TCO.
convert :: F.Func -> I.Func
convert (F.Func name params body) = I.Func name params [I.Ret (convExpr body)]

-- Convert a functional expr into an imperative one without TCO.
convExpr :: F.Expr -> I.Expr
convExpr = \case
  F.Var v -> I.Var v
  F.Prim p -> I.Prim (fmap convExpr p)
  F.If x y z -> I.If (convExpr x) (convExpr y) (convExpr z)
  F.Let v x y -> I.Let v (convExpr x) (convExpr y)
  F.Call f xs -> I.Call f (map convExpr xs)

-- Check whether every recursive call is in tail position.
eligble :: F.Func -> Bool
eligble (F.Func name _ body) = onlyTailCalls body
  where
    onlyTailCalls :: F.Expr -> Bool
    onlyTailCalls = \case
      F.Var _ -> True
      F.Prim p -> all noRecursion p
      F.If x y z -> noRecursion x && onlyTailCalls y && onlyTailCalls z
      F.Let _ x y -> noRecursion x && onlyTailCalls y
      F.Call _ xs -> all noRecursion xs

    noRecursion :: F.Expr -> Bool
    noRecursion = \case
      F.Var _ -> True
      F.Prim p -> all noRecursion p
      F.If x y z -> noRecursion x && noRecursion y && noRecursion z
      F.Let _ x y -> noRecursion x && noRecursion y
      F.Call f xs -> f /= name && all noRecursion xs