module Main where

import Common.Term
import Data.Fix
import IR0.Lower qualified as IR0
import IR1.Lower qualified as IR1
import IR1.Pretty qualified as IR1
import IR1.Term
import Prettyprinter.Render.Terminal

main :: IO ()
main = do
  putDoc $ IR1.prettyProg [func]
  putDoc $ IR0.lower $ IR1.lower [func]

func :: Func
func = Func "add" ["x", "y"] body
  where
    body =
      Fix $
        If
          (Fix $ Prim $ GreaterThan (Fix $ Var "x") (Fix $ Prim $ Int 0))
          (Fix $ Call "add" [Fix $ Prim $ Sub (Fix $ Var "x") (Fix $ Prim $ Int 1), Fix $ Prim $ Add (Fix $ Var "y") (Fix $ Prim $ Int 1)])
          (Fix $ Var "y")