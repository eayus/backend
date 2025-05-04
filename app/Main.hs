module Main where

import Common.Term
import IR0.Lower qualified as IR0
import IR1.Lower qualified as IR1
import IR1.Pretty qualified as IR1
import IR2.Lower qualified as IR2
import IR2.Pretty qualified as IR2
import IR2.Term
import Prettyprinter
import Prettyprinter.Render.Terminal

main :: IO ()
main = do
  let ir2 = [func]
  let ir1 = IR2.lower ir2
  let ir0 = IR1.lower ir1
  putDoc $ vsep [IR2.prettyProg [func], IR1.prettyProg ir1, IR0.lower ir0]

func :: Func
func =
  Func "main" ["x", "y"] $
    Expr $
      Match
        (Expr $ Prim $ GreaterThan (Expr $ Var "x") (Expr $ Prim $ Int 0))
        [("_", Expr $ Call "main" [Expr $ Prim $ Sub (Expr $ Var "x") (Expr $ Prim $ Int 1), Expr $ Prim $ Add (Expr $ Var "y") (Expr $ Prim $ Int 1)]), ("_", Expr $ Var "y")]