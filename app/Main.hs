module Main where

import Common.Term
import Data.Fix
import IR0.Pretty qualified as IR0
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
  putDoc $ vsep [IR2.prettyProg ir2, IR1.prettyProg ir1, IR0.prettyProg ir0]

func :: Func
func =
  Func "adder" [("x", TInt), ("y", TInt)] TInt $
    Expr $
      Match
        (Expr $ Prim $ IGT (Expr $ Var "x" TInt) (Expr $ Prim $ Lit $ Int 0))
        [ ClauseF (Fix $ PLit $ Bool True) $ Expr $ Call "adder" [Expr $ Prim $ Sub (Expr $ Var "x" TInt) (Expr $ Prim $ Lit $ Int 1), Expr $ Prim $ Add (Expr $ Var "y" TInt) (Expr $ Prim $ Lit $ Int 1)],
          ClauseF (Fix $ PLit $ Bool False) $ Expr $ Var "y" TInt
        ]