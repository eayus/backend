module Main where

import Common.Term
import IR1.Lower qualified as IR1
import IR0.Lower qualified as IR0
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
func = Func "main" ["x", "y"] body
  where
    body =
      ELetRec (Func "add_to_x" ["q"] (Expr $ Prim $ Add (Expr $ Var "q") (Expr $ Var "x"))) $
        ELetRec (Func "add" ["p"] (Expr $ Call "add_to_x" [Expr $ Var "p"])) $
          Expr $
            Call "add" [Expr $ Var "y"]