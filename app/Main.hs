module Main where

import Common.Term
import Data.Fix
import GHC.Generics
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

wrap :: CoreF Expr -> Expr
wrap = Fix . R1 . R1

func :: Func
func =
  FuncF "adder" [("x", TInt), ("y", TInt)] TInt $
    wrap $
      Match
        (wrap $ IGT (wrap $ Var "x" TInt) (wrap $ Lit $ Int 0))
        [ ClauseF (Fix $ PLit $ Bool True) $ Fix $ R1 $ L1 $ CallF "adder" [wrap $ Sub (wrap $ Var "x" TInt) (wrap $ Lit $ Int 1), wrap $ Add (wrap $ Var "y" TInt) (wrap $ Lit $ Int 1)],
          ClauseF (Fix $ PLit $ Bool False) $ wrap $ Var "y" TInt
        ]