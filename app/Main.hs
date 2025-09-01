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
  let ir2 = Fix $ L1 $ LetFuncF func (flat $ Lit $ Int 3)
  let ir1 = IR2.lower ir2
  let ir0 = IR1.lower ir1
  putDoc $ vsep [IR2.prettyProg ir2, IR1.prettyProg ir1, IR0.prettyProg ir0]

wrap :: CoreF Expr -> Expr
wrap = Fix . R1 . R1

flat :: FlatF Expr -> Expr
flat = wrap . L1

bind :: BindF Expr -> Expr
bind = wrap . R1

func :: Func
func =
  FuncF "adder" [("x", TInt), ("y", TInt)] TInt $
    bind $
      Match
        (flat $ IGT (flat $ Var "x" TInt) (flat $ Lit $ Int 0))
        [ ClauseF (Fix $ PLit $ Bool True) $ Fix $ R1 $ L1 $ CallF "adder" [flat $ Sub (flat $ Var "x" TInt) (flat $ Lit $ Int 1), flat $ Add (flat $ Var "y" TInt) (flat $ Lit $ Int 1)],
          ClauseF (Fix $ PLit $ Bool False) $ flat $ Var "y" TInt
        ]