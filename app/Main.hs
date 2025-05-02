module Main where

import Common.Term
import IR0.Lower qualified as IR0
import IR1.Lower qualified as IR1
import IR1.Term

main :: IO ()
main = putStrLn $ IR0.lower $ IR1.lower [func]

func :: Func
func = Func "add" ["x", "y"] body
  where
    body =
      If
        (Prim $ GreaterThan (Var "x") (Prim $ Int 0))
        (Call "add" [Prim $ Sub (Var "x") (Prim $ Int 1), Prim $ Add (Var "y") (Prim $ Int 1)])
        (Var "y")