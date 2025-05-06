module IR2.Lower (lower) where

import Common.Term
import Control.Monad.State
import Control.Monad.Writer
import Data.Fix
import Data.Foldable
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.Maybe
import IR1.Term qualified as D
import IR2.Term qualified as S

-- Extra parameters are PREPENDED to the existing param list.
data LocalFuncInfo = LocalFuncInfo
  { newName :: Ident IFunc,
    extraParams :: [Ident IVar]
  }

type LocalFuncs = M.HashMap (Ident IFunc) LocalFuncInfo

type Lifter = StateT (S.HashSet (Ident IFunc)) (Writer [D.Func])

lower :: S.Prog -> D.Prog
lower (ProgF types funcs) = do
  let topNames = map (.name) funcs
  let funcs' = execWriter $ evalStateT (mapM llTop funcs) (S.fromList topNames)
  ProgF types funcs'

llTop :: S.Func -> Lifter ()
llTop (S.Func name params ret body) = do
  body' <- llExpr name M.empty body
  tell [D.Func name params ret body']

llExpr :: Ident IFunc -> LocalFuncs -> S.Expr -> Lifter D.Expr
llExpr currentFunc localFuncs = \case
  -- If 'f' is local, then we need to pass the extra parameters added by lambda lifting.
  -- If 'f' is global, then we can leave it unchanged as lambda lifting does not
  -- affect global functions.
  S.Expr (Call f xs) | Just info <- M.lookup f localFuncs -> do
    -- 'f' is local
    xs' <- mapM (llExpr currentFunc localFuncs) xs
    pure $ Fix $ Call info.newName (map (Fix . Var) info.extraParams ++ xs')
  S.Expr x -> Fix <$> mapM (llExpr currentFunc localFuncs) x
  S.ELetRec func@(S.Func name params ret body) cont -> do
    newName <- freshFuncIdent currentFunc name
    let captured = S.toList $ funcCaptures localFuncs func
    let localFuncs' = M.insert name (LocalFuncInfo newName captured) localFuncs
    body' <- llExpr currentFunc localFuncs' body
    tell [D.Func newName (captured ++ params) ret body']
    llExpr currentFunc localFuncs' cont

-- We must generate fresh names for newly lifted functions.
-- We base the name on the original name
freshFuncIdent :: Ident IFunc -> Ident IFunc -> Lifter (Ident IFunc)
freshFuncIdent (Ident outer) (Ident oldName) = do
  taken <- get
  let candidates = map (Ident . ((outer ++ "_" ++ oldName) ++)) ("" : map show [0 :: Integer ..])
  let newName = fromJust $ find (not . flip S.member taken) candidates
  put (S.insert newName taken)
  pure newName

-- Calculcate the variables a function captures. If this function calls other functions, and they require extra parameters,
-- this function must also capture those too!
funcCaptures :: LocalFuncs -> S.Func -> S.HashSet (Ident IVar)
funcCaptures locals (S.Func _ params _ body) = exprCaptures locals body `S.difference` S.fromList params

exprCaptures :: LocalFuncs -> S.Expr -> S.HashSet (Ident IVar)
exprCaptures locals = \case
  S.Expr (Var v) -> S.singleton v
  S.Expr (Let v x y) -> exprCaptures locals x `S.union` S.delete v (exprCaptures locals y)
  S.Expr (Match x cs) -> exprCaptures locals x `S.union` S.unions (map (clauseCaptures locals) cs)
  S.Expr (Call f xs) | Just info <- M.lookup f locals -> S.unions (fmap (exprCaptures locals) xs) `S.union` S.fromList info.extraParams -- The interesting case!
  S.Expr e -> S.unions $ toList $ fmap (exprCaptures locals) e
  S.ELetRec func cont -> funcCaptures locals func `S.union` exprCaptures locals cont

clauseCaptures :: LocalFuncs -> ClauseF S.Expr -> S.HashSet (Ident IVar)
clauseCaptures locals (ClauseF pat x) = exprCaptures locals x `S.difference` patBinds pat

patBinds :: Pattern -> S.HashSet (Ident IVar)
patBinds pat = S.fromList pat.args