------------------------------------------------------------
--
-- A call-by-value partial evaluator for Core
--
module CoreEval where

import Core
import Prim
import Utils

import Control.Monad.Except

import qualified Data.IntMap as IM

-- call-by-value environment
type Env = IM.IntMap Value

-- we need a value type to keep compositionality (no deep patterns)
-- and to support failure.
data Value
  = VAbs (Value -> Value)
  | VVal Expr
  | VErr String

instance Show Value where
  show (VAbs _) = "<Abs>"
  show (VVal e) = show e
  show (VErr s) = "Error: " ++ s

eval :: Expr -> Value
eval e = (cata alg e) IM.empty
  where
    alg :: ExprF (Env -> Value) -> Env -> Value
    alg (Var v)        env = lookupValue env v
    alg (App f x)      env = evalApp (f env) (x env)
    alg (TyApp f x)    env = undefined -- evalApp (f env) (x env)
    alg (Abs _ b e1)   env = mkAbs $ \x -> e1 $ bind env b x
    alg (TyAbs _ b e1) env = undefined -- mkAbs $ \t -> e1 $ bind env b t
    alg (Let b e1 e2)  env = e2 $ bind env b (e1 env)
    alg (LetRec bs e2) env =
      e2 $ fix $ \env' ->
        let f (b, e1) env'' = bind env'' b (e1 env')
        in foldr f env bs
    alg (Case s alts)  env = evalCase (s env) alts env
    alg (Lit l)        _   = mkVal $ Fix $ Lit l
    alg (Prim1 p x)    env = lift1 (evalPrim1 p) (x env)
    alg (Prim2 p x y)  env = lift2 (evalPrim2 p) (x env) (y env)
    alg (Inject dc)    _   = mkVal $ Fix $ Inject dc
    alg (Tuple _)      _   = mkErr "Tuple unsupported"

mkVal :: Expr -> Value
mkVal = VVal

mkAbs :: (Value -> Value) -> Value
mkAbs = VAbs

mkErr :: String -> Value
mkErr = VErr

evalApp :: Value -> Value -> Value
evalApp (VAbs f)   val = f val
evalApp (VVal val) _   = mkErr $ "evalApp: type mismatch: " ++ show val
evalApp err@VErr{} _   = err

evalPrim1 :: Prim1 -> Expr -> Expr
evalPrim1 p _x = error $ "TODO: prim not supported: " ++ show p

lift1 :: (Expr -> Expr) -> Value -> Value
lift1 f (VVal x)   = mkVal $ f x
lift1 _ err@VErr{} = err
lift1 _ val        = mkErr $ "lift1: type mismatch: " ++ show val

lift2 :: (Expr -> Expr -> Expr) -> Value -> Value -> Value
lift2 f (VVal x)   (VVal y)   = mkVal $ f x y
lift2 _ err@VErr{} _          = err
lift2 _ _          err@VErr{} = err
lift2 _ val1       val2       =
  mkErr $ "lift1: type mismatch: " ++ show (val1, val2)

evalPrim2 :: Prim2 -> Expr -> Expr -> Expr
evalPrim2 PrimEqInt x y | x==y = Fix (Inject TrueCon)
                        | x/=y = Fix (Inject FalseCon)
evalPrim2 PrimMulInt (LInt x) (LInt y) = Fix $ Lit (LitInt $ x * y)
evalPrim2 PrimSubInt (LInt x) (LInt y) = Fix $ Lit (LitInt $ x - y)
evalPrim2 p _ _ = error $ "TODO: prim not supported: " ++ show p

evalCase :: Value -> [(AltCon, [bind], Env -> Value)] -> Env -> Value
evalCase (VVal (Fix (Inject dcon))) alts = foldr f err alts
  where
    f (AltCon dcon', _, e') _ | dcon == dcon' = e' -- TODO how to handle binds?
    f _                     e                 = e
    err = const $ mkErr "Empty alternative list"
evalCase e _ = const $ mkErr $ "Not a data constructor: " ++ show e

-- call-by-value bind
bind :: Env -> Var -> Value -> Env
bind env v x = IM.insert (varId v) x env

lookupValue :: Env -> Var -> Value
lookupValue env v = IM.findWithDefault err (varId v) env
  where
    err = mkErr $ "Not found: " ++ show v

extract :: Value -> Either String Expr
extract (VVal e) = return e
extract (VAbs _) = throwError "value is an abstraction"
extract (VErr s) = throwError s
