module Examples(
  fac
)
where

import Core
import Prim
import Type
import Utils

--
-- fac :: Int -> Int
-- fac 0 = 1
-- fac n = n * fac (n-1)
--
-- fac :: Int -> Int
-- fac = \n ->
--       case n of
--         0 -> 1
--         n -> n * fac (n-1)
--
--
-- fac :: Int -> Int
-- fac = \n -> if n == 0 then 1 else (n * fac (n-1))
--
-- fac :: Int -> Int
-- fac = \n -> case n == 0 of
--               True  -> 1
--               False -> n * fac (n-1)
--

fac :: Expr
fac = Fix $ Abs varn cs
    where
      varn    = V 0 TInt
      n       = Fix $ Var varn
      eqi x y = Fix $ Prim2 PrimEqInt x y
      cs      = Fix $ Case (eqi n (LInt 0)) [(AltCon TrueCon,[],LInt 1), (AltCon FalseCon,[],binFun PrimMulInt n rece)]
      dec     = binFun PrimSubInt n (LInt 1)
      rece    = Fix $ App (Fix $ Var $ V 1 tfac) dec -- assumes v1 is (the recursively bound) 'fac'
      tfac    = Fix $ TFunTy TInt TInt

