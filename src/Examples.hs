module Examples(
  fac
)
where

import Core

--
-- fac :: Int -> Int
-- fac 0 = 1
-- fac n = n * fac (n-1)
--
-- fac :: Int -> Int
-- fac = \n ->
--       case n of
--         0 -> 1
--         n -> n * fac (n -1)
--
--
-- fac :: Int -> Int
-- fac = \n -> if n == 0 then 1 else (n * fac (n -1))
--
-- fac :: Int -> Int
-- fac = \n -> case n == 0 of
--               True  -> 1
--               False -> n * fac (n -1)
--

fac :: Expr
fac = undefined
-- fac = Abs
--     where
--       n = Var 1 (Ty...)

