module CoreCheck(
  coreCheck
)
where

import Core
import Prim
import Type
import Utils

import qualified Data.Text as T

-- | Returns 'Nothing' if type-correct.
coreCheck :: KindEnv -> TypeEnv -> Expr -> Maybe T.Text
coreCheck kenv tenv e = Nothing

