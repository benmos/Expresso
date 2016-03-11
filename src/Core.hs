{-# LANGUAGE FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Core(
  Expr,
  ExprF(..),
  Fix(..),
  Var(..),
  TyVar(..)
)
where

import Utils

data Var = V { varId :: Int,
               varType :: Type
             }
          deriving (Eq, Ord, Show)

data TyVar = TV { tvarId :: Int
             }
          deriving (Eq, Ord, Show)

data ExprF f =
   Var Var
 | App f f
 | Abs Var f
 | TyApp f f
 | TyAbs Var f
 deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data TypeF f =
   TVar TyVar
 | TApp f f
 | TAbs TyVar f
 deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Expr = Fix ExprF
type Type = Fix TypeF

