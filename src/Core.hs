{-# LANGUAGE FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Core(
  Expr,
  ExprF(..),
  Fix(..),
  Var(..),
  TyVar(..),
  Literal(..),
  AltCon(..)
)
where

import Prim
import Utils

import qualified Data.Text as T

data Var = V { varId :: Int,
               varType :: Type
             }
          deriving (Eq, Ord, Show)

data TyVar = TV { tvarId :: Int
             }
          deriving (Eq, Ord, Show)

data ExprF f =
   Var      Var
 | App         f f
 | Abs      Bind f
 | TyApp       f f
 | TyAbs    Bind f

 | Let      Bind f   f
 | LetRec [(Bind,f)] f -- (Mutually-)Recursive bindings

 | Case   f [(AltCon,[Bind],f)] -- NB. Case must be 'exhaustive' (which may be achieved by using 'DEFAULT')
                                -- Q. How to handle literals?
                                -- A. As 'guards' (c.f. SPLJ / Wadler Ch.4, p.58)
                                -- Guards are themselves compiled as 'IF' which is
                                -- itself compiled as 'case <bool> of True -> ... | False -> ...'

 | Lit      Literal
 | Prim     PrimOp   -- In GHC primops are represented as variables
 | InjectDataCon Int -- In GHC data ctrs are represented as variables
                     -- (ie as 'Var Id' but tagged with appropriate 'IdDetails'...)
                     -- we prefer to represent them explicitly .... Q. Is that sensible??

 deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data TypeF f =
   TVar TyVar
 | TApp f f
 | TAbs TyVar f

 | TLet TyVar f f
 | TLetRec [(TyVar,f)] f

 deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Expr = Fix ExprF
type Type = Fix TypeF
type Bind = Var

data AltCon = AltCon Int -- Int is the index of the data ctr being matched
            | DEFAULT
              deriving (Eq, Ord, Show)

data Literal = LitInt Int
             | LitBool Bool
             | LitChar Char
             | LitString T.Text
  deriving (Eq, Ord, Show)


