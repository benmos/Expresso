{-# LANGUAGE FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable, PatternSynonyms #-}
module Core(
  Expr,
  ExprF(..),
  Var(..),
  Literal(..),
  AltCon(..),
  DataCon(..),

  -- * Pattern Synonyms
  pattern LInt,
  pattern LFloat,
  pattern LChar,
  pattern LString,
  pattern TrueCon,
  pattern FalseCon,
  pattern ETrue,
  pattern EFalse,

  -- * Functions
  binFun
)
where

import Prim
import Type
import Utils

import qualified Data.Text as T

data Var = V { varId :: Int,
               varType :: Type
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
                                -- A. As 'guards' (c.f. SLPJ / Wadler Ch.4, p.58)
                                -- Guards are themselves compiled as 'IF' which is
                                -- itself compiled as 'case <bool> of True -> ... | False -> ...'

 | Lit      Literal
 | Prim1    Prim1 f   -- In GHC primops are represented as variables. Also here we saturate.
 | Prim2    Prim2 f f -- In GHC primops are represented as variables. Also here we saturate.
 | Tuple    [f]       -- Could be done as a DataCon
 | Inject   DataCon   -- In GHC data ctrs are represented as variables
                      -- (ie as 'Var Id' but tagged with appropriate 'IdDetails'...)
                      -- we prefer to represent them explicitly .... Q. Is that sensible??

 deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Expr = Fix ExprF
type Bind = Var

data AltCon  = AltCon DataCon | DEFAULT deriving (Eq, Ord, Show)
data DataCon = DataCon { dcTag :: Int } -- Int is the index of the data ctr being matched
 deriving (Eq, Ord, Show)

data Literal = LitInt    Int
             | LitFloat  Double
             | LitChar   Char
             | LitString T.Text
             --  | LitBool   Bool -- No literal booleans, we use 'TrueCon', 'FalseCon' instead
  deriving (Eq, Ord, Show)

-- pattern LInt n <- Fix (Lit (LitInt n)) where
--         LInt n  = Fix (Lit (LitInt n))
pattern LInt  :: Int -> Expr
pattern LInt n = Fix (Lit (LitInt n))

pattern LFloat  :: Double -> Expr
pattern LFloat x = Fix (Lit (LitFloat x))

pattern LChar  :: Char -> Expr
pattern LChar x = Fix (Lit (LitChar x))

pattern LString  :: T.Text -> Expr
pattern LString x = Fix (Lit (LitString x))


pattern TrueCon  :: DataCon
pattern FalseCon :: DataCon
pattern TrueCon  = DataCon 1
pattern FalseCon = DataCon 0

pattern ETrue  :: Expr
pattern EFalse :: Expr
pattern ETrue    = Fix (Inject TrueCon)
pattern EFalse   = Fix (Inject FalseCon)

binFun :: Prim2 -> Expr -> Expr -> Expr
binFun p e1 e2 = Fix $ Prim2 p e1 e2

