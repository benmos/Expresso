{-# LANGUAGE FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable, PatternSynonyms #-}
module Core(
  TypeEnv, -- ^ Kept abstract
  lookupTypeEnv,
  extendTypeEnv,
  emptyTypeEnv,

  primKindEnv,
  primTypeEnv,

  Expr,
  ExprF(..),
  Var(..),
  Literal(..),
  AltCon(..),
  DataCon(..),

  typeLit,

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

import qualified Data.IntMap as IM
import qualified Data.Text   as T

-- c.f. 'TyVar' in Type.hs
newtype Var = V { varId :: Int } deriving (Eq, Ord, Show)

newtype TypeEnv = TypeEnv { unTypeEnv :: IM.IntMap Type } deriving (Eq, Show) -- Var   -> Type

emptyTypeEnv :: TypeEnv
emptyTypeEnv = TypeEnv IM.empty

lookupTypeEnv :: TypeEnv -> Var -> Maybe Type
lookupTypeEnv te v = IM.lookup (varId v) (unTypeEnv te)

extendTypeEnv :: TypeEnv -> Var -> Type -> TypeEnv
extendTypeEnv te v t = TypeEnv $ IM.insert (varId v) t $ unTypeEnv te

unionTypeEnv :: TypeEnv -> TypeEnv -> TypeEnv
unionTypeEnv (TypeEnv t1) (TypeEnv t2) = TypeEnv (IM.union t1 t2)

primTypeEnv :: TypeEnv
primTypeEnv = -- foldr (\(var,t) te -> extendTypeEnv te var t) emptyTypeEnv [minBound .. maxBound]
              -- ++
              -- foldr (\(var,t) te -> extendTypeEnv te var t) emptyTypeEnv [minBound .. maxBound]
              -- FIXME !!!!!!!!!!!
              -- FIXME: Starting at 1000 is hideous hack. Work out how to assign these sensibly...
              -- FIXME !!!!!!!!!!!
              foldr (\np te -> uncurry (extendTypeEnv te) $ p1 np) emptyTypeEnv (zip [1000..] [minBound .. maxBound])
              `unionTypeEnv`
              foldr (\np te -> uncurry (extendTypeEnv te) $ p2 np) emptyTypeEnv (zip [2000..] [minBound .. maxBound])
    where
      p1 :: (Int,Prim1) -> (Var, Type)
      p1 (n, p) = (V n, primTy1 p)

      p2 :: (Int,Prim2) -> (Var, Type)
      p2 (n, p) = (V n, primTy2 p)

primKindEnv :: KindEnv
primKindEnv = KindEnv $ IM.map (const kindStar) $ unTypeEnv primTypeEnv

data ExprF f =
   Var      Var                -- (STLC)
 | App                  f f    -- (STLC)
 | Abs      Type Bind   f      -- (STLC)

 | TyApp                f Type -- (System-F)
 | TyAbs    Kind TyBind f      -- (System-F)

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

 deriving (Eq, Ord, Show, Functor, Foldable, Traversable) -- FIXME - Remove 'Eq' instance. We need alpha equiv!!!

type Expr   = Fix ExprF
type Bind   = Var
type TyBind = TyVar

data AltCon  = AltCon DataCon | DEFAULT deriving (Eq, Ord, Show)
data DataCon = DataCon { dcTag :: Int } -- Int is the index of the data ctr being matched
 deriving (Eq, Ord, Show)

data Literal = LitInt    Int
             | LitFloat  Double
             | LitChar   Char
             | LitString T.Text
             --  | LitBool   Bool -- No literal booleans, we use 'TrueCon', 'FalseCon' instead
  deriving (Eq, Ord, Show)

typeLit :: Literal -> Type
typeLit (LitInt    _) = TInt
typeLit (LitFloat  _) = TFloat
typeLit (LitChar   _) = TChar
typeLit (LitString _) = TString

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

