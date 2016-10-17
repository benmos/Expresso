{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, PatternSynonyms #-}
module Type(
  KindEnv, -- ^ Kept abstract
  lookupKindEnv,
  extendKindEnv,

  Kind,
  Type,
  TypeF(..),
  TyCon(..),
  TyVar(..),
  PrimTyCon(..),

  -- * Pattern Synonyms
  pattern TInt,
  pattern TFloat,
  pattern TBool,
  pattern TChar,
  pattern TString,

  -- * Functions
  binFunTy,
  unaryFun
)
where

import Utils

import qualified Data.IntMap as IM
import qualified Data.Text   as T

-- c.f. 'Var' in Core.hs
newtype TyVar = TV { tvarId :: Int} deriving (Eq, Ord, Show)

newtype KindEnv = KindEnv { unKindEnv :: IM.IntMap Kind } deriving (Eq, Show) -- TyVar -> Kind

lookupKindEnv :: KindEnv -> TyVar -> Maybe Kind
lookupKindEnv ke tv = IM.lookup (tvarId tv) (unKindEnv ke)

extendKindEnv :: KindEnv -> TyVar -> Kind -> KindEnv
extendKindEnv ke v k = KindEnv $ IM.insert (tvarId v) k $ unKindEnv ke

type Kind = () -- NYI
type Type = Fix TypeF

-- Greg Morrisett would probably call this 'ConstructorF', because not all constructors are types (ie of Kind *)
-- TODO: Rename ConstructorF
data TypeF f =
   TFunTy  f f          -- (STLC)
                        -- Arrow type -- GHC removed this fairly recently IIUC
                        -- IIUC GHC's "FunTy" used to be used for /saturated/
                        -- applications of the (->) TyCon (which would then be
                        -- implicit) ... which presumably means that 'TApp'
                        -- would sometimes be used for non-saturated
                        -- applications of (->). [http://www.cis.upenn.edu/~eir/files/ghc/ghc.pdf]
                        -- See also ["An overabundance of equality" paper (Fig.2)] for comment
                        -- on 'FunTy' / (->) type
                        -- See (slightly) old GHC:
                        -- [https://github.com/ghc/ghc/blob/6e56ac58a6905197412d58e32792a04a63b94d7e/compiler/types/TypeRep.hs]
                        -- Before: [https://github.com/ghc/ghc/tree/6e56ac58a6905197412d58e32792a04a63b94d7e/compiler/types]
                        -- After:  [https://github.com/ghc/ghc/tree/6746549772c5cc0ac66c0fce562f297f4d4b80a2/compiler/types]

 | TVar    TyVar        -- (System-F)
 | TForAll Kind TyVar f -- (System-F) Polymorphic types

 | TApp    f f          -- (System-Fw)
--  | TAbs Kind TyVar f -- (System Fw) Type-lambda

 | TCon TyCon
 | TTuple [f]
 | TLet      TyVar f   f
 | TLetRec [(TyVar,f)] f

 deriving (Eq, Ord, Show, Functor, Foldable, Traversable) -- FIXME - Remove 'Eq' instance. We need alpha equiv!!!

type Name = T.Text

data TyCon   = AlgTyCon Name -- Algebraic Data Type
             | PrimTyCon PrimTyCon
               deriving (Eq, Ord, Show)

data PrimTyCon = PTInt
               | PTFloat
               | PTBool
               | PTChar
               | PTString
  deriving (Eq, Ord, Show)


pattern TInt    :: Type
pattern TFloat  :: Type
pattern TBool   :: Type
pattern TChar   :: Type
pattern TString :: Type
pattern TInt    = Fix (TCon (PrimTyCon PTInt))
pattern TFloat  = Fix (TCon (PrimTyCon PTFloat))
pattern TBool   = Fix (TCon (PrimTyCon PTBool))
pattern TChar   = Fix (TCon (PrimTyCon PTChar))
pattern TString = Fix (TCon (PrimTyCon PTString))

-- | binFunTy t1 t2 t3 = (t1,t2) -> t3
--   NB - Tupled not curried
binFunTy :: Type -> Type -> Type -> Type
binFunTy t1 t2 t3 = Fix $ TFunTy (Fix $ TTuple [t1, t2]) t3

-- | unaryFun t1 t2 = t1 -> t2
unaryFun :: Type -> Type -> Type
unaryFun t1 t2 = Fix $ TFunTy t1 t2

