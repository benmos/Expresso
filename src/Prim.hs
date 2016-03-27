module Prim(
  Prim1(..),
  Prim2(..),
  primTy1,
  primTy2
)
where

import Type
-- import Utils

data Prim1 = PrimProject Int
           | PrimNot
           | PrimNegateInt
           | PrimNegateFloat
           | PrimAbsInt
           | PrimAbsFloat
  deriving (Eq, Ord, Show)

data Prim2 = PrimEqInt
           | PrimEqFloat
           | PrimEqBool
           | PrimEqChar
           | PrimEqString
           | PrimMulInt
           | PrimMulFloat
           | PrimSubInt
           | PrimSubFloat
           | PrimAddInt
           | PrimAddFloat
           | PrimDivInt
           | PrimDivFloat
           | PrimModInt
  deriving (Eq, Ord, Show)

primTy1 :: Prim1 -> Type
primTy1 (PrimProject _) = error "primTy1 - TODO - need to add type for polymorphism"
primTy1 PrimNot         = unaryFun TBool  TBool
primTy1 PrimNegateInt   = unaryFun TInt   TInt
primTy1 PrimNegateFloat = unaryFun TFloat TFloat
primTy1 PrimAbsInt      = unaryFun TInt   TInt
primTy1 PrimAbsFloat    = unaryFun TFloat TFloat


primTy2 :: Prim2 -> Type
primTy2 PrimEqInt    = binFunTy TInt    TInt    TBool
primTy2 PrimEqFloat  = binFunTy TFloat  TFloat  TBool
primTy2 PrimEqBool   = binFunTy TBool   TBool   TBool
primTy2 PrimEqChar   = binFunTy TChar   TChar   TBool
primTy2 PrimEqString = binFunTy TString TString TBool
primTy2 PrimMulInt   = binFunTy TInt    TInt    TInt
primTy2 PrimMulFloat = binFunTy TFloat  TFloat  TFloat
primTy2 PrimSubInt   = binFunTy TInt    TInt    TInt
primTy2 PrimSubFloat = binFunTy TFloat  TFloat  TFloat
primTy2 PrimAddInt   = binFunTy TInt    TInt    TInt
primTy2 PrimAddFloat = binFunTy TFloat  TFloat  TFloat
primTy2 PrimDivInt   = binFunTy TInt    TInt    TInt
primTy2 PrimDivFloat = binFunTy TFloat  TFloat  TFloat
primTy2 PrimModInt   = binFunTy TInt    TInt    TInt

