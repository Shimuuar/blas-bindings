{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.BLAS.Bindings.Types
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--
#include <atlas/cblas.h>
module Numeric.BLAS.Bindings.Types where

import Data.Complex     (Complex)
import Foreign          (Ptr,with,castPtr)
import Foreign.C.Types  (CInt,CDouble)
import Foreign.C.String (CString,withCString)
import Unsafe.Coerce


----------------------------------------------------------------
-- Enums
----------------------------------------------------------------

data RowOrder
  = RowMajor
  | ColMajor
  deriving (Eq, Show, Enum)

toOrder :: RowOrder -> CInt
toOrder RowMajor = fromIntegral $ fromEnum CblasRowMajor
toOrder ColMajor = fromIntegral $ fromEnum CblasColMajor
{-# INLINE toOrder #-}

{#enum CBLAS_ORDER {} #}



-- ^ Describe matrix transformation in matrix-vector or matrix-matrix
--   operation
data Trans
  = NoTrans   -- ^ Do not transpose matrix
  | Trans     -- ^ Transpose matrix
  | ConjTrans -- ^ Transpose and complex conjugate
  deriving (Eq, Show, Enum)

toTrans :: Trans -> CInt
toTrans NoTrans   = fromIntegral $ fromEnum CblasNoTrans
toTrans Trans     = fromIntegral $ fromEnum CblasTrans
toTrans ConjTrans = fromIntegral $ fromEnum CblasConjTrans
{-# INLINE toTrans #-}

{#enum CBLAS_TRANSPOSE {} #}



-- | Storage mode for symmetric/hermitian/triangular matrix
data Uplo
  = Upper -- ^ Upper part should be used
  | Lower -- ^ Lower part should be used
  deriving (Eq, Show, Enum)

toUplo :: Uplo -> CInt
toUplo Upper = fromIntegral $ fromEnum CblasUpper
toUplo Lower = fromIntegral $ fromEnum CblasLower
{-# INLINE toUplo #-}

{#enum CBLAS_UPLO {} #}



data Side
  = LeftSide
  | RightSide
  deriving (Eq, Show)

toSide :: Side -> CInt
toSide LeftSide  = fromIntegral $ fromEnum CblasLeft
toSide RightSide = fromIntegral $ fromEnum CblasRight
{-# INLINE toSide #-}

{#enum CBLAS_SIDE {} #}



data Diag
  = NonUnit
  | Unit
  deriving (Eq, Show)

toDiag :: Diag -> CInt
toDiag NonUnit = fromIntegral $ fromEnum CblasNonUnit
toDiag Unit    = fromIntegral $ fromEnum CblasUnit
{-# INLINE toDiag< #-}

{#enum CBLAS_DIAG {} #}



----------------------------------------------------------------
-- Conversions
----------------------------------------------------------------

toI :: Int -> CInt
toI = fromIntegral
{-# INLINE toI #-}

toD :: Double -> CDouble
toD = unsafeCoerce
{-# INLINE toD #-}

fromD :: CDouble -> Double
fromD = unsafeCoerce
{-# INLINE fromD #-}



ptr :: Ptr Double -> Ptr CDouble
ptr = castPtr
{-# INLINE ptr #-}

ptrC :: Ptr (Complex Double) -> Ptr ()
ptrC = castPtr
{-# INLINE ptrC #-}