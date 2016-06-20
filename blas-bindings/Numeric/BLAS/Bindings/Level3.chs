{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances        #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.BLAS.Bindings.Level3
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>,
--              2012 Aleksey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Aleksey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
-- Matrix-Matrix operations.
--
#include "blas.h"
module Numeric.BLAS.Bindings.Level3 (
    BLAS3(..),
    ) where

import Data.Complex
import Foreign         ( Ptr, with )
import Foreign.C.Types ( CInt(..), CDouble(..), CFloat(..) )

import Numeric.BLAS.Bindings.Types
import Numeric.BLAS.Bindings.Trace
import Numeric.BLAS.Bindings.Level1 (RealType)
import Numeric.BLAS.Bindings.Level2 (BLAS2   )



-- | Types with matrix-matrix operations.
class (BLAS2 a) => BLAS3 a where
  -- | Performs one of the matrix-matrix operations on dense matrices:
  --
  -- > C ← α·op(A)·op(B) + β·C
  --
  --  where @op@ is noop, transposition or conjugation.
  gemm  :: RowOrder --
        -> Trans    -- ^ Operation for matrix /A/
        -> Trans    -- ^ Operation for matrix /B/
        -> Int      -- ^ Number of row of matrix op(/A/) and matrix /C/
        -> Int      -- ^ Number of columns of matrix @op(/B/)@ and matrix /C/
        -> Int      -- ^ Number of columns of matrix @op(/A/)@ and rows of matrix @op(/B/)@
        -> a        -- ^ Scalar /α/
        -> Ptr a    -- ^ Matrix /A/
        -> Int      -- ^ Leading dimension of /A/
        -> Ptr a    -- ^ Matrix /B/
        -> Int      -- ^ Leading dimension of /B/
        -> a        -- ^ Scalar /β/
        -> Ptr a    -- ^ Matrix /C/
        -> Int      -- ^ Leading dimension of /C/
        -> IO ()

  -- | Performs one of operations on dense matrices:
  --
  -- > C ← α·A·B + β·C
  -- > C ← α·B·A + β·C
  --
  -- where /A/ is a symmetric matrix and /B/ and /C/ are m by n matrices.
  symm  :: RowOrder --
        -> Side     -- ^ Whether symmetric matrix appears on the left or on the right
        -> Uplo     -- ^ Upper or lower triangular part of matix should be used
        -> Int      -- ^ Number of row of matrix /B/ and /C/
        -> Int      -- ^ Number of columns of matrix /B/ and /C/
        -> a        -- ^ Scalar /α/
        -> Ptr a    -- ^ Matrix /A/
        -> Int      -- ^ Leading dimension of /A/
        -> Ptr a    -- ^ Matrix /B/
        -> Int      -- ^ Leading dimension of /B/
        -> a        -- ^ Scalar /β/
        -> Ptr a    -- ^ Matrix /C/
        -> Int      -- ^ Leading dimension of /C/
        -> IO ()

  -- | Performs one of operations
  --
  -- > C ← α·A·B + β·C
  -- > C ← α·B·A + β·C
  --
  --   where A is a hermitian matrix (or symmetric for real /a/) and B
  --   and C are m by n matrices.
  hemm  :: RowOrder --
        -> Side     -- ^ Whether symmetric matrix appears on the left or on the right
        -> Uplo     -- ^ Upper or lower triangular part of matix should be used
        -> Int      -- ^ Number of row of matrix /B/ and /C/
        -> Int      -- ^ Number of columns of matrix /B/ and /C/
        -> a        -- ^ Scalar /α/
        -> Ptr a    -- ^ Matrix /A/
        -> Int      -- ^ Leading dimension of /A/
        -> Ptr a    -- ^ Matrix /B/
        -> Int      -- ^ Leading dimension of /B/
        -> a        -- ^ Scalar /β/
        -> Ptr a    -- ^ Matrix /C/
        -> Int      -- ^ Leading dimension of /C/
        -> IO ()

  -- | Performs one of the matrix-matrix operations
  --
  -- > B ← α·op(A)·B
  -- > B ← α·B·op(A)
  --
  --   where /α/ is a scalar, /B/ is an /m/ by /n/ matrix, /A/ is a
  --   unit, or non-unit, upper or lower triangular matrix.
  trmm  :: RowOrder --
        -> Side     -- ^ Whether symmetric matrix appears on the left or on the right
        -> Uplo     -- ^ Upper or lower triangular part of matix should be used
        -> Trans    -- ^ Transformation for matrix /A/
        -> Diag     -- ^ Whether matrix is unit diagonal or not.
        -> Int      -- ^ Number of rows of matrix /B/
        -> Int      -- ^ Number of columns of matrix /B/
        -> a        -- ^ Scalar /α/
        -> Ptr a    -- ^ Matrix /A/
        -> Int      -- ^ Leading dimension of /A/
        -> Ptr a    -- ^ Matrix /B/
        -> Int      -- ^ Leading dimension of /B/
        -> IO ()

  -- | Solves one of the matrix equations
  --
  -- > op(A)·X = α·B
  -- > X·op(A) = α·B
  --
  --   where /α/ is scalar, /X/ and /B/ are /n/ by /m/ matrices, /A/
  --   is a unit, or non-unit, upper or lower triangular matrix.
  trsm  :: RowOrder --
        -> Side     -- ^ Whether A appears on the left or right side of
                    --   X. 'L' : op(A)X = α·B, 'R' : Xop(A) = α·B
        -> Uplo     -- ^ Upper or lower triangular part of matix should
                    --   be used
        -> Trans    -- ^ Transformation applied to A
        -> Diag     -- ^ Whether A is unit triangular or not
        -> Int      -- ^ Number of rows of B
        -> Int      -- ^ Number of columns of B
        -> a        -- ^ Scalar α
        -> Ptr a    -- ^ Matrix A data
        -> Int      -- ^ Leading dimension of A
        -> Ptr a    -- ^ Matrix B. Will be overwritten by solution
        -> Int      -- ^ Leading dimension of B
        -> IO ()

  -- | performs one of the symmetric rank k operations
  --
  -- > C <- α·A·A' + β·C
  -- > C <- α·A'·A + β·C
  syrk  :: RowOrder --
        -> Uplo     -- ^ Whether upper or lower triangular array of C is referneced.
        -> Trans    -- ^ Choses operation to perform:
                    --   TRANS = 'N' or 'n' C := α·A·A' + β·C
                    --   TRANS = 'T' or 't' C := α·A'·A + β·C.
        -> Int      -- ^ Order of matrix C
        -> Int      -- ^ If Trans is N number of columns of matrix A,
                    --   if Trans is T number of rows in matrix A.
        -> a        -- ^ Scalar α
        -> Ptr a    -- ^ Matrix A
        -> Int      -- ^ Leading dimension of A
        -> a        -- ^ Scalar β
        -> Ptr a    -- ^ Matrix C
        -> Int      -- ^ Leading dimension of C
        -> IO ()

  -- | performs one of the symmetric rank 2k operations]
  --
  -- > C ← α·A·B' + α·B·A' + β·C
  -- > C ← α·A'·B + α·B'·A + β·C
  syr2k :: RowOrder --
        -> Uplo     -- ^ Whether upper or lower triangular part of C is
                    --   referenced
        -> Trans    -- ^ On entry, TRANS specifies the operation to be performed as follows:
                    -- TRANS = 'N' or 'n' C := α·A·B' + α·B·A' + β·C.
                    -- TRANS = 'T' or 't' C := α·A'·B + α·B'·A + β·C.
        -> Int      -- ^ Order of matrix C
        -> Int      -- ^ If Trans is 'N' number of columns of matrices
                    --   A and B it's number of rows otherwise
        -> a        -- ^ Scalar α
        -> Ptr a    -- ^ Matrix A
        -> Int      -- ^ Leading dimension of A
        -> Ptr a    -- ^ Matrix B
        -> Int      -- ^ Leading dimension of B
        -> a        -- ^ Scalar β
        -> Ptr a    -- ^ Matrix C
        -> Int      -- ^ Leading dimension of C
        -> IO ()

  -- | Performs one of the hermitian rank k operations
  --
  -- > C ← α·A·conjg(A') + β·C
  -- > C ← α·conjg(A')·A + β·C
  --
  --   where /α/ and /β/ are real scalars, /C/ is an /n/ by /n/
  --   hermitian matrix and /A/ is an /n/ by /k/ matrix in the first
  --   case and a /k/ by /n/ matrix in the second case.
  herk  :: RowOrder   --
        -> Uplo       -- ^ On entry, UPLO specifies whether the upper or
                      --   lower triangular part of the array C is to be
                      --   referenced as follows: UPLO = 'U' or 'u' Only
                      --   the upper triangular part of C is to be
                      --   referenced. UPLO = 'L' or 'l' Only the lower
                      --   triangular part of C is to be referenced.
        -> Trans      -- ^ On entry, TRANS specifies the operation to be
                      --   performed as follows: TRANS = 'N' or 'n' C :=
                      --   α·A·conjg( A' ) + β·C. TRANS = 'C' or
                      --   'c' C := α·conjg( A' )·A + β·C.
        -> Int        -- ^ Order of matrix C
        -> Int        -- ^ If Trans is N number of columns of matrix A,
                      --   if Trans is C number of rows in matrix A.
        -> RealType a -- ^ Scalar α
        -> Ptr a      -- ^ Matrix A
        -> Int        -- ^ Leading dimension of A
        -> RealType a -- ^ Scalar β
        -> Ptr a      -- ^ Matrix C
        -> Int        -- ^ Leading dimension of C
        -> IO ()

  -- | performs one of the hermitian rank 2k operations
  --
  -- > C ← α·A·conjg(B') + conjg(α)·B·conjg(A') + β·C
  -- > C ← α·conjg(A')·B + conjg(α)·conjg(B')·A + β·C
  --
  --   where /α/ and /β/ are scalars with /β/ real, /C/ is an /n/ by
  --   /n/ hermitian matrix and /A/ and /B/ are /n/ by /k/ matrices in
  --   the first case and /k/ by /n/ matrices in the second case.
  her2k :: RowOrder   --
        -> Uplo       -- ^ Whether upper or lower triangular part of C is
                      --   referenced
        -> Trans      -- ^ On entry, TRANS specifies the operation to be performed as follows:
                      -- TRANS = 'N' or 'n' C := α·A·conjg(B) + α·B·conjg(A) + β·C.
                      -- TRANS = 'C' or 'c' C := α·conjg(A)·B + α·conjg(B)·A + β·C.
        -> Int        -- ^ Order of matrix C
        -> Int        -- ^ If Trans is 'N' number of columns of matrices
                      --   A and B it's number of rows otherwise
        -> a          -- ^ Scalar α
        -> Ptr a      -- ^ Matrix A
        -> Int        -- ^ Leading dimension of A
        -> Ptr a      -- ^ Matrix B
        -> Int        -- ^ Leading dimension of B
        -> RealType a -- ^ Scalar β
        -> Ptr a      -- ^ Matrix C
        -> Int        -- ^ Leading dimension of C
        -> IO ()



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance BLAS3 Double where
  gemm ord transa transb m n k alpha pa lda pb ldb beta pc ldc =
    traceBLAS "gemm"
    {#call cblas_dgemm #} (toOrder ord)
      (toTrans transa) (toTrans transb)
      (toI m) (toI n) (toI k)
      (toD alpha) (ptrD pa) (toI lda)
                  (ptrD pb) (toI ldb)
      (toD beta)  (ptrD pc) (toI ldc)
  {-# INLINE gemm #-}

  symm ord side uplo m n alpha pa lda pb ldb beta pc ldc =
    traceBLAS "symm"
    {#call cblas_dsymm #} (toOrder ord)
       (toSide side) (toUplo uplo)
       (toI m) (toI n)
       (toD alpha) (ptrD pa) (toI lda)
                   (ptrD pb) (toI ldb)
       (toD beta)  (ptrD pc) (toI ldc)
  {-# INLINE symm #-}

  hemm = symm
  {-# INLINE hemm #-}

  trmm ord side uplo transa diag m n alpha pa lda pb ldb =
    traceBLAS "trmm"
    {#call cblas_dtrmm #} (toOrder ord)
      (toSide side) (toUplo uplo) (toTrans transa) (toDiag diag)
      (toI m) (toI n)
      (toD alpha) (ptrD pa) (toI lda)
                  (ptrD pb) (toI ldb)
  {-# INLINE trmm #-}

  trsm ord side uplo transa diag m n alpha pa lda pb ldb =
    traceBLAS "trsm"
    {#call cblas_dtrsm #} (toOrder ord)
      (toSide side) (toUplo uplo) (toTrans transa) (toDiag diag)
      (toI m) (toI n)
      (toD alpha) (ptrD pa) (toI lda)
                  (ptrD pb) (toI ldb)
  {-# INLINE trsm #-}

  syrk ord uplo transa n k alpha pa lda beta pc ldc =
    traceBLAS "syrk"
    {#call cblas_dsyrk #} (toOrder ord)
    (toUplo uplo) (toTrans transa)
    (toI n) (toI k)
    (toD alpha) (ptrD pa)  (toI lda)
    (toD beta)  (ptrD pc)  (toI ldc)
  {-# INLINE syrk #-}

  syr2k ord uplo transa n k alpha pa lda pb ldb beta pc ldc =
    traceBLAS "syr2k"
    {#call cblas_dsyr2k #} (toOrder ord)
      (toUplo uplo) (toTrans transa)
      (toI n) (toI k)
      (toD alpha) (ptrD pa) (toI lda)
                  (ptrD pb) (toI ldb)
      (toD beta)  (ptrD pc) (toI ldc)
  {-# INLINE syr2k #-}

  herk  = syrk
  {-# INLINE herk #-}
  her2k = syr2k
  {-# INLINE her2k #-}



instance BLAS3 Float where
  gemm ord transa transb m n k alpha pa lda pb ldb beta pc ldc =
    traceBLAS "gemm"
    {#call cblas_sgemm #} (toOrder ord)
      (toTrans transa) (toTrans transb)
      (toI m) (toI n) (toI k)
      (toF alpha) (ptrF pa) (toI lda)
                  (ptrF pb) (toI ldb)
      (toF beta)  (ptrF pc) (toI ldc)
  {-# INLINE gemm #-}

  symm ord side uplo m n alpha pa lda pb ldb beta pc ldc =
    traceBLAS "symm"
    {#call cblas_ssymm #} (toOrder ord)
       (toSide side) (toUplo uplo)
       (toI m) (toI n)
       (toF alpha) (ptrF pa) (toI lda)
                   (ptrF pb) (toI ldb)
       (toF beta)  (ptrF pc) (toI ldc)
  {-# INLINE symm #-}

  hemm = symm
  {-# INLINE hemm #-}

  trmm ord side uplo transa diag m n alpha pa lda pb ldb =
    traceBLAS "trmm"
    {#call cblas_strmm #} (toOrder ord)
      (toSide side) (toUplo uplo) (toTrans transa) (toDiag diag)
      (toI m) (toI n)
      (toF alpha) (ptrF pa) (toI lda)
                  (ptrF pb) (toI ldb)
  {-# INLINE trmm #-}

  trsm ord side uplo transa diag m n alpha pa lda pb ldb =
    traceBLAS "trsm"
    {#call cblas_strsm #} (toOrder ord)
      (toSide side) (toUplo uplo) (toTrans transa) (toDiag diag)
      (toI m) (toI n)
      (toF alpha) (ptrF pa) (toI lda)
                  (ptrF pb) (toI ldb)
  {-# INLINE trsm #-}

  syrk ord uplo transa n k alpha pa lda beta pc ldc =
    traceBLAS "syrk"
    {#call cblas_ssyrk #} (toOrder ord)
    (toUplo uplo) (toTrans transa)
    (toI n) (toI k)
    (toF alpha) (ptrF pa)  (toI lda)
    (toF beta)  (ptrF pc)  (toI ldc)
  {-# INLINE syrk #-}

  syr2k ord uplo transa n k alpha pa lda pb ldb beta pc ldc =
    traceBLAS "syr2k"
    {#call cblas_ssyr2k #} (toOrder ord)
      (toUplo uplo) (toTrans transa)
      (toI n) (toI k)
      (toF alpha) (ptrF pa) (toI lda)
                  (ptrF pb) (toI ldb)
      (toF beta)  (ptrF pc) (toI ldc)
  {-# INLINE syr2k #-}

  herk  = syrk
  {-# INLINE herk #-}
  her2k = syr2k
  {-# INLINE her2k #-}



instance BLAS3 (Complex Float) where
  gemm ord transa transb m n k alpha pa lda pb ldb beta pc ldc =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
    traceBLAS "gemm"
    {#call cblas_cgemm #} (toOrder ord)
      (toTrans transa) (toTrans transb)
      (toI m) (toI n) (toI k)
      (ptrC palpha) (ptrC pa) (toI lda)
                    (ptrC pb) (toI ldb)
      (ptrC pbeta)  (ptrC pc) (toI ldc)
  {-# INLINE gemm #-}

  symm ord side uplo m n alpha pa lda pb ldb beta pc ldc =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "symm"
      {#call cblas_csymm #} (toOrder ord)
        (toSide side) (toUplo uplo)
        (toI m) (toI n)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC pb) (toI ldb)
        (ptrC pbeta ) (ptrC pc) (toI ldc)
  {-# INLINE symm #-}

  hemm ord side uplo m n alpha pa lda pb ldb beta pc ldc =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "hemm"
      {#call cblas_chemm #} (toOrder ord)
        (toSide side) (toUplo uplo)
        (toI m) (toI n)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC pb) (toI ldb)
        (ptrC pbeta ) (ptrC pc) (toI ldc)
  {-# INLINE hemm #-}

  trmm ord side uplo transa diag m n alpha pa lda pb ldb =
    with alpha $ \palpha ->
      traceBLAS "trmm"
      {#call cblas_ctrmm #} (toOrder ord)
        (toSide side) (toUplo uplo) (toTrans transa) (toDiag diag)
        (toI m) (toI n)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC pb) (toI ldb)
  {-# INLINE trmm #-}

  trsm ord side uplo transa diag m n alpha pa lda pb ldb =
    with alpha $ \palpha ->
      traceBLAS "trsm"
      {#call cblas_ctrsm #} (toOrder ord)
        (toSide side) (toUplo uplo) (toTrans transa) (toDiag diag)
        (toI m) (toI n)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC pb) (toI ldb)
  {-# INLINE trsm #-}

  syrk ord uplo transa n k alpha pa lda beta pc ldc =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
     traceBLAS "syrk"
     {#call cblas_csyrk #} (toOrder ord)
       (toUplo uplo) (toTrans transa)
       (toI n) (toI k)
       (ptrC palpha) (ptrC pa) (toI lda)
       (ptrC pbeta)  (ptrC pc) (toI ldc)
  {-# INLINE syrk #-}

  syr2k ord uplo transa n k alpha pa lda pb ldb beta pc ldc =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "syr2k"
      {#call cblas_csyr2k #} (toOrder ord)
        (toUplo uplo) (toTrans transa)
        (toI n) (toI k)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC pb) (toI ldb)
        (ptrC pbeta ) (ptrC pc) (toI ldc)
  {-# INLINE syr2k #-}

  herk ord uplo transa n k alpha pa lda beta pc ldc =
    traceBLAS "herk"
    {#call cblas_cherk #} (toOrder ord)
      (toUplo uplo) (toTrans transa)
      (toI n) (toI k)
      (toF alpha) (ptrC pa) (toI lda)
      (toF beta ) (ptrC pc) (toI ldc)
  {-# INLINE herk #-}

  her2k ord uplo transa n k alpha pa lda pb ldb beta pc ldc =
    with alpha $ \palpha ->
      traceBLAS "her2k"
      {#call cblas_cher2k #} (toOrder ord)
        (toUplo uplo) (toTrans transa)
        (toI n) (toI k)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC pb) (toI ldb)
        (toF  beta  ) (ptrC pc) (toI ldc)
  {-# INLINE her2k #-}



instance BLAS3 (Complex Double) where
  gemm ord transa transb m n k alpha pa lda pb ldb beta pc ldc =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
    traceBLAS "gemm"
    {#call cblas_zgemm #} (toOrder ord)
      (toTrans transa) (toTrans transb)
      (toI m) (toI n) (toI k)
      (ptrZ palpha) (ptrZ pa) (toI lda)
                    (ptrZ pb) (toI ldb)
      (ptrZ pbeta)  (ptrZ pc) (toI ldc)
  {-# INLINE gemm #-}

  symm ord side uplo m n alpha pa lda pb ldb beta pc ldc =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "symm"
      {#call cblas_zsymm #} (toOrder ord)
        (toSide side) (toUplo uplo)
        (toI m) (toI n)
        (ptrZ palpha) (ptrZ pa) (toI lda)
                      (ptrZ pb) (toI ldb)
        (ptrZ pbeta ) (ptrZ pc) (toI ldc)
  {-# INLINE symm #-}

  hemm ord side uplo m n alpha pa lda pb ldb beta pc ldc =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "hemm"
      {#call cblas_zhemm #} (toOrder ord)
        (toSide side) (toUplo uplo)
        (toI m) (toI n)
        (ptrZ palpha) (ptrZ pa) (toI lda)
                      (ptrZ pb) (toI ldb)
        (ptrZ pbeta ) (ptrZ pc) (toI ldc)
  {-# INLINE hemm #-}

  trmm ord side uplo transa diag m n alpha pa lda pb ldb =
    with alpha $ \palpha ->
      traceBLAS "trmm"
      {#call cblas_ztrmm #} (toOrder ord)
        (toSide side) (toUplo uplo) (toTrans transa) (toDiag diag)
        (toI m) (toI n)
        (ptrZ palpha) (ptrZ pa) (toI lda)
                      (ptrZ pb) (toI ldb)
  {-# INLINE trmm #-}

  trsm ord side uplo transa diag m n alpha pa lda pb ldb =
    with alpha $ \palpha ->
      traceBLAS "trsm"
      {#call cblas_ztrsm #} (toOrder ord)
        (toSide side) (toUplo uplo) (toTrans transa) (toDiag diag)
        (toI m) (toI n)
        (ptrZ palpha) (ptrZ pa) (toI lda)
                      (ptrZ pb) (toI ldb)
  {-# INLINE trsm #-}

  syrk ord uplo transa n k alpha pa lda beta pc ldc =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
     traceBLAS "syrk"
     {#call cblas_zsyrk #} (toOrder ord)
       (toUplo uplo) (toTrans transa)
       (toI n) (toI k)
       (ptrZ palpha) (ptrZ pa) (toI lda)
       (ptrZ pbeta)  (ptrZ pc) (toI ldc)
  {-# INLINE syrk #-}

  syr2k ord uplo transa n k alpha pa lda pb ldb beta pc ldc =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "syr2k"
      {#call cblas_zsyr2k #} (toOrder ord)
        (toUplo uplo) (toTrans transa)
        (toI n) (toI k)
        (ptrZ palpha) (ptrZ pa) (toI lda)
                      (ptrZ pb) (toI ldb)
        (ptrZ pbeta ) (ptrZ pc) (toI ldc)
  {-# INLINE syr2k #-}

  herk ord uplo transa n k alpha pa lda beta pc ldc =
    traceBLAS "herk"
    {#call cblas_zherk #} (toOrder ord)
      (toUplo uplo) (toTrans transa)
      (toI n) (toI k)
      (toD alpha) (ptrZ pa) (toI lda)
      (toD beta ) (ptrZ pc) (toI ldc)
  {-# INLINE herk #-}

  her2k ord uplo transa n k alpha pa lda pb ldb beta pc ldc =
    with alpha $ \palpha ->
      traceBLAS "her2k"
      {#call cblas_zher2k #} (toOrder ord)
        (toUplo uplo) (toTrans transa)
        (toI n) (toI k)
        (ptrZ palpha) (ptrZ pa) (toI lda)
                      (ptrZ pb) (toI ldb)
        (toD  beta  ) (ptrZ pc) (toI ldc)
  {-# INLINE her2k #-}
