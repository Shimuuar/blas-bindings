{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances        #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.BLAS.Bindings.Level3
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--
-- Matrix-Matrix operations.
--
#include <altas/cblas.h>
module Numeric.BLAS.Bindings.Level3 (
    BLAS3(..),
    ) where

import Data.Complex
import Foreign         ( Ptr, with )
import Foreign.C.Types ( CInt(..), CDouble(..))

import Numeric.BLAS.Bindings.Types
import Numeric.BLAS.Bindings.Level2


-- | Types with matrix-matrix operations.
class (BLAS2 a) => BLAS3 a where
  -- | Performs one of the matrix-matrix operations @C :=
  --   alpha*op(A)*op(B) + beta*C@ where @op@ is noop, transposition
  --   or conjugation.
  gemm  :: RowOrder --
        -> Trans    -- ^ Operation for matrix /A/
        -> Trans    -- ^ Operation for matrix /B/
        -> Int      -- ^ Number of row of matrix op(/A/) and matrix /C/
        -> Int      -- ^ Number of columns of matrix @op(/B/)@ and matrix /C/
        -> Int      -- ^ Number of columns of matrix @op(/A/)@ and rows of matrix @op(/B/)@
        -> a        -- ^ Scalar /alpha/
        -> Ptr a    -- ^ Matrix /A/
        -> Int      -- ^ Leading dimension of /A/
        -> Ptr a    -- ^ Matrix /B/
        -> Int      -- ^ Leading dimension of /B/
        -> a        -- ^ Scalar /beta/
        -> Ptr a    -- ^ Matrix /C/
        -> Int      -- ^ Leading dimension of /C/
        -> IO ()

  -- | Performs one of operations @C := alpha*A*B + beta*C@ or @C :=
  --   alpha*B*A + beta*C@ where A is a symmetric matrix and B and C
  --   are m by n matrices.
  symm  :: RowOrder --
        -> Side     -- ^ Whether symmetric matrix appears on the left or on the right
        -> Uplo     -- ^ Upper or lower triangular part of matix should be used
        -> Int      -- ^ Number of row of matrix /B/ and /C/
        -> Int      -- ^ Number of columns of matrix /B/ and /C/
        -> a        -- ^ Scalar /alpha/
        -> Ptr a    -- ^ Matrix /A/
        -> Int      -- ^ Leading dimension of /A/
        -> Ptr a    -- ^ Matrix /B/
        -> Int      -- ^ Leading dimension of /B/
        -> a        -- ^ Scalar /beta/
        -> Ptr a    -- ^ Matrix /C/
        -> Int      -- ^ Leading dimension of /C/
        -> IO ()

  -- | Performs one of operations @C := alpha*A*B + beta*C@ or @C :=
  --   alpha*B*A + beta*C@ where A is a hermitian matrix and B and C
  --   are m by n matrices.
  hemm  :: RowOrder --
        -> Side     -- ^ Whether symmetric matrix appears on the left or on the right
        -> Uplo     -- ^ Upper or lower triangular part of matix should be used
        -> Int      -- ^ Number of row of matrix /B/ and /C/
        -> Int      -- ^ Number of columns of matrix /B/ and /C/
        -> a        -- ^ Scalar /alpha/
        -> Ptr a    -- ^ Matrix /A/
        -> Int      -- ^ Leading dimension of /A/
        -> Ptr a    -- ^ Matrix /B/
        -> Int      -- ^ Leading dimension of /B/
        -> a        -- ^ Scalar /beta/
        -> Ptr a    -- ^ Matrix /C/
        -> Int      -- ^ Leading dimension of /C/
        -> IO ()

  -- | Performs one of the matrix-matrix operations B := alpha*op( A
  -- )*B, or B := alpha*B*op( A ) where alpha is a scalar, B is an m
  -- by n matrix, A is a unit, or non-unit, upper or lower
  -- triangular matrix and op( A ) is one of op( A ) = A or op( A )
  -- = A' or op( A ) = conjg( A' )
  trmm  :: RowOrder --
        -> Side     -- ^ Whether symmetric matrix appears on the left or on the right
        -> Uplo     -- ^ Upper or lower triangular part of matix should be used
        -> Trans    -- ^ Transformation for matrix /A/
        -> Diag     -- ^ Whether matrix is unit diagonal or not.
        -> Int      -- ^ Number of rows of matrix /B/
        -> Int      -- ^ Number of columns of matrix /B/
        -> a        -- ^ Scalar /alpha/
        -> Ptr a    -- ^ Matrix /A/
        -> Int      -- ^ Leading dimension of /A/
        -> Ptr a    -- ^ Matrix /B/
        -> Int      -- ^ Leading dimension of /B/
        -> IO ()

  -- | Solves one of the matrix equations op( A )*X = alpha*B, or
  --   X*op( A ) = alpha*B where alpha is scalar, X and B are n by m
  --   matrices A is a unit, or non-unit, upper or lower triangular
  --   matrix and op is noop, transpose or Hermit conjugation
  trsm  :: RowOrder --
        -> Side     -- ^ Whether A appears on the left or right side of
                    --   X. 'L' : op(A)X = alpha*B, 'R' : Xop(A) = alpha*B
        -> Uplo     -- ^ Upper or lower triangular part of matix should
                    --   be used
        -> Trans    -- ^ Transformation applied to A
        -> Diag     -- ^ Whether A is unit triangular or not
        -> Int      -- ^ Number of rows of B
        -> Int      -- ^ Number of columns of B
        -> a        -- ^ Scalar alpha
        -> Ptr a    -- ^ Matrix A data
        -> Int      -- ^ Leading dimension of A
        -> Ptr a    -- ^ Matrix B. Will be overwritten by solution
        -> Int      -- ^ Leading dimension of B
        -> IO ()

  -- | performs one of the symmetric rank k operations C :=
  --   alpha*A*A' + beta*C, or C := alpha*A'*A + beta*C.
  syrk  :: RowOrder --
        -> Uplo     -- ^ Whether upper or lower triangular array of C is referneced.
        -> Trans    -- ^ Choses operation to perform:
                    --   TRANS = 'N' or 'n' C := alpha*A*A' + beta*C
                    --   TRANS = 'T' or 't' C := alpha*A'*A + beta*C.
        -> Int      -- ^ Order of matrix C
        -> Int      -- ^ If Trans is N number of columns of matrix A,
                    --   if Trans is T number of rows in matrix A.
        -> a        -- ^ Scalar alpha
        -> Ptr a    -- ^ Matrix A
        -> Int      -- ^ Leading dimension of A
        -> a        -- ^ Scalar beta
        -> Ptr a    -- ^ Matrix C
        -> Int      -- ^ Leading dimension of C
        -> IO ()

  -- | performs one of the symmetric rank 2k operations C :=
  -- alpha*A*B' + alpha*B*A' + beta*C or C := alpha*A'*B +
  -- alpha*B'*A + beta*C,
  syr2k :: RowOrder --
        -> Uplo     -- ^ Whether upper or lower triangular part of C is
                    --   referenced
        -> Trans    -- ^ On entry, TRANS specifies the operation to be performed as follows:
                    -- TRANS = 'N' or 'n' C := alpha*A*B' + alpha*B*A' + beta*C.
                    -- TRANS = 'T' or 't' C := alpha*A'*B + alpha*B'*A + beta*C.
        -> Int      -- ^ Order of matrix C
        -> Int      -- ^ If Trans is 'N' number of columns of matrices
                    --   A and B it's number of rows otherwise
        -> a        -- ^ Scalar alpha
        -> Ptr a    -- ^ Matrix A
        -> Int      -- ^ Leading dimension of A
        -> Ptr a    -- ^ Matrix B
        -> Int      -- ^ Leading dimension of B
        -> a        -- ^ Scalar beta
        -> Ptr a    -- ^ Matrix C
        -> Int      -- ^ Leading dimension of C
        -> IO ()

  -- | Performs one of the hermitian rank k operations C :=
  --   alpha*A*conjg( A' ) + beta*C, or C := alpha*conjg( A' )*A +
  --   beta*C, where alpha and beta are real scalars, C is an n by n
  --   hermitian matrix and A is an n by k matrix in the first case
  --   and a k by n matrix in the second case.
  herk  :: RowOrder --
        -> Uplo     -- ^ On entry, UPLO specifies whether the upper or
                    --   lower triangular part of the array C is to be
                    --   referenced as follows: UPLO = 'U' or 'u' Only
                    --   the upper triangular part of C is to be
                    --   referenced. UPLO = 'L' or 'l' Only the lower
                    --   triangular part of C is to be referenced.
        -> Trans    -- ^ On entry, TRANS specifies the operation to be
                    --   performed as follows: TRANS = 'N' or 'n' C :=
                    --   alpha*A*conjg( A' ) + beta*C. TRANS = 'C' or
                    --   'c' C := alpha*conjg( A' )*A + beta*C.
        -> Int      -- ^ Order of matrix C
        -> Int      -- ^ If Trans is N number of columns of matrix A,
                    --   if Trans is C number of rows in matrix A.
        -> Double   -- ^ Scalar alpha
        -> Ptr a    -- ^ Matrix A
        -> Int      -- ^ Leading dimension of A
        -> Double   -- ^ Scalar beta
        -> Ptr a    -- ^ Matrix C
        -> Int      -- ^ Leading dimension of C
        -> IO ()

  -- | performs one of the hermitian rank 2k operations C :=
  --   alpha*A*conjg( B' ) + conjg( alpha )*B*conjg( A' ) + beta*C,
  --   or C := alpha*conjg( A' )*B + conjg( alpha )*conjg( B' )*A +
  --   beta*C, where alpha and beta are scalars with beta real, C is
  --   an n by n hermitian matrix and A and B are n by k matrices in
  --   the first case and k by n matrices in the second case.
  her2k :: RowOrder --
        -> Uplo     -- ^ Whether upper or lower triangular part of C is
                    --   referenced
        -> Trans    -- ^ On entry, TRANS specifies the operation to be performed as follows:
                    -- TRANS = 'N' or 'n' C := alpha*A*conjg(B) + alpha*B*conjg(A) + beta*C.
                    -- TRANS = 'C' or 'c' C := alpha*conjg(A)*B + alpha*conjg(B)*A + beta*C.
        -> Int      -- ^ Order of matrix C
        -> Int      -- ^ If Trans is 'N' number of columns of matrices
                    --   A and B it's number of rows otherwise
        -> a        -- ^ Scalar alpha
        -> Ptr a    -- ^ Matrix A
        -> Int      -- ^ Leading dimension of A
        -> Ptr a    -- ^ Matrix B
        -> Int      -- ^ Leading dimension of B
        -> Double   -- ^ Scalar beta
        -> Ptr a    -- ^ Matrix C
        -> Int      -- ^ Leading dimension of C
        -> IO ()


instance BLAS3 Double where
  gemm ord transa transb m n k alpha pa lda pb ldb beta pc ldc =
    {#call cblas_dgemm #} (toOrder ord)
      (toTrans transa) (toTrans transb)
      (toI m) (toI n) (toI k)
      (toD alpha) (ptr pa) (toI lda)
                  (ptr pb) (toI ldb)
      (toD beta)  (ptr pc) (toI ldc)
  {-# INLINE gemm #-}

  symm ord side uplo m n alpha pa lda pb ldb beta pc ldc =
    {#call cblas_dsymm #} (toOrder ord)
       (toSide side) (toUplo uplo)
       (toI m) (toI n)
       (toD alpha) (ptr pa) (toI lda)
                   (ptr pb) (toI ldb)
       (toD beta)  (ptr pc) (toI ldc)
  {-# INLINE symm #-}

  hemm = symm
  {-# INLINE hemm #-}

  trmm ord side uplo transa diag m n alpha pa lda pb ldb =
    {#call cblas_dtrmm #} (toOrder ord)
      (toSide side) (toUplo uplo) (toTrans transa) (toDiag diag)
      (toI m) (toI n)
      (toD alpha) (ptr pa) (toI lda)
                  (ptr pb) (toI ldb)
  {-# INLINE trmm #-}

  trsm ord side uplo transa diag m n alpha pa lda pb ldb =
    {#call cblas_dtrsm #} (toOrder ord)
      (toSide side) (toUplo uplo) (toTrans transa) (toDiag diag)
      (toI m) (toI n)
      (toD alpha) (ptr pa) (toI lda)
                  (ptr pb) (toI ldb)
  {-# INLINE trsm #-}

  syrk ord uplo transa n k alpha pa lda beta pc ldc =
    {#call cblas_dsyrk #} (toOrder ord)
    (toUplo uplo) (toTrans transa)
    (toI n) (toI k)
    (toD alpha) (ptr pa)  (toI lda)
    (toD beta)  (ptr pc)  (toI ldc)
  {-# INLINE syrk #-}

  syr2k ord uplo transa n k alpha pa lda pb ldb beta pc ldc =
    {#call cblas_dsyr2k #} (toOrder ord)
      (toUplo uplo) (toTrans transa)
      (toI n) (toI k)
      (toD alpha) (ptr pa) (toI lda)
                  (ptr pb) (toI ldb)
      (toD beta)  (ptr pc) (toI ldc)
  {-# INLINE syr2k #-}

  herk  = syrk
  {-# INLINE herk #-}
  her2k = syr2k
  {-# INLINE her2k #-}


instance BLAS3 (Complex Double) where
  gemm ord transa transb m n k alpha pa lda pb ldb beta pc ldc =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
    {#call cblas_zgemm #} (toOrder ord)
      (toTrans transa) (toTrans transb)
      (toI m) (toI n) (toI k)
      (ptrC palpha) (ptrC pa) (toI lda)
                    (ptrC pb) (toI ldb)
      (ptrC pbeta)  (ptrC pc) (toI ldc)
  {-# INLINE gemm #-}

  symm ord side uplo m n alpha pa lda pb ldb beta pc ldc =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      {#call cblas_zsymm #} (toOrder ord)
        (toSide side) (toUplo uplo)
        (toI m) (toI n)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC pb) (toI ldb)
        (ptrC pbeta ) (ptrC pc) (toI ldc)
  {-# INLINE symm #-}

  hemm ord side uplo m n alpha pa lda pb ldb beta pc ldc =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      {#call cblas_zhemm #} (toOrder ord)
        (toSide side) (toUplo uplo)
        (toI m) (toI n)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC pb) (toI ldb)
        (ptrC pbeta ) (ptrC pc) (toI ldc)
  {-# INLINE hemm #-}

  trmm ord side uplo transa diag m n alpha pa lda pb ldb =
    with alpha $ \palpha ->
      {#call cblas_ztrmm #} (toOrder ord)
        (toSide side) (toUplo uplo) (toTrans transa) (toDiag diag)
        (toI m) (toI n)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC pb) (toI ldb)
  {-# INLINE trmm #-}

  trsm ord side uplo transa diag m n alpha pa lda pb ldb =
    with alpha $ \palpha ->
      {#call cblas_ztrsm #} (toOrder ord)
        (toSide side) (toUplo uplo) (toTrans transa) (toDiag diag)
        (toI m) (toI n)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC pb) (toI ldb)
  {-# INLINE trsm #-}

  syrk ord uplo transa n k alpha pa lda beta pc ldc =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
     {#call cblas_zsyrk #} (toOrder ord)
       (toUplo uplo) (toTrans transa)
       (toI n) (toI k)
       (ptrC palpha) (ptrC pa) (toI lda)
       (ptrC pbeta)  (ptrC pc) (toI ldc)
  {-# INLINE syrk #-}

  syr2k ord uplo transa n k alpha pa lda pb ldb beta pc ldc =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      {#call cblas_zsyr2k #} (toOrder ord)
        (toUplo uplo) (toTrans transa)
        (toI n) (toI k)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC pb) (toI ldb)
        (ptrC pbeta ) (ptrC pc) (toI ldc)
  {-# INLINE syr2k #-}

  herk ord uplo transa n k alpha pa lda beta pc ldc =
    {#call cblas_zherk #} (toOrder ord)
      (toUplo uplo) (toTrans transa)
      (toI n) (toI k)
      (toD alpha) (ptrC pa) (toI lda)
      (toD beta ) (ptrC pc) (toI ldc)
  {-# INLINE herk #-}

  her2k ord uplo transa n k alpha pa lda pb ldb beta pc ldc =
    with alpha $ \palpha ->
      {#call cblas_zher2k #} (toOrder ord)
        (toUplo uplo) (toTrans transa)
        (toI n) (toI k)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC pb) (toI ldb)
        (toD  beta  ) (ptrC pc) (toI ldc)
  {-# INLINE her2k #-}
