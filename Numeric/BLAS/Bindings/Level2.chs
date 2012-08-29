{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances        #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.BLAS.Bindings.Level2
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>,
--              2012 Aleksey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Aleksey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
-- Matrix-Vector operations.
--
#include "blas.h"
module Numeric.BLAS.Bindings.Level2 (
    BLAS2(..),
    ) where

import Data.Complex
import Foreign         ( Ptr, with )
import Foreign.C.Types ( CInt(..), CDouble(..), CFloat(..) )

import Numeric.BLAS.Bindings.Types
import Numeric.BLAS.Bindings.Trace
import Numeric.BLAS.Bindings.Level1



-- | Level 2 BLAS. Matrix-vector operations.
class (BLAS1 a) => BLAS2 a where
  -- OPERATION: aMV + bV

  -- | Compute matrix-vector multiplication with dense matrix:
  --
  --  > y ← α·op(A)·x + β·y
  --
  --   Matrix /A/ is transformed according to 'Trans' parameter.
  gemv :: RowOrder --
       -> Trans    -- ^ Matrix transformation
       -> Int      -- ^ /M/ number of rows
       -> Int      -- ^ /N/ number of columns
       -> a        -- ^ Scalar /α/
       -> Ptr a    -- ^ Pointer to matrix /A/
       -> Int      -- ^ /LDA/ Leading dimension size of /A/
       -> Ptr a    -- ^ Vector /x/
       -> Int      -- ^ Stride of /x/
       -> a        -- ^ Scalar /β/
       -> Ptr a    -- ^ Vector /y/
       -> Int      -- ^ Stride of /y/
       -> IO ()

  -- | Perform matrix-vector operation with dense hermitian/symmetric
  --   matrix:
  --
  --   > y ← α·A·x + β·y
  hemv :: RowOrder --
       -> Uplo     -- ^ Hermitian/symmetric matrix storage mode
       -> Int      -- ^ /N/ order of the matrix
       -> a        -- ^ Scalar /α/
       -> Ptr a    -- ^ Matrix data
       -> Int      -- ^ /LDA/ Leading dimension size of /A/
       -> Ptr a    -- ^ Vector /x/
       -> Int      -- ^ Stride for /x/
       -> a        -- ^ Scalar /β/
       -> Ptr a    -- ^ Vector /y/
       -> Int      -- ^ Stride for /y/
       -> IO ()

  -- | Perform matrix-vector operation with packed hermitian/symmetric
  --   matrix:
  --
  --   > y ← α·A·x + β·y
  hpmv :: RowOrder --
       -> Uplo   -- ^ Hermitian/symmetric matrix storage mode
       -> Int    -- ^ /N/ order of the matrix
       -> a      -- ^ Scalar /α/
       -> Ptr a  -- ^ Matrix data
       -> Ptr a  -- ^ Vector /x/
       -> Int    -- ^ Stride for /x/
       -> a      -- ^ Scalar /β/
       -> Ptr a  -- ^ Vector /y/
       -> Int    -- ^ Stride for /y/
       -> IO ()

  -- | Perform matrix-vector operation with banded matrix:
  --
  --   > y ← α·op(A)·x + β·y
  gbmv :: RowOrder --
       -> Trans    -- ^ Matrix transformation
       -> Int      -- ^ /M/  number of row
       -> Int      -- ^ /N/  number of columns
       -> Int      -- ^ /KL/ number of sub-diagonals @KL >= 0@
       -> Int      -- ^ /KU/ number of super-diagonals @KU >= 0@
       -> a        -- ^ Scalar /α/
       -> Ptr a    -- ^ Matrix data
       -> Int      -- ^ /LDA/ leading dimension of matrix /A/, @LDA >= KL + KU + 1@
       -> Ptr a    -- ^ Vector /x/
       -> Int      -- ^ Stride of /x/
       -> a        -- ^ Scalar /β/
       -> Ptr a    -- ^ Vector /y/
       -> Int      -- ^ Stride of /y/
       -> IO ()


  -- | Perform matrix-vector operation with hermitian/symmetric banded
  --   matrix with /K/ superdiagonals:
  --
  --   > y ← α·A·x + β·y
  hbmv :: RowOrder --
       -> Uplo     -- ^ Hermitian/symmetric matrix storage mode
       -> Int      -- ^ /N/ Size of matrix
       -> Int      -- ^ /K/ Number of super-diagonals
       -> a        -- ^ Scalar /α/
       -> Ptr a    -- ^ Matrix data
       -> Int      -- ^ /LDA/ Leading dimension size of /A/
       -> Ptr a    -- ^ Vector /x/
       -> Int      -- ^ Stride for /x/
       -> a        -- ^ Scalar /β/
       -> Ptr a    -- ^ Vector /y/
       -> Int      -- ^ Stride for /y/
       -> IO ()

  -- RANK 1 operations

  -- | Perform rank-1 operation. It's same as 'geru' for real valued vector.
  --
  -- > A ← α·x·conjg(y') + A
  gerc :: RowOrder --
       -> Int      -- ^ Number of rows
       -> Int      -- ^ Number of columns
       -> a        -- ^ Scalar /α/
       -> Ptr a    -- ^ Vector /x/
       -> Int      -- ^ Stride of /x/
       -> Ptr a    -- ^ Vector /y/
       -> Int      -- ^ Stride of /y/
       -> Ptr a    -- ^ Matrix /A/
       -> Int      -- ^ /LDA/ leading dimension of /A/
       -> IO ()

  -- | Perform rank-1 operation. It's same as 'gerc' for real values vectors.
  --
  -- > A ← α·x·y' + A
  geru :: RowOrder --
       -> Int      -- ^ Number of rows
       -> Int      -- ^ Number of columns
       -> a        -- ^ Scalar /α/
       -> Ptr a    -- ^ Vector /x/
       -> Int      -- ^ Stride of /x/
       -> Ptr a    -- ^ Vector /y/
       -> Int      -- ^ Stride of /y/
       -> Ptr a    -- ^ Matrix /A/
       -> Int      -- ^ /LDA/ leading dimension of /A/
       -> IO ()

  -- | Perform operation
  --
  -- > A ← α·x·conjg(x') + A
  --
  --   /A/ is hermitian or symmetric matrix
  her  :: RowOrder   --
       -> Uplo       -- ^ Hermitian/symmetric matrix storage mode
       -> Int        -- ^ Size of matrix
       -> RealType a -- ^ Scalar /α/
       -> Ptr a      -- ^ Vector /x/
       -> Int        -- ^ Stride of /x/
       -> Ptr a      -- ^ Matrix data
       -> Int        -- ^ /LDA/ leading dimension of /A/
       -> IO ()

  -- | Perform operation
  --
  -- > A ← α·x·conjg(y') + conjg(α)·y·conjg(x') + A
  --
  --   /A/ is hermitian or symmetric matrix.
  her2 :: RowOrder --
       -> Uplo     -- ^ Hermitian/symmetric matrix storage mode
       -> Int      -- ^ Size of matrix
       -> a        -- ^ Scalar /α/
       -> Ptr a    -- ^ Vector /x/
       -> Int      -- ^ Stride for /x/
       -> Ptr a    -- ^ Vector /y/
       -> Int      -- ^ Stride for /y/
       -> Ptr a    -- ^ Matrix data
       -> Int      -- ^ First dimenstion of matrix
       -> IO ()

  -- | Perform operation
  --
  -- > A ← α·x·conjg(x) + A
  --
  --   where /A/ is packed hermitian/symmetric matrix
  hpr  :: RowOrder   --
       -> Uplo       -- ^ Hermitian/symmetric matrix storage mode
       -> Int        -- ^ Matri order
       -> RealType a -- ^ Scalar /α/
       -> Ptr a      -- ^ Vector /x/
       -> Int        -- ^ Stride for /x/
       -> Ptr a      -- ^ Matrix data
       -> IO ()

  -- | Perform operation
  --
  -- > A ← α·x·conjg(y') + conjg(α)·y·conjg(x') + A
  --
  --   /A/ is packed hermitian or symmetric matrix.
  hpr2 :: RowOrder --
       -> Uplo     -- ^ Hermitian/symmetric matrix storage mode
       -> Int      -- ^ Matrix order
       -> a        -- ^ Scalar /α/
       -> Ptr a    -- ^ Vector /x/
       -> Int      -- ^ Stride for /x/
       -> Ptr a    -- ^ Vector /y/
       -> Int      -- ^ Stride for /y/
       -> Ptr a    -- ^ Matrix /A/
       -> IO ()

  -- Vector transformations

  -- | Perform one of the operations which are selected by 'Trans'
  --   parameter:
  --
  -- > x ← A·x
  -- > x ← A'·x
  -- > x ← conjg(A)·x
  --
  --   where /A/ is /n/ by /n/ unit, or non-unit, upper or lower
  --   triangular matrix.
  trmv :: RowOrder --
       -> Uplo     -- ^ Upper or low triangular
       -> Trans    -- ^ How should matrix be transformed
       -> Diag     -- ^ Whether matrix is unit diagonal or not
       -> Int      -- ^ Order of matrix /A/
       -> Ptr a    -- ^ Matrix /A/
       -> Int      -- ^ Leading dimension of /A/
       -> Ptr a    -- ^ Vector /x/ or /x'/. Before call it contains
                   --   original vector. After call it's overwritten
                   --   with transformed vector
       -> Int      -- ^ Stride for /x/ or /x'/
       -> IO ()

  -- | Perform one of the operations which are selected by 'Trans'
  --   parameter:
  --
  -- > x ← A·x
  -- > x ← A'·x
  -- > x ← conjg(A)·x
  --
  --   where /A/ is /n/ by /n/ unit, or non-unit, upper or lower
  --   triangular matrix in packed form.
  tpmv :: RowOrder --
       -> Uplo     -- ^ Upper or low triangular
       -> Trans    -- ^ How should matrix be transformed
       -> Diag     -- ^ Whether matrix is unit diagonal or not
       -> Int      -- ^ Order of matrix /A/
       -> Ptr a    -- ^ Matrix /A/
       -> Ptr a    -- ^ Vector /x/ or /x'/. Before call it contains
                   --   original vector. After call it's overwritten
                   --   with transformed vector
       -> Int      -- ^ Stride for /x/, /x'/
       -> IO ()

  -- | Perform one of the operations which are selected by 'Trans'
  --   parameter:
  --
  -- > x ← A·x
  -- > x ← A'·x
  -- > x ← conjg(A)·x
  --
  --   where /A/ is n by n unit, or non-unit, upper or lower
  --   triangular band matrix, with @k + 1@ diagonals.
  tbmv :: RowOrder --
       -> Uplo     -- ^ Upper or low triangular
       -> Trans    -- ^ How should matrix be transformed
       -> Diag     -- ^ Whether matrix is unit diagonal or not
       -> Int      -- ^ Matrix order
       -> Int      -- ^ Number of superdiagonals /k/
       -> Ptr a    -- ^ Matrix data
       -> Int      -- ^ /LDA/ Leading dimension of A (at least @k+1@)
       -> Ptr a    -- ^ Vector /x/
       -> Int      -- ^ Stride for /x/
       -> IO ()


  -- Equation solving

  -- | Solve one of the systems of equations
  --
  -- > A·x = b
  -- > A'·x = b
  -- > conjg(A')·x = b
  --
  --   where /b/ and /x/ are /n/ element vectors and A is an n by n
  --   unit, or non-unit, upper or lower triangular matrix.
  --
  --   No test for singularity or near-singularity is included in
  --   this routine. Such tests must be performed before calling
  --   this routine.
  trsv :: RowOrder --
       -> Uplo     -- ^ Upper or low triangular
       -> Trans    -- ^ How should matrix be transformed
       -> Diag     -- ^ Whether matrix is unit diagonal or not
       -> Int      -- ^ Order of matrix /A/
       -> Ptr a    -- ^ Matrix /A/
       -> Int      -- ^ Leading dimension of /A/
       -> Ptr a    -- ^ Before call contains right side of equation
                   --   /b/. Ater call it is ovrwritten with solution
                   --   /x/.
       -> Int      -- ^ Stride of /x/,/b/
       -> IO ()

  -- | Solve one of the systems of equations
  --
  -- > A·x = b
  -- > A'·x = b
  -- > conjg(A')·x = b
  --
  --   where /b/ and /x/ are /n/ element vectors and A is an n by n
  --   unit, or non-unit, upper or lower triangular matrix in packed
  --   form.
  --
  --   No test for singularity or near-singularity is included in
  --   this routine. Such tests must be performed before calling
  --   this routine.
  tpsv :: RowOrder --
       -> Uplo     -- ^ Upper or low triangular
       -> Trans    -- ^ How should matrix be transformed
       -> Diag     -- ^ Whether matrix is unit diagonal or not
       -> Int      -- ^ Order of matrix /A/
       -> Ptr a    -- ^ Matrix /A/
       -> Ptr a    -- ^ Vector /b/ and /x/. Befor call it contains
                   --   right side of equation after call it contains
                   --   solution.
       -> Int      -- ^ Stride for /b/ or /x/
       -> IO ()

  -- | Solve one of the systems of equations
  --
  -- > A·x = b
  -- > A'·x = b
  -- > conjg(A')·x = b
  --
  --   where /b/ and /x/ are /n/ element vectors and A is an n by n
  --   unit, or non-unit, upper or lower triangular band matrix, with
  --   @k+1@ diagonals.
  --
  --   No test for singularity or near-singularity is included in
  --   this routine. Such tests must be performed before calling
  --   this routine.
  tbsv :: RowOrder --
       -> Uplo     -- ^ Upper or low triangular
       -> Trans    -- ^ How should matrix be transformed
       -> Diag     -- ^ Whether matrix unit diagonal or not
       -> Int      -- ^ Order of matrix /A/
       -> Int      -- ^ Number of super diagonals
       -> Ptr a    -- ^ Matrix data
       -> Int      -- ^ Leading dimension of /A/ (at least @k+1@)
       -> Ptr a    -- ^ Before call contains right side of equation
                   --   /b/. Ater call it is ovrwritten with solution
                   --   /x/.
       -> Int      -- ^ Stride of /x/,/b/
       -> IO ()




----------------------------------------------------------------
-- Float
----------------------------------------------------------------

instance BLAS2 Float where
  gemv ord trans m n alpha pa lda px incx beta py incy =
    traceBLAS "gemv"
    {#call cblas_sgemv #} (toOrder ord) (toTrans trans)
      (toI m) (toI n) (toF alpha) (ptrF pa) (toI lda)
                 (ptrF px) (toI incx)
      (toF beta) (ptrF py) (toI incy)
  {-# INLINE gemv #-}

  hemv ord uplo n alpha pa lda px incx beta py incy =
    traceBLAS "hemv"
    {#call cblas_ssymv #} (toOrder ord) (toUplo uplo)
      (toI n) (toF alpha) (ptrF pa) (toI lda)
                 (ptrF px) (toI incx)
      (toF beta) (ptrF py) (toI incy)
  {-# INLINE hemv #-}

  hpmv ord uplo n alpha pap px incx beta py incy =
    traceBLAS "hpmv"
    {#call cblas_sspmv #} (toOrder ord) (toUplo uplo) (toI n)
      (toF alpha) (ptrF pap)
                  (ptrF px) (toI incx)
      (toF beta)  (ptrF py) (toI incy)
  {-# INLINE hpmv #-}

  gbmv ord trans m n kl ku alpha pa lda px incx beta py incy =
    traceBLAS "gbmv"
    {#call cblas_sgbmv #} (toOrder ord) (toTrans trans)
      (toI m) (toI n) (toI kl) (toI ku)
      (toF alpha) (ptrF pa) (toI lda )
                  (ptrF px) (toI incx)
      (toF beta)  (ptrF py) (toI incy)
  {-# INLINE gbmv #-}

  hbmv ord uplo n k alpha pa lda px incx beta py incy =
    traceBLAS "hbmv"
    {#call cblas_ssbmv #} (toOrder ord) (toUplo uplo) (toI n) (toI k)
      (toF alpha) (ptrF pa) (toI lda)
      (ptrF px) (toI incx)
      (toF beta) (ptrF py) (toI incy)
  {-# INLINE hbmv #-}

  gerc ord m n alpha px incx py incy pa lda =
    traceBLAS "gerc"
    {#call cblas_sger #} (toOrder ord)
      (toI m) (toI n) (toF alpha) (ptrF px) (toI incx) (ptrF py) (toI incy) (ptrF pa) (toI lda)
  {-# INLINE gerc #-}

  geru = gerc
  {-# INLINE geru #-}

  her ord uplo n alpha px incx pa lda =
    traceBLAS "her"
    {#call cblas_ssyr #} (toOrder ord)
      (toUplo uplo) (toI n) (toF alpha) (ptrF px) (toI incx) (ptrF pa) (toI lda)
  {-# INLINE her #-}

  her2 ord uplo n alpha px incx py incy pa lda =
    traceBLAS "her2"
    {#call cblas_ssyr2 #} (toOrder ord)
      (toUplo uplo) (toI n) (toF alpha) (ptrF px) (toI incx) (ptrF py) (toI incy) (ptrF pa) (toI lda)
  {-# INLINE her2 #-}

  hpr ord uplo n alpha px incx pap =
    traceBLAS "hpr"
    {#call cblas_sspr#} (toOrder ord)
     (toUplo uplo) (toI n) (toF alpha) (ptrF px) (toI incx) (ptrF pap)
  {-# INLINE hpr #-}

  hpr2 ord uplo n alpha px incx py incy pap =
    traceBLAS "hpr2"
    {#call cblas_sspr2 #} (toOrder ord)
      (toUplo uplo) (toI n) (toF alpha) (ptrF px) (toI incx) (ptrF py) (toI incy) (ptrF pap)
  {-# INLINE hpr2 #-}

  tpmv ord uplo trans diag n pap px incx =
    traceBLAS "tpmv"
    {#call cblas_stpmv #} (toOrder ord)
    (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptrF pap) (ptrF px) (toI incx)
  {-# INLINE tpmv #-}

  tpsv ord uplo trans diag n pap px incx =
    traceBLAS "tpsv"
    {#call cblas_stpsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptrF pap) (ptrF px) (toI incx)
  {-# INLINE tpsv #-}

  trmv ord uplo trans diag n pa lda px incx =
    traceBLAS "trmv"
    {#call cblas_strmv #} (toOrder ord) (toUplo uplo) (toTrans trans) (toDiag diag)
      (toI n) (ptrF pa) (toI lda) (ptrF px) (toI incx)
  {-# INLINE trmv #-}

  tbsv ord uplo trans diag n k pa lda px incx =
    traceBLAS "tbsv"
    {#call cblas_stbsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (toI k) (ptrF pa) (toI lda) (ptrF px) (toI incx)
  {-# INLINE tbsv #-}


  tbmv ord uplo trans diag n k pa lda px incx =
    traceBLAS "tbmv"
    {# call cblas_stbmv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (toI k) (ptrF pa) (toI lda) (ptrF px) (toI incx)
  {-# INLINE tbmv #-}

  trsv ord uplo trans diag n pa lda px incx =
    traceBLAS "trsv"
    {# call cblas_strsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptrF pa) (toI lda) (ptrF px) (toI incx)
  {-# INLINE trsv #-}



----------------------------------------------------------------
-- Double
----------------------------------------------------------------

instance BLAS2 Double where
  gemv ord trans m n alpha pa lda px incx beta py incy =
    traceBLAS "gemv"
    {#call cblas_dgemv #} (toOrder ord) (toTrans trans)
      (toI m) (toI n) (toD alpha) (ptrD pa) (toI lda)
                 (ptrD px) (toI incx)
      (toD beta) (ptrD py) (toI incy)
  {-# INLINE gemv #-}

  hemv ord uplo n alpha pa lda px incx beta py incy =
    traceBLAS "hemv"
    {#call cblas_dsymv #} (toOrder ord) (toUplo uplo)
      (toI n) (toD alpha) (ptrD pa) (toI lda)
                 (ptrD px) (toI incx)
      (toD beta) (ptrD py) (toI incy)
  {-# INLINE hemv #-}

  hpmv ord uplo n alpha pap px incx beta py incy =
    traceBLAS "hpmv"
    {#call cblas_dspmv #} (toOrder ord) (toUplo uplo) (toI n)
      (toD alpha) (ptrD pap)
                  (ptrD px) (toI incx)
      (toD beta)  (ptrD py) (toI incy)
  {-# INLINE hpmv #-}

  gbmv ord trans m n kl ku alpha pa lda px incx beta py incy =
    traceBLAS "gbmv"
    {#call cblas_dgbmv #} (toOrder ord) (toTrans trans)
      (toI m) (toI n) (toI kl) (toI ku)
      (toD alpha) (ptrD pa) (toI lda )
                  (ptrD px) (toI incx)
      (toD beta)  (ptrD py) (toI incy)
  {-# INLINE gbmv #-}

  hbmv ord uplo n k alpha pa lda px incx beta py incy =
    traceBLAS "hbmv"
    {#call cblas_dsbmv #} (toOrder ord) (toUplo uplo) (toI n) (toI k)
      (toD alpha) (ptrD pa) (toI lda)
      (ptrD px) (toI incx)
      (toD beta) (ptrD py) (toI incy)
  {-# INLINE hbmv #-}

  gerc ord m n alpha px incx py incy pa lda =
    traceBLAS "gerc"
    {#call cblas_dger #} (toOrder ord)
      (toI m) (toI n) (toD alpha) (ptrD px) (toI incx) (ptrD py) (toI incy) (ptrD pa) (toI lda)
  {-# INLINE gerc #-}

  geru = gerc
  {-# INLINE geru #-}

  her ord uplo n alpha px incx pa lda =
    traceBLAS "her"
    {#call cblas_dsyr #} (toOrder ord)
      (toUplo uplo) (toI n) (toD alpha) (ptrD px) (toI incx) (ptrD pa) (toI lda)
  {-# INLINE her #-}

  her2 ord uplo n alpha px incx py incy pa lda =
    traceBLAS "her2"
    {#call cblas_dsyr2 #} (toOrder ord)
      (toUplo uplo) (toI n) (toD alpha) (ptrD px) (toI incx) (ptrD py) (toI incy) (ptrD pa) (toI lda)
  {-# INLINE her2 #-}

  hpr ord uplo n alpha px incx pap =
    traceBLAS "hpr"
    {#call cblas_dspr#} (toOrder ord)
     (toUplo uplo) (toI n) (toD alpha) (ptrD px) (toI incx) (ptrD pap)
  {-# INLINE hpr #-}

  hpr2 ord uplo n alpha px incx py incy pap =
    traceBLAS "hpr2"
    {#call cblas_dspr2 #} (toOrder ord)
      (toUplo uplo) (toI n) (toD alpha) (ptrD px) (toI incx) (ptrD py) (toI incy) (ptrD pap)
  {-# INLINE hpr2 #-}

  tpmv ord uplo trans diag n pap px incx =
    traceBLAS "tpmv"
    {#call cblas_dtpmv #} (toOrder ord)
    (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptrD pap) (ptrD px) (toI incx)
  {-# INLINE tpmv #-}

  tpsv ord uplo trans diag n pap px incx =
    traceBLAS "tpsv"
    {#call cblas_dtpsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptrD pap) (ptrD px) (toI incx)
  {-# INLINE tpsv #-}

  trmv ord uplo trans diag n pa lda px incx =
    traceBLAS "trmv"
    {#call cblas_dtrmv #} (toOrder ord) (toUplo uplo) (toTrans trans) (toDiag diag)
      (toI n) (ptrD pa) (toI lda) (ptrD px) (toI incx)
  {-# INLINE trmv #-}

  tbsv ord uplo trans diag n k pa lda px incx =
    traceBLAS "tbsv"
    {#call cblas_dtbsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (toI k) (ptrD pa) (toI lda) (ptrD px) (toI incx)
  {-# INLINE tbsv #-}

  tbmv ord uplo trans diag n k pa lda px incx =
    traceBLAS "tbmv"
    {# call cblas_dtbmv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (toI k) (ptrD pa) (toI lda) (ptrD px) (toI incx)
  {-# INLINE tbmv #-}

  trsv ord uplo trans diag n pa lda px incx =
    traceBLAS "trsv"
    {# call cblas_dtrsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptrD pa) (toI lda) (ptrD px) (toI incx)
  {-# INLINE trsv #-}



----------------------------------------------------------------
-- Complex Float
----------------------------------------------------------------

instance BLAS2 (Complex Float) where
  gemv ord trans m n alpha pa lda px incx beta py incy =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "gemv"
      {#call cblas_cgemv #} (toOrder ord) (toTrans trans)
        (toI m) (toI n)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC px) (toI incx)
        (ptrC pbeta)  (ptrC py) (toI incy)
  {-# INLINE gemv #-}

  hemv ord uplo n alpha pa lda px incx beta py incy =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "hemv"
      {#call cblas_chemv #} (toOrder ord) (toUplo uplo)
        (toI n)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC px) (toI incx)
        (ptrC pbeta)  (ptrC py) (toI incy)
  {-# INLINE hemv #-}

  hpmv ord uplo n alpha pap px incx beta py incy =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "hpmv"
      {#call cblas_chpmv #} (toOrder ord) (toUplo uplo) (toI n)
        (ptrC palpha) (ptrC pap)
                      (ptrC px) (toI incx)
        (ptrC pbeta)  (ptrC py) (toI incy)
  {-# INLINE hpmv #-}

  gbmv ord trans m n kl ku alpha pa lda px incx beta py incy =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "gbmv"
      {#call cblas_cgbmv #} (toOrder ord) (toTrans trans)
        (toI m) (toI n) (toI kl) (toI ku)
        (ptrC palpha) (ptrC pa) (toI lda )
                      (ptrC px) (toI incx)
        (ptrC pbeta)  (ptrC py) (toI incy)
  {-# INLINE gbmv #-}

  hbmv ord uplo n k alpha pa lda px incx beta py incy =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "hbmv"
      {#call cblas_chbmv #} (toOrder ord) (toUplo uplo) (toI n) (toI k)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC px) (toI incx)
        (ptrC pbeta)  (ptrC py) (toI incy)
  {-# INLINE hbmv #-}

  gerc ord m n alpha px incx py incy pa lda =
    with alpha $ \palpha ->
      traceBLAS "gerc"
      {#call cblas_cgerc #} (toOrder ord)
        (toI m) (toI n) (ptrC palpha) (ptrC px) (toI incx) (ptrC py) (toI incy) (ptrC pa) (toI lda)
  {-# INLINE gerc #-}

  geru ord m n alpha px incx py incy pa lda =
    with alpha $ \palpha ->
      traceBLAS "geru"
      {#call cblas_cgeru #} (toOrder ord)
        (toI m) (toI n) (ptrC palpha) (ptrC px) (toI incx) (ptrC py) (toI incy) (ptrC pa) (toI lda)
  {-# INLINE geru #-}

  her ord uplo n alpha px incx pa lda =
    traceBLAS "her"
    {#call cblas_cher #} (toOrder ord)
      (toUplo uplo) (toI n) (toF alpha) (ptrC px) (toI incx) (ptrC pa) (toI lda)
  {-# INLINE her #-}

  her2 ord uplo n alpha px incx py incy pa lda =
    with alpha $ \palpha ->
      traceBLAS "her2"
      {#call cblas_cher2 #} (toOrder ord)
        (toUplo uplo) (toI n) (ptrC palpha) (ptrC px) (toI incx) (ptrC py) (toI incy) (ptrC pa) (toI lda)
  {-# INLINE her2 #-}

  hpr ord uplo n alpha px incx pap =
    traceBLAS "hpr"
    {#call cblas_chpr#} (toOrder ord)
      (toUplo uplo) (toI n) (toF alpha) (ptrC px) (toI incx) (ptrC pap)
  {-# INLINE hpr #-}

  hpr2 ord uplo n alpha px incx py incy pap =
    with alpha $ \palpha ->
      traceBLAS "hpr2"
      {#call cblas_chpr2 #} (toOrder ord)
        (toUplo uplo) (toI n) (ptrC palpha) (ptrC px) (toI incx) (ptrC py) (toI incy) (ptrC pap)
  {-# INLINE hpr2 #-}

  tpmv ord uplo trans diag n pap px incx =
    traceBLAS "tpmv"
    {#call cblas_ctpmv #} (toOrder ord)
    (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptrC pap) (ptrC px) (toI incx)
  {-# INLINE tpmv #-}

  tpsv ord uplo trans diag n pap px incx =
    traceBLAS "tpsv"
    {#call cblas_ctpsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptrC pap) (ptrC px) (toI incx)
  {-# INLINE tpsv #-}

  trmv ord uplo trans diag n pa lda px incx =
    traceBLAS "trmv"
    {#call cblas_ctrmv #} (toOrder ord) (toUplo uplo) (toTrans trans) (toDiag diag)
      (toI n) (ptrC pa) (toI lda) (ptrC px) (toI incx)
  {-# INLINE trmv #-}

  tbsv ord uplo trans diag n k pa lda px incx =
    traceBLAS "tbsv"
    {#call cblas_ctbsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (toI k) (ptrC pa) (toI lda) (ptrC px) (toI incx)
  {-# INLINE tbsv #-}

  tbmv ord uplo trans diag n k pa lda px incx =
    {# call cblas_ctbmv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (toI k) (ptrC pa) (toI lda) (ptrC px) (toI incx)
  {-# INLINE tbmv #-}

  trsv ord uplo trans diag n pa lda px incx =
    {# call cblas_ctrsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptrC pa) (toI lda) (ptrC px) (toI incx)
  {-# INLINE trsv #-}



----------------------------------------------------------------
-- Complex Double
----------------------------------------------------------------

instance BLAS2 (Complex Double) where
  gemv ord trans m n alpha pa lda px incx beta py incy =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "tbmv"
      {#call cblas_zgemv #} (toOrder ord) (toTrans trans)
        (toI m) (toI n)
        (ptrZ palpha) (ptrZ pa) (toI lda)
                      (ptrZ px) (toI incx)
        (ptrZ pbeta)  (ptrZ py) (toI incy)
  {-# INLINE gemv #-}

  hemv ord uplo n alpha pa lda px incx beta py incy =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "hemv"
      {#call cblas_zhemv #} (toOrder ord) (toUplo uplo)
        (toI n)
        (ptrZ palpha) (ptrZ pa) (toI lda)
                      (ptrZ px) (toI incx)
        (ptrZ pbeta)  (ptrZ py) (toI incy)
  {-# INLINE hemv #-}

  hpmv ord uplo n alpha pap px incx beta py incy =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "hpmv"
      {#call cblas_zhpmv #} (toOrder ord) (toUplo uplo) (toI n)
        (ptrZ palpha) (ptrZ pap)
                      (ptrZ px) (toI incx)
        (ptrZ pbeta)  (ptrZ py) (toI incy)
  {-# INLINE hpmv #-}

  gbmv ord trans m n kl ku alpha pa lda px incx beta py incy =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "gbmv"
      {#call cblas_zgbmv #} (toOrder ord) (toTrans trans)
        (toI m) (toI n) (toI kl) (toI ku)
        (ptrZ palpha) (ptrZ pa) (toI lda )
                      (ptrZ px) (toI incx)
        (ptrZ pbeta)  (ptrZ py) (toI incy)
  {-# INLINE gbmv #-}

  hbmv ord uplo n k alpha pa lda px incx beta py incy =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      traceBLAS "hbmv"
      {#call cblas_zhbmv #} (toOrder ord) (toUplo uplo) (toI n) (toI k)
        (ptrZ palpha) (ptrZ pa) (toI lda)
                      (ptrZ px) (toI incx)
        (ptrZ pbeta)  (ptrZ py) (toI incy)
  {-# INLINE hbmv #-}

  gerc ord m n alpha px incx py incy pa lda =
    with alpha $ \palpha ->
      traceBLAS "gerc"
      {#call cblas_zgerc #} (toOrder ord)
        (toI m) (toI n) (ptrZ palpha) (ptrZ px) (toI incx) (ptrZ py) (toI incy) (ptrZ pa) (toI lda)
  {-# INLINE gerc #-}

  geru ord m n alpha px incx py incy pa lda =
    with alpha $ \palpha ->
      traceBLAS "geru"
      {#call cblas_zgeru #} (toOrder ord)
        (toI m) (toI n) (ptrZ palpha) (ptrZ px) (toI incx) (ptrZ py) (toI incy) (ptrZ pa) (toI lda)
  {-# INLINE geru #-}

  her ord uplo n alpha px incx pa lda =
    traceBLAS "her"
    {#call cblas_zher #} (toOrder ord)
      (toUplo uplo) (toI n) (toD alpha) (ptrZ px) (toI incx) (ptrZ pa) (toI lda)
  {-# INLINE her #-}

  her2 ord uplo n alpha px incx py incy pa lda =
    with alpha $ \palpha ->
      traceBLAS "her2"
      {#call cblas_zher2 #} (toOrder ord)
        (toUplo uplo) (toI n) (ptrZ palpha) (ptrZ px) (toI incx) (ptrZ py) (toI incy) (ptrZ pa) (toI lda)
  {-# INLINE her2 #-}

  hpr ord uplo n alpha px incx pap =
    traceBLAS "hpr"
    {#call cblas_zhpr#} (toOrder ord)
      (toUplo uplo) (toI n) (toD alpha) (ptrZ px) (toI incx) (ptrZ pap)
  {-# INLINE hpr #-}

  hpr2 ord uplo n alpha px incx py incy pap =
    with alpha $ \palpha ->
      traceBLAS "hpr2"
      {#call cblas_zhpr2 #} (toOrder ord)
        (toUplo uplo) (toI n) (ptrZ palpha) (ptrZ px) (toI incx) (ptrZ py) (toI incy) (ptrZ pap)
  {-# INLINE hpr2 #-}

  tpmv ord uplo trans diag n pap px incx =
    traceBLAS "tpmv"
    {#call cblas_ztpmv #} (toOrder ord)
    (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptrZ pap) (ptrZ px) (toI incx)
  {-# INLINE tpmv #-}

  tpsv ord uplo trans diag n pap px incx =
    traceBLAS "tpsv"
    {#call cblas_ztpsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptrZ pap) (ptrZ px) (toI incx)
  {-# INLINE tpsv #-}

  trmv ord uplo trans diag n pa lda px incx =
    traceBLAS "trmv"
    {#call cblas_ztrmv #} (toOrder ord) (toUplo uplo) (toTrans trans) (toDiag diag)
      (toI n) (ptrZ pa) (toI lda) (ptrZ px) (toI incx)
  {-# INLINE trmv #-}

  tbsv ord uplo trans diag n k pa lda px incx =
    traceBLAS "tbsv"
    {#call cblas_ztbsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (toI k) (ptrZ pa) (toI lda) (ptrZ px) (toI incx)
  {-# INLINE tbsv #-}

  tbmv ord uplo trans diag n k pa lda px incx =
    traceBLAS "tbmv"
    {# call cblas_ztbmv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (toI k) (ptrZ pa) (toI lda) (ptrZ px) (toI incx)
  {-# INLINE tbmv #-}

  trsv ord uplo trans diag n pa lda px incx =
    traceBLAS "trsv"
    {# call cblas_ztrsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptrZ pa) (toI lda) (ptrZ px) (toI incx)
  {-# INLINE trsv #-}
