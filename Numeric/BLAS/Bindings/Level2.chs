{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances        #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.BLAS.Bindings.Level2
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
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
import Foreign.C.Types ( CInt(..), CDouble(..) )

import Numeric.BLAS.Bindings.Types
import Numeric.BLAS.Bindings.Level1



-- | Types with matrix-vector operations. This type class contain a
--   lot of operations. Here is summary:
--
--  > y ← α·A·x + β·y
--
--  [@Generic dense matrices@] 'gemv'
--
--  [@Hermitian/symmetric matrices@] 'hemv'
--
--  [@Packed hermitian/symmetric matrices@] 'hpmv'
--
--  [@Banded matrices@] 'gbmv'
--
--  [@Hermitian/symmetric banded matrices@] 'hbmv'
class (BLAS1 a) => BLAS2 a where
  -- OPERATION: aMV + bV

  -- | Compute matrix-vector multiplication with dense matrix:
  --
  --  > y ← α·A·x + β·y
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
  --   > y ← α·A·x + β·y
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

  -- | Perform rank-1 operation:
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

  -- | Perform rank-1 operation:
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
-- Double
----------------------------------------------------------------

instance BLAS2 Double where
  gemv ord trans m n alpha pa lda px incx beta py incy =
    {#call cblas_dgemv #} (toOrder ord) (toTrans trans)
      (toI m) (toI n) (toD alpha) (ptr pa) (toI lda)
                 (ptr px) (toI incx)
      (toD beta) (ptr py) (toI incy)
  {-# INLINE gemv #-}

  hemv ord uplo n alpha pa lda px incx beta py incy =
    {#call cblas_dsymv #} (toOrder ord) (toUplo uplo)
      (toI n) (toD alpha) (ptr pa) (toI lda)
                 (ptr px) (toI incx)
      (toD beta) (ptr py) (toI incy)
  {-# INLINE hemv #-}

  hpmv ord uplo n alpha pap px incx beta py incy =
    {#call cblas_dspmv #} (toOrder ord) (toUplo uplo) (toI n)
      (toD alpha) (ptr pap)
                  (ptr px) (toI incx)
      (toD beta)  (ptr py) (toI incy)
  {-# INLINE hpmv #-}

  gbmv ord trans m n kl ku alpha pa lda px incx beta py incy =
    {#call cblas_dgbmv #} (toOrder ord) (toTrans trans)
      (toI m) (toI n) (toI kl) (toI ku)
      (toD alpha) (ptr pa) (toI lda )
                  (ptr px) (toI incx)
      (toD beta)  (ptr py) (toI incy)
  {-# INLINE gbmv #-}

  hbmv ord uplo n k alpha pa lda px incx beta py incy =
    {#call cblas_dsbmv #} (toOrder ord) (toUplo uplo) (toI n) (toI k)
      (toD alpha) (ptr pa) (toI lda)
      (ptr px) (toI incx)
      (toD beta) (ptr py) (toI incy)
  {-# INLINE hbmv #-}

  gerc ord m n alpha px incx py incy pa lda =
    {#call cblas_dger #} (toOrder ord)
      (toI m) (toI n) (toD alpha) (ptr px) (toI incx) (ptr py) (toI incy) (ptr pa) (toI lda)
  {-# INLINE gerc #-}

  geru = gerc
  {-# INLINE geru #-}

  her ord uplo n alpha px incx pa lda =
    {#call cblas_dsyr #} (toOrder ord)
      (toUplo uplo) (toI n) (toD alpha) (ptr px) (toI incx) (ptr pa) (toI lda)
  {-# INLINE her #-}

  her2 ord uplo n alpha px incx py incy pa lda =
    {#call cblas_dsyr2 #} (toOrder ord)
      (toUplo uplo) (toI n) (toD alpha) (ptr px) (toI incx) (ptr py) (toI incy) (ptr pa) (toI lda)
  {-# INLINE her2 #-}

  hpr ord uplo n alpha px incx pap =
    {#call cblas_dspr#} (toOrder ord)
     (toUplo uplo) (toI n) (toD alpha) (ptr px) (toI incx) (ptr pap)
  {-# INLINE hpr #-}

  hpr2 ord uplo n alpha px incx py incy pap =
    {#call cblas_dspr2 #} (toOrder ord)
      (toUplo uplo) (toI n) (toD alpha) (ptr px) (toI incx) (ptr py) (toI incy) (ptr pap)
  {-# INLINE hpr2 #-}

  tpmv ord uplo trans diag n pap px incx =
    {#call cblas_dtpmv #} (toOrder ord)
    (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptr pap) (ptr px) (toI incx)
  {-# INLINE tpmv #-}

  tpsv ord uplo trans diag n pap px incx =
    {#call cblas_dtpsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptr pap) (ptr px) (toI incx)
  {-# INLINE tpsv #-}

  trmv ord uplo trans diag n pa lda px incx =
    {#call cblas_dtrmv #} (toOrder ord) (toUplo uplo) (toTrans trans) (toDiag diag)
      (toI n) (ptr pa) (toI lda) (ptr px) (toI incx)
  {-# INLINE trmv #-}

  tbsv ord uplo trans diag n k pa lda px incx =
    {#call cblas_dtbsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (toI k) (ptr pa) (toI lda) (ptr px) (toI incx)
  {-# INLINE tbsv #-}


  tbmv ord uplo trans diag n k pa lda px incx =
    {# call cblas_dtbmv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (toI k) (ptr pa) (toI lda) (ptr px) (toI incx)
  {-# INLINE tbmv #-}

  trsv ord uplo trans diag n pa lda px incx =
    {# call cblas_dtrsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptr pa) (toI lda) (ptr px) (toI incx)
  {-# INLINE trsv #-}



----------------------------------------------------------------
-- Complex Double
----------------------------------------------------------------

instance BLAS2 (Complex Double) where
  gemv ord trans m n alpha pa lda px incx beta py incy =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      {#call cblas_zgemv #} (toOrder ord) (toTrans trans)
        (toI m) (toI n)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC px) (toI incx)
        (ptrC pbeta)  (ptrC py) (toI incy)
  {-# INLINE gemv #-}

  hemv ord uplo n alpha pa lda px incx beta py incy =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
    {#call cblas_zhemv #} (toOrder ord) (toUplo uplo)
      (toI n)
      (ptrC palpha) (ptrC pa) (toI lda)
                    (ptrC px) (toI incx)
      (ptrC pbeta)  (ptrC py) (toI incy)
  {-# INLINE hemv #-}

  hpmv ord uplo n alpha pap px incx beta py incy =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
    {#call cblas_zhpmv #} (toOrder ord) (toUplo uplo) (toI n)
      (ptrC palpha) (ptrC pap)
                    (ptrC px) (toI incx)
      (ptrC pbeta)  (ptrC py) (toI incy)
  {-# INLINE hpmv #-}

  gbmv ord trans m n kl ku alpha pa lda px incx beta py incy =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      {#call cblas_zgbmv #} (toOrder ord) (toTrans trans)
        (toI m) (toI n) (toI kl) (toI ku)
        (ptrC palpha) (ptrC pa) (toI lda )
                      (ptrC px) (toI incx)
        (ptrC pbeta)  (ptrC py) (toI incy)
  {-# INLINE gbmv #-}

  hbmv ord uplo n k alpha pa lda px incx beta py incy =
    with alpha $ \palpha ->
    with beta  $ \pbeta  ->
      {#call cblas_zhbmv #} (toOrder ord) (toUplo uplo) (toI n) (toI k)
        (ptrC palpha) (ptrC pa) (toI lda)
                      (ptrC px) (toI incx)
        (ptrC pbeta)  (ptrC py) (toI incy)
  {-# INLINE hbmv #-}

  gerc ord m n alpha px incx py incy pa lda =
    with alpha $ \palpha ->
      {#call cblas_zgerc #} (toOrder ord)
        (toI m) (toI n) (ptrC palpha) (ptrC px) (toI incx) (ptrC py) (toI incy) (ptrC pa) (toI lda)
  {-# INLINE gerc #-}

  geru ord m n alpha px incx py incy pa lda =
    with alpha $ \palpha ->
      {#call cblas_zgeru #} (toOrder ord)
        (toI m) (toI n) (ptrC palpha) (ptrC px) (toI incx) (ptrC py) (toI incy) (ptrC pa) (toI lda)
  {-# INLINE geru #-}

  her ord uplo n alpha px incx pa lda =
    {#call cblas_zher #} (toOrder ord)
      (toUplo uplo) (toI n) (toD alpha) (ptrC px) (toI incx) (ptrC pa) (toI lda)
  {-# INLINE her #-}

  her2 ord uplo n alpha px incx py incy pa lda =
    with alpha $ \palpha ->
      {#call cblas_zher2 #} (toOrder ord)
        (toUplo uplo) (toI n) (ptrC palpha) (ptrC px) (toI incx) (ptrC py) (toI incy) (ptrC pa) (toI lda)
  {-# INLINE her2 #-}

  hpr ord uplo n alpha px incx pap =
    {#call cblas_zhpr#} (toOrder ord)
      (toUplo uplo) (toI n) (toD alpha) (ptrC px) (toI incx) (ptrC pap)
  {-# INLINE hpr #-}

  hpr2 ord uplo n alpha px incx py incy pap =
    with alpha $ \palpha ->
      {#call cblas_zhpr2 #} (toOrder ord)
        (toUplo uplo) (toI n) (ptrC palpha) (ptrC px) (toI incx) (ptrC py) (toI incy) (ptrC pap)
  {-# INLINE hpr2 #-}

  tpmv ord uplo trans diag n pap px incx =
    {#call cblas_ztpmv #} (toOrder ord)
    (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptrC pap) (ptrC px) (toI incx)
  {-# INLINE tpmv #-}

  tpsv ord uplo trans diag n pap px incx =
    {#call cblas_ztpsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptrC pap) (ptrC px) (toI incx)
  {-# INLINE tpsv #-}

  trmv ord uplo trans diag n pa lda px incx =
    {#call cblas_ztrmv #} (toOrder ord) (toUplo uplo) (toTrans trans) (toDiag diag)
      (toI n) (ptrC pa) (toI lda) (ptrC px) (toI incx)
  {-# INLINE trmv #-}

  tbsv ord uplo trans diag n k pa lda px incx =
    {#call cblas_ztbsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (toI k) (ptrC pa) (toI lda) (ptrC px) (toI incx)
  {-# INLINE tbsv #-}


  tbmv ord uplo trans diag n k pa lda px incx =
    {# call cblas_ztbmv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (toI k) (ptrC pa) (toI lda) (ptrC px) (toI incx)
  {-# INLINE tbmv #-}

  trsv ord uplo trans diag n pa lda px incx =
    {# call cblas_ztrsv #} (toOrder ord)
      (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) (ptrC pa) (toI lda) (ptrC px) (toI incx)
  {-# INLINE trsv #-}


--   gemv transa m n alpha pa lda px incx beta py incy =
--       withTrans transa $ \(toTrans trans)a ->
--       withCI m $ \(toI m) ->
--       withCI n $ \(toI n) ->
--       with alpha $ \palpha ->
--       withCI lda $ \plda ->
--       withCI incx $ \(toI incx) ->
--       with beta $ \pbeta ->
--       withCI incy $ \(toI incy) ->
--           zgemv (toTrans trans)a (toI m) (toI n) palpha pa plda px (toI incx) pbeta py (toI incy)
--   {-# INLINE gemv #-}

--   gbmv transa m n kl ku alpha pa lda px incx beta py incy =
--       withTrans transa $ \(toTrans trans)a ->
--       withCI m $ \(toI m) ->
--       withCI n $ \(toI n) ->
--       withCI kl $ \pkl ->
--       withCI ku $ \pku ->
--       with alpha $ \palpha ->
--       withCI lda $ \plda ->
--       withCI incx $ \(toI incx) ->
--       with beta $ \pbeta ->
--       withCI incy $ \(toI incy) ->
--           zgbmv (toTrans trans)a (toI m) (toI n) pkl pku palpha pa plda px (toI incx) pbeta py (toI incy)
--   {-# INLINE gbmv #-}

--   trmv uplo trans diag n pa lda px incx =
--       withUplo uplo $ \(toUplo uplo) ->
--       withTrans trans $ \(toTrans trans) ->
--       withDiag diag $ \(toDiag diag) ->
--       withCI n $ \(toI n) ->
--       withCI lda $ \plda ->
--       withCI incx $ \(toI incx) ->
--           ztrmv (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) pa plda px (toI incx)
--   {-# INLINE trmv #-}

--   tpmv uplo trans diag n pap px incx =
--       withUplo uplo $ \(toUplo uplo) ->
--       withTrans trans $ \(toTrans trans) ->
--       withDiag diag $ \(toDiag diag) ->
--       withCI n $ \(toI n) ->
--       withCI incx $ \(toI incx) ->
--           ztpmv (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) pap px (toI incx)
--   {-# INLINE tpmv #-}

--   tpsv uplo trans diag n pap px incx =
--       withUplo uplo $ \(toUplo uplo) ->
--       withTrans trans $ \(toTrans trans) ->
--       withDiag diag $ \(toDiag diag) ->
--       withCI n $ \(toI n) ->
--       withCI incx $ \(toI incx) ->
--           ztpsv (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) pap px (toI incx)
--   {-# INLINE tpsv #-}

--   tbmv uplo trans diag n k pa lda px incx =
--       withUplo uplo $ \(toUplo uplo) ->
--       withTrans trans $ \(toTrans trans) ->
--       withDiag diag $ \(toDiag diag) ->
--       withCI n $ \(toI n) ->
--       withCI k $ \pk ->
--       withCI lda $ \plda ->
--       withCI incx $ \(toI incx) ->
--           ztbmv (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) pk pa plda px (toI incx)
--   {-# INLINE tbmv #-}

--   trsv uplo trans diag n pa lda px incx =
--       withUplo uplo $ \(toUplo uplo) ->
--       withTrans trans $ \(toTrans trans) ->
--       withDiag diag $ \(toDiag diag) ->
--       withCI n $ \(toI n) ->
--       withCI lda $ \plda ->
--       withCI incx $ \(toI incx) ->
--           ztrsv (toUplo uplo) (toTrans trans) (toDiag diag) (toI n) pa plda px (toI incx)
--   {-# INLINE trsv #-}

--   tbsv uplo trans diag n k pa lda px incx =
--       withUplo uplo $ \(toUplo uplo) ->
--       withTrans trans $ \(toTrans trans) ->
--       withDiag diag $ \(toDiag diag) ->
--       withCI n $ \(toI n) ->
--       withCI k $ \pk ->
--       withCI lda $ \plda ->
--       withCI incx $ \(toI incx) ->
--           ztbsv (toUplo uplo) (toTrans trans) (toDiag diag)  (toI n) pk pa plda px (toI incx)
--   {-# INLINE tbsv #-}

--   hemv uplo n alpha pa lda px incx beta py incy =
--       withUplo uplo $ \(toUplo uplo) ->
--       withCI n $ \(toI n) ->
--       with alpha $ \palpha ->
--       withCI lda $ \plda ->
--       withCI incx $ \(toI incx) ->
--       with beta $ \pbeta ->
--       withCI incy $ \(toI incy) ->
--           zhemv (toUplo uplo) (toI n) palpha pa plda px (toI incx) pbeta py (toI incy)
--   {-# INLINE hemv #-}

--   hbmv uplo n k alpha pa lda px incx beta py incy =
--       withUplo uplo $ \(toUplo uplo) ->
--       withCI n $ \(toI n) ->
--       withCI k $ \pk ->
--       with alpha $ \palpha ->
--       withCI lda $ \plda ->
--       withCI incx $ \(toI incx) ->
--       with beta $ \pbeta ->
--       withCI incy $ \(toI incy) ->
--           zhbmv (toUplo uplo) (toI n) pk palpha pa plda px (toI incx) pbeta py (toI incy)
--   {-# INLINE hbmv #-}

--   gerc m n alpha px incx py incy pa lda =
--       withCI m $ \pm ->
--       withCI n $ \(toI n) ->
--       with alpha $ \palpha ->
--       withCI incx $ \(toI incx) ->
--       withCI incy $ \(toI incy) ->
--       withCI lda $ \plda ->
--           zgerc (toI m) (toI n) palpha px (toI incx) py (toI incy) pa plda
--   {-# INLINE gerc #-}

--   geru m n alpha px incx py incy pa lda =
--       withCI m $ \(toI m) ->
--       withCI n $ \(toI n) ->
--       with alpha $ \palpha ->
--       withCI incx $ \(toI incx) ->
--       withCI incy $ \(toI incy) ->
--       withCI lda $ \plda ->
--           zgeru (toI m) (toI n) palpha px (toI incx) py (toI incy) pa plda
--   {-# INLINE geru #-}

--   her uplo n alpha px incx pa lda =
--       withUplo uplo $ \(toUplo uplo) ->
--       withCI n $ \(toI n) ->
--       with alpha $ \palpha ->
--       withCI incx $ \(toI incx) ->
--       withCI lda $ \plda ->
--           zher (toUplo uplo) (toI n) palpha px (toI incx) pa plda
--   {-# INLINE her #-}

--   her2 uplo n alpha px incx py incy pa lda =
--       withUplo uplo $ \(toUplo uplo) ->
--       withCI n $ \(toI n) ->
--       with alpha $ \palpha ->
--       withCI incx $ \(toI incx) ->
--       withCI incy $ \(toI incy) ->
--       withCI lda $ \plda ->
--           zher2 (toUplo uplo) (toI n) palpha px (toI incx) py (toI incy) pa plda
--   {-# INLINE her2 #-}

--   hpmv uplo n alpha pap px incx beta py incy =
--       withUplo uplo $ \(toUplo uplo) ->
--       withCI n $ \(toI n) ->
--       with alpha $ \palpha ->
--       withCI incx $ \(toI incx) ->
--       with beta $ \pbeta ->
--       withCI incy $ \(toI incy) ->
--           zhpmv (toUplo uplo) (toI n) palpha pap px (toI incx) pbeta py (toI incy)
--   {-# INLINE hpmv #-}

--   hpr uplo n alpha px incx pap =
--       withUplo uplo $ \(toUplo uplo) ->
--       withCI n $ \(toI n) ->
--       with alpha $ \palpha ->
--       withCI incx $ \(toI incx) ->
--           zhpr (toUplo uplo) (toI n) palpha px (toI incx) pap
--   {-# INLINE hpr #-}

--   hpr2 uplo n alpha px incx py incy pap =
--       withUplo uplo $ \(toUplo uplo) ->
--       withCI n $ \(toI n) ->
--       with alpha $ \palpha ->
--       withCI incx $ \(toI incx) ->
--       withCI incy $ \(toI incy) ->
--           zhpr2 (toUplo uplo) (toI n) palpha px (toI incx) py (toI incy) pap
--   {-# INLINE hpr2 #-}
