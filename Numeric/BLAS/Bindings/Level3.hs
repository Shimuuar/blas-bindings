{-# LANGUAGE FlexibleInstances #-}
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

module Numeric.BLAS.Bindings.Level3 (
    BLAS3(..),
    ) where

import Data.Complex
import Foreign         ( Ptr, with )

import Numeric.BLAS.Bindings.Types
import Numeric.BLAS.Bindings.Level2
import Numeric.BLAS.Bindings.Double
import Numeric.BLAS.Bindings.Zomplex

-- | Types with matrix-matrix operations.
class (BLAS2 a) => BLAS3 a where
    -- | Performs one of the matrix-matrix operations @C :=
    --   alpha*op(A)*op(B) + beta*C@ where @op@ is noop, transposition
    --   or conjugation.
    gemm  :: Trans -- ^ Operation for matrix /A/
          -> Trans -- ^ Operation for matrix /B/
          -> Int   -- ^ Number of row of matrix op(/A/) and matrix /C/
          -> Int   -- ^ Number of columns of matrix @op(/B/)@ and matrix /C/
          -> Int   -- ^ Number of columns of matrix @op(/A/)@ and rows of matrix @op(/B/)@
          -> a     -- ^ Scalar /alpha/
          -> Ptr a -- ^ Matrix /A/
          -> Int   -- ^ Leading dimension of /A/
          -> Ptr a -- ^ Matrix /B/
          -> Int   -- ^ Leading dimension of /B/
          -> a     -- ^ Scalar /beta/
          -> Ptr a -- ^ Matrix /C/
          -> Int   -- ^ Leading dimension of /C/
          -> IO ()

    -- | Performs one of operations @C := alpha*A*B + beta*C@ or @C :=
    --   alpha*B*A + beta*C@ where A is a symmetric matrix and B and C
    --   are m by n matrices.
    symm  :: Side  -- ^ Whether symmetric matrix appears on the left or on the right
          -> Uplo  -- ^ Upper or lower triangular part of matix should be used
          -> Int   -- ^ Number of row of matrix /B/ and /C/
          -> Int   -- ^ Number of columns of matrix /B/ and /C/
          -> a     -- ^ Scalar /alpha/
          -> Ptr a -- ^ Matrix /A/
          -> Int   -- ^ Leading dimension of /A/
          -> Ptr a -- ^ Matrix /B/
          -> Int   -- ^ Leading dimension of /B/
          -> a     -- ^ Scalar /beta/
          -> Ptr a -- ^ Matrix /C/
          -> Int   -- ^ Leading dimension of /C/
          -> IO ()

    -- | Performs one of operations @C := alpha*A*B + beta*C@ or @C :=
    --   alpha*B*A + beta*C@ where A is a hermitian matrix and B and C
    --   are m by n matrices.
    hemm  :: Side  -- ^ Whether symmetric matrix appears on the left or on the right
          -> Uplo  -- ^ Upper or lower triangular part of matix should be used
          -> Int   -- ^ Number of row of matrix /B/ and /C/
          -> Int   -- ^ Number of columns of matrix /B/ and /C/
          -> a     -- ^ Scalar /alpha/
          -> Ptr a -- ^ Matrix /A/
          -> Int   -- ^ Leading dimension of /A/
          -> Ptr a -- ^ Matrix /B/
          -> Int   -- ^ Leading dimension of /B/
          -> a     -- ^ Scalar /beta/
          -> Ptr a -- ^ Matrix /C/
          -> Int   -- ^ Leading dimension of /C/
          -> IO ()

    -- | Performs one of the matrix-matrix operations B := alpha*op( A
    -- )*B, or B := alpha*B*op( A ) where alpha is a scalar, B is an m
    -- by n matrix, A is a unit, or non-unit, upper or lower
    -- triangular matrix and op( A ) is one of op( A ) = A or op( A )
    -- = A' or op( A ) = conjg( A' )
    trmm  :: Side  -- ^ Whether symmetric matrix appears on the left or on the right
          -> Uplo  -- ^ Upper or lower triangular part of matix should be used
          -> Trans -- ^ Transformation for matrix /A/
          -> Diag  -- ^ Whether matrix is unit diagonal or not.
          -> Int   -- ^ Number of rows of matrix /B/
          -> Int   -- ^ Number of columns of matrix /B/
          -> a     -- ^ Scalar /alpha/
          -> Ptr a -- ^ Matrix /A/
          -> Int   -- ^ Leading dimension of /A/
          -> Ptr a -- ^ Matrix /B/
          -> Int   -- ^ Leading dimension of /B/
          -> IO ()

    -- | Solves one of the matrix equations op( A )*X = alpha*B, or
    --   X*op( A ) = alpha*B where alpha is scalar, X and B are n by m
    --   matrices A is a unit, or non-unit, upper or lower triangular
    --   matrix and op is noop, transpose or Hermit conjugation
    trsm  :: Side  -- ^ Whether A appears on the left or right side of
                   --   X. 'L' : op(A)X = alpha*B, 'R' : Xop(A) = alpha*B
          -> Uplo  -- ^ Upper or lower triangular part of matix should
                   --   be used
          -> Trans -- ^ Transformation applied to A
          -> Diag  -- ^ Whether A is unit triangular or not
          -> Int   -- ^ Number of rows of B
          -> Int   -- ^ Number of columns of B
          -> a     -- ^ Scalar alpha
          -> Ptr a -- ^ Matrix A data
          -> Int   -- ^ Leading dimension of A
          -> Ptr a -- ^ Matrix B. Will be overwritten by solution
          -> Int   -- ^ Leading dimension of B
          -> IO ()

    -- | performs one of the symmetric rank k operations C :=
    --   alpha*A*A' + beta*C, or C := alpha*A'*A + beta*C.
    syrk  :: Uplo  -- ^ Whether upper or lower triangular array of C is referneced.
          -> Trans -- ^ Choses operation to perform:
                   --   TRANS = 'N' or 'n' C := alpha*A*A' + beta*C
                   --   TRANS = 'T' or 't' C := alpha*A'*A + beta*C.
          -> Int   -- ^ Order of matrix C
          -> Int   -- ^ If Trans is N number of columns of matrix A,
                   --   if Trans is T number of rows in matrix A.
          -> a     -- ^ Scalar alpha
          -> Ptr a -- ^ Matrix A
          -> Int   -- ^ Leading dimension of A
          -> a     -- ^ Scalar beta
          -> Ptr a -- ^ Matrix C
          -> Int   -- ^ Leading dimension of C
          -> IO ()

    -- | performs one of the symmetric rank 2k operations C :=
    -- alpha*A*B' + alpha*B*A' + beta*C or C := alpha*A'*B +
    -- alpha*B'*A + beta*C,
    syr2k :: Uplo  -- ^ Whether upper or lower triangular part of C is
                   --   referenced
          -> Trans -- ^ On entry, TRANS specifies the operation to be performed as follows:
                   -- TRANS = 'N' or 'n' C := alpha*A*B' + alpha*B*A' + beta*C.
                   -- TRANS = 'T' or 't' C := alpha*A'*B + alpha*B'*A + beta*C.
          -> Int   -- ^ Order of matrix C
          -> Int   -- ^ If Trans is 'N' number of columns of matrices
                   --   A and B it's number of rows otherwise
          -> a     -- ^ Scalar alpha
          -> Ptr a -- ^ Matrix A
          -> Int   -- ^ Leading dimension of A
          -> Ptr a -- ^ Matrix B
          -> Int   -- ^ Leading dimension of B
          -> a     -- ^ Scalar beta
          -> Ptr a -- ^ Matrix C
          -> Int   -- ^ Leading dimension of C
          -> IO ()

    -- | Performs one of the hermitian rank k operations C :=
    --   alpha*A*conjg( A' ) + beta*C, or C := alpha*conjg( A' )*A +
    --   beta*C, where alpha and beta are real scalars, C is an n by n
    --   hermitian matrix and A is an n by k matrix in the first case
    --   and a k by n matrix in the second case.
    herk  :: Uplo  -- ^ On entry, UPLO specifies whether the upper or
                   --   lower triangular part of the array C is to be
                   --   referenced as follows: UPLO = 'U' or 'u' Only
                   --   the upper triangular part of C is to be
                   --   referenced. UPLO = 'L' or 'l' Only the lower
                   --   triangular part of C is to be referenced.
          -> Trans -- ^ On entry, TRANS specifies the operation to be
                   --   performed as follows: TRANS = 'N' or 'n' C :=
                   --   alpha*A*conjg( A' ) + beta*C. TRANS = 'C' or
                   --   'c' C := alpha*conjg( A' )*A + beta*C.
          -> Int   -- ^ Order of matrix C
          -> Int   -- ^ If Trans is N number of columns of matrix A,
                   --   if Trans is C number of rows in matrix A.
          -> a     -- ^ Scalar alpha
          -> Ptr a -- ^ Matrix A
          -> Int   -- ^ Leading dimension of A
          -> a     -- ^ Scalar beta
          -> Ptr a -- ^ Matrix C
          -> Int   -- ^ Leading dimension of C
          -> IO ()

    -- | performs one of the hermitian rank 2k operations C :=
    --   alpha*A*conjg( B' ) + conjg( alpha )*B*conjg( A' ) + beta*C,
    --   or C := alpha*conjg( A' )*B + conjg( alpha )*conjg( B' )*A +
    --   beta*C, where alpha and beta are scalars with beta real, C is
    --   an n by n hermitian matrix and A and B are n by k matrices in
    --   the first case and k by n matrices in the second case.
    her2k :: Uplo  -- ^ Whether upper or lower triangular part of C is
                   --   referenced
          -> Trans -- ^ On entry, TRANS specifies the operation to be performed as follows:
                   -- TRANS = 'N' or 'n' C := alpha*A*conjg(B) + alpha*B*conjg(A) + beta*C.
                   -- TRANS = 'C' or 'c' C := alpha*conjg(A)*B + alpha*conjg(B)*A + beta*C.
          -> Int   -- ^ Order of matrix C
          -> Int   -- ^ If Trans is 'N' number of columns of matrices
                   --   A and B it's number of rows otherwise
          -> a     -- ^ Scalar alpha
          -> Ptr a -- ^ Matrix A
          -> Int   -- ^ Leading dimension of A
          -> Ptr a -- ^ Matrix B
          -> Int   -- ^ Leading dimension of B
          -> a     -- ^ Scalar beta
          -> Ptr a -- ^ Matrix C
          -> Int   -- ^ Leading dimension of C
          -> IO ()


instance BLAS3 Double where
    gemm transa transb m n k alpha pa lda pb ldb beta pc ldc =
        withTrans transa $ \ptransa ->
        withTrans transb $ \ptransb ->
        withCI m $ \pm ->
        withCI n $ \pn ->
        withCI k $ \pk ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI ldb $ \pldb ->
        with beta $ \pbeta ->
        withCI ldc $ \pldc ->
            dgemm ptransa ptransb pm pn pk palpha pa plda pb pldb pbeta pc pldc
    {-# INLINE gemm #-}

    symm side uplo m n alpha pa lda pb ldb beta pc ldc =
        withSide side $ \pside ->
        withUplo uplo $ \puplo ->
        withCI m $ \pm ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI ldb $ \pldb ->
        with beta $ \pbeta ->
        withCI ldc $ \pldc ->
            dsymm pside puplo pm pn palpha pa plda pb pldb pbeta pc pldc
    {-# INLINE symm #-}

    hemm = symm
    {-# INLINE hemm #-}

    trmm side uplo transa diag m n alpha pa lda pb ldb =
        withSide side $ \pside ->
        withUplo uplo $ \puplo ->
        withTrans transa $ \ptransa ->
        withDiag diag $ \pdiag ->
        withCI m $ \pm ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI ldb $ \pldb ->
            dtrmm pside puplo ptransa pdiag pm pn palpha pa plda pb pldb
    {-# INLINE trmm #-}

    trsm side uplo transa diag m n alpha pa lda pb ldb =
        withSide side $ \pside ->
        withUplo uplo $ \puplo ->
        withTrans transa $ \ptransa ->
        withDiag diag $ \pdiag ->
        withCI m $ \pm ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI ldb $ \pldb ->
            dtrsm pside puplo ptransa pdiag pm pn palpha pa plda pb pldb
    {-# INLINE trsm #-}

    syrk uplo transa n k alpha pa lda beta pc ldc =
        withUplo uplo $ \puplo ->
        withTrans transa $ \ptransa ->
        withCI n $ \pn ->
        withCI k $ \pk ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        with beta $ \pbeta ->
        withCI ldc $ \pldc ->
            dsyrk puplo ptransa pn pk palpha pa plda pbeta pc pldc
    {-# INLINE syrk #-}

    syr2k uplo transa n k alpha pa lda pb ldb beta pc ldc =
        withUplo uplo $ \puplo ->
        withTrans transa $ \ptransa ->
        withCI n $ \pn ->
        withCI k $ \pk ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI ldb $ \pldb ->
        with beta $ \pbeta ->
        withCI ldc $ \pldc ->
            dsyr2k puplo ptransa pn pk palpha pa plda pb pldb pbeta pc pldc
    {-# INLINE syr2k #-}

    herk = syrk
    {-# INLINE herk #-}

    her2k = syr2k
    {-# INLINE her2k #-}


instance BLAS3 (Complex Double) where
    gemm transa transb m n k alpha pa lda pb ldb beta pc ldc =
        withTrans transa $ \ptransa ->
        withTrans transb $ \ptransb ->
        withCI m $ \pm ->
        withCI n $ \pn ->
        withCI k $ \pk ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI ldb $ \pldb ->
        with beta $ \pbeta ->
        withCI ldc $ \pldc ->
            zgemm ptransa ptransb pm pn pk palpha pa plda pb pldb pbeta pc pldc
    {-# INLINE gemm #-}

    symm side uplo m n alpha pa lda pb ldb beta pc ldc =
        withSide side $ \pside ->
        withUplo uplo $ \puplo ->
        withCI m $ \pm ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI ldb $ \pldb ->
        with beta $ \pbeta ->
        withCI ldc $ \pldc ->
            zsymm pside puplo pm pn palpha pa plda pb pldb pbeta pc pldc
    {-# INLINE symm #-}

    hemm side uplo m n alpha pa lda pb ldb beta pc ldc =
        withSide side $ \pside ->
        withUplo uplo $ \puplo ->
        withCI m $ \pm ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI ldb $ \pldb ->
        with beta $ \pbeta ->
        withCI ldc $ \pldc ->
            zhemm pside puplo pm pn palpha pa plda pb pldb pbeta pc pldc
    {-# INLINE hemm #-}

    trmm side uplo transa diag m n alpha pa lda pb ldb =
        withSide side $ \pside ->
        withUplo uplo $ \puplo ->
        withTrans transa $ \ptransa ->
        withDiag diag $ \pdiag ->
        withCI m $ \pm ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI ldb $ \pldb ->
            ztrmm pside puplo ptransa pdiag pm pn palpha pa plda pb pldb
    {-# INLINE trmm #-}

    trsm side uplo transa diag m n alpha pa lda pb ldb =
        withSide side $ \pside ->
        withUplo uplo $ \puplo ->
        withTrans transa $ \ptransa ->
        withDiag diag $ \pdiag ->
        withCI m $ \pm ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI ldb $ \pldb ->
            ztrsm pside puplo ptransa pdiag pm pn palpha pa plda pb pldb
    {-# INLINE trsm #-}

    syrk uplo transa n k alpha pa lda beta pc ldc =
        withUplo uplo $ \puplo ->
        withTrans transa $ \ptransa ->
        withCI n $ \pn ->
        withCI k $ \pk ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        with beta $ \pbeta ->
        withCI ldc $ \pldc ->
            zsyrk puplo ptransa pn pk palpha pa plda pbeta pc pldc
    {-# INLINE syrk #-}

    syr2k uplo transa n k alpha pa lda pb ldb beta pc ldc =
        withUplo uplo $ \puplo ->
        withTrans transa $ \ptransa ->
        withCI n $ \pn ->
        withCI k $ \pk ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI ldb $ \pldb ->
        with beta $ \pbeta ->
        withCI ldc $ \pldc ->
            zsyr2k puplo ptransa pn pk palpha pa plda pb pldb pbeta pc pldc
    {-# INLINE syr2k #-}

    herk uplo transa n k alpha pa lda beta pc ldc =
        withUplo uplo $ \puplo ->
        withTrans transa $ \ptransa ->
        withCI n $ \pn ->
        withCI k $ \pk ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        with beta $ \pbeta ->
        withCI ldc $ \pldc ->
            zherk puplo ptransa pn pk palpha pa plda pbeta pc pldc
    {-# INLINE herk #-}

    her2k uplo transa n k alpha pa lda pb ldb beta pc ldc =
        withUplo uplo $ \puplo ->
        withTrans transa $ \ptransa ->
        withCI n $ \pn ->
        withCI k $ \pk ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI ldb $ \pldb ->
        with beta $ \pbeta ->
        withCI ldc $ \pldc ->
            zher2k puplo ptransa pn pk palpha pa plda pb pldb pbeta pc pldc
    {-# INLINE her2k #-}
