{-# LANGUAGE FlexibleInstances #-}
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

module Numeric.BLAS.Bindings.Level2 (
    BLAS2(..),
    ) where

import Data.Complex
import Foreign         ( Ptr, with )

import Numeric.BLAS.Bindings.Types
import Numeric.BLAS.Bindings.Level1
import Numeric.BLAS.Bindings.Double
import Numeric.BLAS.Bindings.Zomplex

-- | Types with matrix-vector operations.
class (BLAS1 a) => BLAS2 a where
    -- | Compute matrix-vector multiplication with banded matrix.
    --   @y <- alpha*A*x + beta*y@ Matrix /A/ is transformed according to
    --   'Trans' parameter.
    gbmv :: Trans -- ^ Matrix transformation
         -> Int   -- ^ /M/  number of row
         -> Int   -- ^ /N/  number of columns
         -> Int   -- ^ /KL/ number of sub-diagonals @KL >= 0@
         -> Int   -- ^ /KU/ number of super-diagonals @KU >= 0@
         -> a     -- ^ Scalar /alpha/
         -> Ptr a -- ^ Matrix data
         -> Int   -- ^ /LDA/ first dimension of matrix @LDA >= KL + KU + 1@
         -> Ptr a -- ^ vector /x/
         -> Int   -- ^ Stride of /x/
         -> a     -- ^ Scalar /beta/
         -> Ptr a -- ^ Vector /y/
         -> Int   -- ^ Stride of /y/
         -> IO ()

    -- | Compute matrix-vector multiplication with dense matrix.
    --   @y <- alpha*A*x + beta*y@. Matrix /A/ is transformed according to
    -- 'Trans' parameter.
    gemv :: Trans -- ^ Matrix transformation
         -> Int   -- ^ Number of rows
         -> Int   -- ^ Number of columns
         -> a     -- ^ Scalar /alpha/
         -> Ptr a -- ^ Pointer to matrix /A/
         -> Int   -- ^ Column size of a matrix
         -> Ptr a -- ^ Vector /x/
         -> Int   -- ^ Stride of /x/
         -> a     -- ^ Scalar /beta/
         -> Ptr a -- ^ Vector /y/
         -> Int   -- ^ Stride of /y/
         -> IO ()

    -- | Perform rank-1 operation @A <- alpha*x*conjg(y') + A@
    gerc :: Int   -- ^ Number of rows
         -> Int   -- ^ Number of columns
         -> a     -- ^ Scalar /alpha/
         -> Ptr a -- ^ Vector /x/
         -> Int   -- ^ Stride of /x/
         -> Ptr a -- ^ Vector /y/
         -> Int   -- ^ Stride of /y/
         -> Ptr a -- ^ Matrix /A/
         -> Int   -- ^ First dimension of /A/
         -> IO ()

    -- | Perform rank-1 operation @A <- alpha*x*y' + A@
    geru :: Int   -- ^ Number of rows
         -> Int   -- ^ Number of columns
         -> a     -- ^ Scalar /alpha/
         -> Ptr a -- ^ Vector /x/
         -> Int   -- ^ Stride of /x/
         -> Ptr a -- ^ Vector /y/
         -> Int   -- ^ Stride of /y/
         -> Ptr a -- ^ Matrix /A/
         -> Int   -- ^ First dimension of /A/
         -> IO ()

    -- | Perform operation @y <- alpha * A * x + beta * y@. /A/ is
    --   hermitian or symmetric banded matrix
    hbmv :: Uplo  -- ^ Hermitian/symmetric matrix storage mode
         -> Int   -- ^ /N/ Size of matrix
         -> Int   -- ^ /K/ Number of super-diagonals
         -> a     -- ^ Scalar /alpha/
         -> Ptr a -- ^ Matrix data
         -> Int   -- ^ First dimension of /A/
         -> Ptr a -- ^ Vector /x/
         -> Int   -- ^ Stride for /x/
         -> a     -- ^ Scalar /beta/
         -> Ptr a -- ^ Vector /y/
         -> Int   -- ^ Stride for /y/
         -> IO ()

    -- | Perform matrix-vector operation @y <- alpha*A*x + beta*y@
    hemv :: Uplo  -- ^ Hermitian/symmetric matrix storage mode
         -> Int   -- ^ Matrix size
         -> a     -- ^ Scalar /alpha/
         -> Ptr a -- ^ Matrix data
         -> Int   -- ^ First dimension of /A/
         -> Ptr a -- ^ Vector /x/
         -> Int   -- ^ Stride for /x/
         -> a     -- ^ Scalar /beta/
         -> Ptr a -- ^ Vector /y/
         -> Int   -- ^ Stride for /y/
         -> Int   -- ^ Stride for /y/
         -> IO ()

    -- | Perform operation @A <- alpha * x * conjg(x') + A@. /A/ is
    --   hermitian or symmetric matrix
    her  :: Uplo   -- ^ Hermitian/symmetric matrix storage mode
         -> Int    -- ^ Size of matrix
         -> Double -- ^ Scalar /alpha/
         -> Ptr a  -- ^ Vector /x/
         -> Int    -- ^ Stride of /x/
         -> Ptr a  -- ^ Matrix data
         -> Int    -- ^ First dimension of /A/
         -> IO ()

    -- | Perform operation @A := alpha*x*conjg( y' ) + conjg( alpha )*y*conjg( x' ) + A@
    --   /A/ is hermitian or symmetric matrix.
    her2 :: Uplo  -- ^ Hermitian/symmetric matrix storage mode
         -> Int   -- ^ Size of matrix
         -> a     -- ^ Scalar /alpha/
         -> Ptr a -- ^ Vector /x/
         -> Int   -- ^ Stride for /x/
         -> Ptr a -- ^ Vector /y/
         -> Int   -- ^ Stride for /y/
         -> Ptr a -- ^ Matrix data
         -> Int   -- ^ First dimenstion of matrix
         -> IO ()
    -- | Perform opration @A := alpha*A*x + beta*y@ where A is
    --   hermitian/symmetric matrix in the packed form.
    hpmv :: Uplo   -- ^ Hermitian/symmetric matrix storage mode
         -> Int    -- ^ Order of the matrix
         -> a      -- ^ Scalar /alpha/
         -> Ptr a  -- ^ Matrix data
         -> Ptr a  -- ^ Vector /x/
         -> Int    -- ^ Stride for /x/
         -> a      -- ^ Scalar /beta/
         -> Ptr a  -- ^ Vector /y/
         -> Int    -- ^ Stride for /y/
         -> Int    -- ^ Stride for /y/
         -> IO ()

    -- | Perform operation @A := alpha*x*conjg(x) + A@ where A is
    --   packed hermitian/symmetric matrix
    hpr  :: Uplo   -- ^ Hermitian/symmetric matrix storage mode
         -> Int    -- ^ Matri order
         -> Double -- ^ Scalar /alpha/
         -> Ptr a  -- ^ Vector /x/
         -> Int    -- ^ Stride for /x/
         -> Ptr a  -- ^ Matrix data
         -> IO ()

    -- | Perform operation @A := alpha*x*conjg( y' ) + conjg( alpha )*y*conjg( x' ) + A@ 
    --   /A/ is packed hermitian or symmetric matrix.
    hpr2 :: Uplo   -- ^ Hermitian/symmetric matrix storage mode
         -> Int    -- ^ Matrix order
         -> a      -- ^ Scalar /alpha/
         -> Ptr a  -- ^ Vector /x/
         -> Int    -- ^ Stride for /x/
         -> Ptr a  -- ^ Vector /y/
         -> Int    -- ^ Stride for /y/
         -> Ptr a  -- ^ Matrix /A/
         -> IO ()

    -- | Perform operation @x := A*x@ or @x := A*x'@ or @x :=
    --   A*conjg(x')@ where @A@ is n by n unit, or non-unit, upper or
    --   lower triangular band matrix, with ( k + 1 ) diagonals.
    tbmv :: Uplo  -- ^ Upper or low triangular
         -> Trans -- ^ How should matrix be transformed
         -> Diag  -- ^ Whether matrix is unit diagonal or not
         -> Int   -- ^ Matrix order
         -> Int   -- ^ Number of superdiagonals /k/
         -> Ptr a -- ^ Matrix data
         -> Int   -- ^ Leading dimension of A (at least @k+1@)
         -> Ptr a -- ^ Vector /x/
         -> Int   -- ^ Stride for /x/
         -> IO ()
         
    tbsv :: Uplo
         -> Trans
         -> Diag
         -> Int
         -> Int
         -> Ptr a
         -> Int
         -> Ptr a
         -> Int
         -> IO ()

    tpmv :: Uplo
         -> Trans
         -> Diag
         -> Int
         -> Ptr a
         -> Ptr a
         -> Int
         -> IO ()

    tpsv :: Uplo
         -> Trans
         -> Diag
         -> Int
         -> Ptr a
         -> Ptr a
         -> Int
         -> IO ()

    trmv :: Uplo
         -> Trans
         -> Diag
         -> Int
         -> Ptr a
         -> Int
         -> Ptr a
         -> Int
         -> IO ()

    trsv :: Uplo
         -> Trans
         -> Diag
         -> Int
         -> Ptr a
         -> Int
         -> Ptr a
         -> Int
         -> IO ()


----------------------------------------------------------------
-- Double
----------------------------------------------------------------

instance BLAS2 Double where
    gemv transa m n alpha pa lda px incx beta py incy =
        withTrans transa $ \ptransa ->
        withCI m $ \pm ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
        with beta $ \pbeta ->
        withCI incy $ \pincy ->
            dgemv ptransa pm pn palpha pa plda px pincx pbeta py pincy
    {-# INLINE gemv #-}

    gbmv transa m n kl ku alpha pa lda px incx beta py incy =
        withTrans transa $ \ptransa ->
        withCI m $ \pm ->
        withCI n $ \pn ->
        withCI kl $ \pkl ->
        withCI ku $ \pku ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
        with beta $ \pbeta ->
        withCI incy $ \pincy ->
            dgbmv ptransa pm pn pkl pku palpha pa plda px pincx pbeta py pincy
    {-# INLINE gbmv #-}

    trmv uplo trans diag n pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withCI n $ \pn ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
            dtrmv puplo ptrans pdiag pn pa plda px pincx
    {-# INLINE trmv #-}

    tpmv uplo trans diag n pap px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withCI n $ \pn ->
        withCI incx $ \pincx ->
            dtpmv puplo ptrans pdiag pn pap px pincx
    {-# INLINE tpmv #-}

    tpsv uplo trans diag n pap px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withCI n $ \pn ->
        withCI incx $ \pincx ->
            dtpsv puplo ptrans pdiag pn pap px pincx
    {-# INLINE tpsv #-}

    tbmv uplo trans diag n k pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withCI n $ \pn ->
        withCI k $ \pk ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
            dtbmv puplo ptrans pdiag pn pk pa plda px pincx
    {-# INLINE tbmv #-}

    trsv uplo trans diag n pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withCI n $ \pn ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
            dtrsv puplo ptrans pdiag pn pa plda px pincx
    {-# INLINE trsv #-}

    tbsv uplo trans diag n k pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withCI n $ \pn ->
        withCI k $ \pk ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
            dtbsv puplo ptrans pdiag pn pk pa plda px pincx
    {-# INLINE tbsv #-}

    hemv uplo n alpha pa lda px incx beta py incy =
        withUplo uplo $ \puplo ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
        with beta $ \pbeta ->
        withCI incy $ \pincy ->
            dsymv puplo pn palpha pa plda px pincx pbeta py pincy
    {-# INLINE hemv #-}

    hbmv uplo n k alpha pa lda px incx beta py incy =
        withUplo uplo $ \puplo ->
        withCI n $ \pn ->
        withCI k $ \pk ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
        with beta $ \pbeta ->
        withCI incy $ \pincy ->
            dsbmv puplo pn pk palpha pa plda px pincx pbeta py pincy
    {-# INLINE hbmv #-}

    gerc m n alpha px incx py incy pa lda =
        withCI m $ \pm ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
        withCI lda $ \plda ->
            dger pm pn palpha px pincx py pincy pa plda
    {-# INLINE gerc #-}

    geru = gerc
    {-# INLINE geru #-}

    her uplo n alpha px incx pa lda =
        withUplo uplo $ \puplo ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI incx $ \pincx ->
        withCI lda $ \plda ->
            dsyr puplo pn palpha px pincx pa plda
    {-# INLINE her #-}

    her2 uplo n alpha px incx py incy pa lda =
        withUplo uplo $ \puplo ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
        withCI lda $ \plda ->
            dsyr2 puplo pn palpha px pincx py pincy pa plda
    {-# INLINE her2 #-}

    hpmv uplo n alpha pap px incx beta py incy =
        withUplo uplo $ \puplo ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI incx $ \pincx ->
        with beta $ \pbeta ->
        withCI incy $ \pincy ->
            dspmv puplo pn palpha pap px pincx pbeta py pincy
    {-# INLINE hpmv #-}

    hpr uplo n alpha px incx pap =
        withUplo uplo $ \puplo ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI incx $ \pincx ->
            dspr puplo pn palpha px pincx pap
    {-# INLINE hpr #-}

    hpr2 uplo n alpha px incx py incy pap =
        withUplo uplo $ \puplo ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
            dspr2 puplo pn palpha px pincx py pincy pap
    {-# INLINE hpr2 #-}



----------------------------------------------------------------
-- Complex Double
----------------------------------------------------------------

instance BLAS2 (Complex Double) where
    gemv transa m n alpha pa lda px incx beta py incy =
        withTrans transa $ \ptransa ->
        withCI m $ \pm ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
        with beta $ \pbeta ->
        withCI incy $ \pincy ->
            zgemv ptransa pm pn palpha pa plda px pincx pbeta py pincy
    {-# INLINE gemv #-}

    gbmv transa m n kl ku alpha pa lda px incx beta py incy =
        withTrans transa $ \ptransa ->
        withCI m $ \pm ->
        withCI n $ \pn ->
        withCI kl $ \pkl ->
        withCI ku $ \pku ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
        with beta $ \pbeta ->
        withCI incy $ \pincy ->
            zgbmv ptransa pm pn pkl pku palpha pa plda px pincx pbeta py pincy
    {-# INLINE gbmv #-}

    trmv uplo trans diag n pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withCI n $ \pn ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
            ztrmv puplo ptrans pdiag pn pa plda px pincx
    {-# INLINE trmv #-}

    tpmv uplo trans diag n pap px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withCI n $ \pn ->
        withCI incx $ \pincx ->
            ztpmv puplo ptrans pdiag pn pap px pincx
    {-# INLINE tpmv #-}

    tpsv uplo trans diag n pap px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withCI n $ \pn ->
        withCI incx $ \pincx ->
            ztpsv puplo ptrans pdiag pn pap px pincx
    {-# INLINE tpsv #-}

    tbmv uplo trans diag n k pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withCI n $ \pn ->
        withCI k $ \pk ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
            ztbmv puplo ptrans pdiag pn pk pa plda px pincx
    {-# INLINE tbmv #-}

    trsv uplo trans diag n pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withCI n $ \pn ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
            ztrsv puplo ptrans pdiag pn pa plda px pincx
    {-# INLINE trsv #-}

    tbsv uplo trans diag n k pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withCI n $ \pn ->
        withCI k $ \pk ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
            ztbsv puplo ptrans pdiag pn pk pa plda px pincx
    {-# INLINE tbsv #-}

    hemv uplo n alpha pa lda px incx beta py incy =
        withUplo uplo $ \puplo ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
        with beta $ \pbeta ->
        withCI incy $ \pincy ->
            zhemv puplo pn palpha pa plda px pincx pbeta py pincy
    {-# INLINE hemv #-}

    hbmv uplo n k alpha pa lda px incx beta py incy =
        withUplo uplo $ \puplo ->
        withCI n $ \pn ->
        withCI k $ \pk ->
        with alpha $ \palpha ->
        withCI lda $ \plda ->
        withCI incx $ \pincx ->
        with beta $ \pbeta ->
        withCI incy $ \pincy ->
            zhbmv puplo pn pk palpha pa plda px pincx pbeta py pincy
    {-# INLINE hbmv #-}

    gerc m n alpha px incx py incy pa lda =
        withCI m $ \pm ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
        withCI lda $ \plda ->
            zgerc pm pn palpha px pincx py pincy pa plda
    {-# INLINE gerc #-}

    geru m n alpha px incx py incy pa lda =
        withCI m $ \pm ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
        withCI lda $ \plda ->
            zgeru pm pn palpha px pincx py pincy pa plda
    {-# INLINE geru #-}

    her uplo n alpha px incx pa lda =
        withUplo uplo $ \puplo ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI incx $ \pincx ->
        withCI lda $ \plda ->
            zher puplo pn palpha px pincx pa plda
    {-# INLINE her #-}

    her2 uplo n alpha px incx py incy pa lda =
        withUplo uplo $ \puplo ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
        withCI lda $ \plda ->
            zher2 puplo pn palpha px pincx py pincy pa plda
    {-# INLINE her2 #-}

    hpmv uplo n alpha pap px incx beta py incy =
        withUplo uplo $ \puplo ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI incx $ \pincx ->
        with beta $ \pbeta ->
        withCI incy $ \pincy ->
            zhpmv puplo pn palpha pap px pincx pbeta py pincy
    {-# INLINE hpmv #-}

    hpr uplo n alpha px incx pap =
        withUplo uplo $ \puplo ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI incx $ \pincx ->
            zhpr puplo pn palpha px pincx pap
    {-# INLINE hpr #-}

    hpr2 uplo n alpha px incx py incy pap =
        withUplo uplo $ \puplo ->
        withCI n $ \pn ->
        with alpha $ \palpha ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
            zhpr2 puplo pn palpha px pincx py pincy pap
    {-# INLINE hpr2 #-}
