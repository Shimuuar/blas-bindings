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
import Foreign( Ptr, Storable, with )

import Numeric.BLAS.Bindings.Types
import Numeric.BLAS.Bindings.Level1
import Numeric.BLAS.Bindings.Double
import Numeric.BLAS.Bindings.Zomplex
   
-- | Types with matrix-vector operations.
class (BLAS1 a) => BLAS2 a where
    gbmv :: Trans -> Int -> Int -> Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
    gemv :: Trans -> Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
    gerc :: Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> Ptr a -> Int -> IO ()
    geru :: Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> Ptr a -> Int -> IO ()

    hbmv :: Uplo -> Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
    hemv :: Uplo -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
    her  :: Uplo -> Int -> Double -> Ptr a -> Int -> Ptr a -> Int -> IO ()
    her2 :: Uplo -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> Ptr a -> Int -> IO ()
    hpmv :: Uplo -> Int -> a -> Ptr a -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()    
    hpr  :: Uplo -> Int -> Double -> Ptr a -> Int -> Ptr a -> IO ()
    hpr2 :: Uplo -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> Ptr a -> IO ()

    tbmv :: Uplo -> Trans -> Diag -> Int -> Int -> Ptr a -> Int -> Ptr a -> Int -> IO ()
    tbsv :: Uplo -> Trans -> Diag -> Int -> Int -> Ptr a -> Int -> Ptr a -> Int -> IO ()
    tpmv :: Uplo -> Trans -> Diag -> Int -> Ptr a -> Ptr a -> Int -> IO ()    
    tpsv :: Uplo -> Trans -> Diag -> Int -> Ptr a -> Ptr a -> Int -> IO ()        
    trmv :: Uplo -> Trans -> Diag -> Int -> Ptr a -> Int -> Ptr a -> Int -> IO ()
    trsv :: Uplo -> Trans -> Diag -> Int -> Ptr a -> Int -> Ptr a -> Int -> IO ()


withEnum :: (Enum a, Storable a) => Int -> (Ptr a -> IO b) -> IO b
withEnum = with . toEnum
{-# INLINE withEnum #-}

instance BLAS2 Double where
    gemv transa m n alpha pa lda px incx beta py incy =
        withTrans transa $ \ptransa ->
        withEnum m $ \pm ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
        with beta $ \pbeta ->
        withEnum incy $ \pincy ->
            dgemv ptransa pm pn palpha pa plda px pincx pbeta py pincy
    {-# INLINE gemv #-}
    
    gbmv transa m n kl ku alpha pa lda px incx beta py incy =
        withTrans transa $ \ptransa ->
        withEnum m $ \pm ->
        withEnum n $ \pn ->
        withEnum kl $ \pkl ->
        withEnum ku $ \pku ->
        with alpha $ \palpha ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
        with beta $ \pbeta ->
        withEnum incy $ \pincy ->
            dgbmv ptransa pm pn pkl pku palpha pa plda px pincx pbeta py pincy
    {-# INLINE gbmv #-}

    trmv uplo trans diag n pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withEnum n $ \pn ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
            dtrmv puplo ptrans pdiag pn pa plda px pincx
    {-# INLINE trmv #-}

    tpmv uplo trans diag n pap px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withEnum n $ \pn ->
        withEnum incx $ \pincx ->
            dtpmv puplo ptrans pdiag pn pap px pincx
    {-# INLINE tpmv #-}

    tpsv uplo trans diag n pap px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withEnum n $ \pn ->
        withEnum incx $ \pincx ->
            dtpsv puplo ptrans pdiag pn pap px pincx
    {-# INLINE tpsv #-}
     
    tbmv uplo trans diag n k pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withEnum n $ \pn ->
        withEnum k $ \pk ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
            dtbmv puplo ptrans pdiag pn pk pa plda px pincx
    {-# INLINE tbmv #-}

    trsv uplo trans diag n pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withEnum n $ \pn ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
            dtrsv puplo ptrans pdiag pn pa plda px pincx
    {-# INLINE trsv #-}

    tbsv uplo trans diag n k pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withEnum n $ \pn ->
        withEnum k $ \pk ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
            dtbsv puplo ptrans pdiag pn pk pa plda px pincx
    {-# INLINE tbsv #-}

    hemv uplo n alpha pa lda px incx beta py incy =
        withUplo uplo $ \puplo ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
        with beta $ \pbeta ->
        withEnum incy $ \pincy ->
            dsymv puplo pn palpha pa plda px pincx pbeta py pincy
    {-# INLINE hemv #-}

    hbmv uplo n k alpha pa lda px incx beta py incy =
        withUplo uplo $ \puplo ->
        withEnum n $ \pn ->
        withEnum k $ \pk ->
        with alpha $ \palpha ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
        with beta $ \pbeta ->
        withEnum incy $ \pincy ->
            dsbmv puplo pn pk palpha pa plda px pincx pbeta py pincy
    {-# INLINE hbmv #-}

    gerc m n alpha px incx py incy pa lda =
        withEnum m $ \pm ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum incx $ \pincx ->
        withEnum incy $ \pincy ->
        withEnum lda $ \plda ->
            dger pm pn palpha px pincx py pincy pa plda
    {-# INLINE gerc #-}

    geru = gerc
    {-# INLINE geru #-}

    her uplo n alpha px incx pa lda =
        withUplo uplo $ \puplo ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum incx $ \pincx ->
        withEnum lda $ \plda ->
            dsyr puplo pn palpha px pincx pa plda
    {-# INLINE her #-}

    her2 uplo n alpha px incx py incy pa lda =
        withUplo uplo $ \puplo ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum incx $ \pincx ->
        withEnum incy $ \pincy ->
        withEnum lda $ \plda ->
            dsyr2 puplo pn palpha px pincx py pincy pa plda
    {-# INLINE her2 #-}

    hpmv uplo n alpha pap px incx beta py incy =
        withUplo uplo $ \puplo ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum incx $ \pincx ->
        with beta $ \pbeta ->
        withEnum incy $ \pincy ->
            dspmv puplo pn palpha pap px pincx pbeta py pincy
    {-# INLINE hpmv #-}

    hpr uplo n alpha px incx pap =
        withUplo uplo $ \puplo ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum incx $ \pincx ->
            dspr puplo pn palpha px pincx pap
    {-# INLINE hpr #-}

    hpr2 uplo n alpha px incx py incy pap =
        withUplo uplo $ \puplo ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum incx $ \pincx ->
        withEnum incy $ \pincy ->
            dspr2 puplo pn palpha px pincx py pincy pap
    {-# INLINE hpr2 #-}
    

instance BLAS2 (Complex Double) where
    gemv transa m n alpha pa lda px incx beta py incy =
        withTrans transa $ \ptransa ->
        withEnum m $ \pm ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
        with beta $ \pbeta ->
        withEnum incy $ \pincy ->
            zgemv ptransa pm pn palpha pa plda px pincx pbeta py pincy
    {-# INLINE gemv #-}
    
    gbmv transa m n kl ku alpha pa lda px incx beta py incy =
        withTrans transa $ \ptransa ->
        withEnum m $ \pm ->
        withEnum n $ \pn ->
        withEnum kl $ \pkl ->
        withEnum ku $ \pku ->
        with alpha $ \palpha ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
        with beta $ \pbeta ->
        withEnum incy $ \pincy ->
            zgbmv ptransa pm pn pkl pku palpha pa plda px pincx pbeta py pincy
    {-# INLINE gbmv #-}

    trmv uplo trans diag n pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withEnum n $ \pn ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
            ztrmv puplo ptrans pdiag pn pa plda px pincx
    {-# INLINE trmv #-}

    tpmv uplo trans diag n pap px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withEnum n $ \pn ->
        withEnum incx $ \pincx ->
            ztpmv puplo ptrans pdiag pn pap px pincx
    {-# INLINE tpmv #-}

    tpsv uplo trans diag n pap px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withEnum n $ \pn ->
        withEnum incx $ \pincx ->
            ztpsv puplo ptrans pdiag pn pap px pincx
    {-# INLINE tpsv #-}
     
    tbmv uplo trans diag n k pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withEnum n $ \pn ->
        withEnum k $ \pk ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
            ztbmv puplo ptrans pdiag pn pk pa plda px pincx
    {-# INLINE tbmv #-}

    trsv uplo trans diag n pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withEnum n $ \pn ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
            ztrsv puplo ptrans pdiag pn pa plda px pincx
    {-# INLINE trsv #-}

    tbsv uplo trans diag n k pa lda px incx =
        withUplo uplo $ \puplo ->
        withTrans trans $ \ptrans ->
        withDiag diag $ \pdiag ->
        withEnum n $ \pn ->
        withEnum k $ \pk ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
            ztbsv puplo ptrans pdiag pn pk pa plda px pincx
    {-# INLINE tbsv #-}

    hemv uplo n alpha pa lda px incx beta py incy =
        withUplo uplo $ \puplo ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
        with beta $ \pbeta ->
        withEnum incy $ \pincy ->
            zhemv puplo pn palpha pa plda px pincx pbeta py pincy
    {-# INLINE hemv #-}

    hbmv uplo n k alpha pa lda px incx beta py incy =
        withUplo uplo $ \puplo ->
        withEnum n $ \pn ->
        withEnum k $ \pk ->
        with alpha $ \palpha ->
        withEnum lda $ \plda ->
        withEnum incx $ \pincx ->
        with beta $ \pbeta ->
        withEnum incy $ \pincy ->
            zhbmv puplo pn pk palpha pa plda px pincx pbeta py pincy
    {-# INLINE hbmv #-}

    gerc m n alpha px incx py incy pa lda =
        withEnum m $ \pm ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum incx $ \pincx ->
        withEnum incy $ \pincy ->
        withEnum lda $ \plda ->
            zgerc pm pn palpha px pincx py pincy pa plda
    {-# INLINE gerc #-}

    geru m n alpha px incx py incy pa lda =
        withEnum m $ \pm ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum incx $ \pincx ->
        withEnum incy $ \pincy ->
        withEnum lda $ \plda ->
            zgeru pm pn palpha px pincx py pincy pa plda
    {-# INLINE geru #-}

    her uplo n alpha px incx pa lda =
        withUplo uplo $ \puplo ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum incx $ \pincx ->
        withEnum lda $ \plda ->
            zher puplo pn palpha px pincx pa plda
    {-# INLINE her #-}

    her2 uplo n alpha px incx py incy pa lda =
        withUplo uplo $ \puplo ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum incx $ \pincx ->
        withEnum incy $ \pincy ->
        withEnum lda $ \plda ->
            zher2 puplo pn palpha px pincx py pincy pa plda
    {-# INLINE her2 #-}

    hpmv uplo n alpha pap px incx beta py incy =
        withUplo uplo $ \puplo ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum incx $ \pincx ->
        with beta $ \pbeta ->
        withEnum incy $ \pincy ->
            zhpmv puplo pn palpha pap px pincx pbeta py pincy
    {-# INLINE hpmv #-}

    hpr uplo n alpha px incx pap =
        withUplo uplo $ \puplo ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum incx $ \pincx ->
            zhpr puplo pn palpha px pincx pap
    {-# INLINE hpr #-}

    hpr2 uplo n alpha px incx py incy pap =
        withUplo uplo $ \puplo ->
        withEnum n $ \pn ->
        with alpha $ \palpha ->
        withEnum incx $ \pincx ->
        withEnum incy $ \pincy ->
            zhpr2 puplo pn palpha px pincx py pincy pap
    {-# INLINE hpr2 #-}
