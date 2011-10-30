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
    gemm  :: Trans
          -> Trans
          -> Int
          -> Int
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> Ptr a
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> IO ()
    symm  :: Side
          -> Uplo
          -> Int
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> Ptr a
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> IO ()
    hemm  :: Side
          -> Uplo
          -> Int
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> Ptr a
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> IO ()
    trmm  :: Side
          -> Uplo
          -> Trans
          -> Diag
          -> Int
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> Ptr a
          -> Int
          -> IO ()
    trsm  :: Side
          -> Uplo
          -> Trans
          -> Diag
          -> Int
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> Ptr a
          -> Int
          -> IO ()
    syrk  :: Uplo
          -> Trans
          -> Int
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> IO ()
    syr2k :: Uplo
          -> Trans
          -> Int
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> Ptr a
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> IO ()
    herk  :: Uplo
          -> Trans
          -> Int
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> IO ()
    her2k :: Uplo
          -> Trans
          -> Int
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> Ptr a
          -> Int
          -> a
          -> Ptr a
          -> Int
          -> IO ()
  

withCI :: Int -> (Ptr CInt -> IO b) -> IO b
withCI = with . fromIntegral
{-# INLINE withCI #-}
  
    
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
