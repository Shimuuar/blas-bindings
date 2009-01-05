{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Data.Elem.BLAS.Level3
-- Copyright  : Copyright (c) 2008, Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Data.Elem.BLAS.Level3
    where
     
import Data.Complex 
import Foreign ( Ptr, with )   

import BLAS.Types
import BLAS.CTypes
import Data.Elem.BLAS.Level2
import Data.Elem.BLAS.Double  
import Data.Elem.BLAS.Zomplex 
        
-- | Types with matrix-matrix operations.        
class (BLAS2 a) => BLAS3 a where
    gemm  :: Trans -> Trans -> Int -> Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
    symm  :: Side -> UpLo -> Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
    hemm  :: Side -> UpLo -> Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
    trmm  :: Side -> UpLo -> Trans -> Diag -> Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> IO ()
    trsm  :: Side -> UpLo -> Trans -> Diag -> Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> IO ()
    syrk  :: UpLo -> Trans -> Int -> Int -> a -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
    syr2k :: UpLo -> Trans -> Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
    herk  :: UpLo -> Trans -> Int -> Int -> a -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
    her2k :: UpLo -> Trans -> Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
    
    
instance BLAS3 Double where
    gemm ta tb = dgemm (cblasTrans ta) (cblasTrans tb)
    symm  s u = dsymm (cblasSide s) (cblasUpLo u) 
    hemm  s u = dsymm (cblasSide s) (cblasUpLo u) 
    trmm  s u t d = dtrmm (cblasSide s) (cblasUpLo u) (cblasTrans t) (cblasDiag d)
    trsm  s u t d = dtrsm (cblasSide s) (cblasUpLo u) (cblasTrans t) (cblasDiag d)
    syrk  u t = dsyrk  (cblasUpLo u) (cblasTrans t)
    syr2k u t = dsyr2k (cblasUpLo u) (cblasTrans t)
    herk  u t = dsyrk  (cblasUpLo u) (cblasTrans t)
    her2k u t = dsyr2k (cblasUpLo u) (cblasTrans t)
    
    
instance BLAS3 (Complex Double) where
    gemm transA transB m n k alpha pA ldA pB ldB beta pC ldC =
        with alpha $ \pAlpha -> with beta $ \pBeta ->
            zgemm (cblasTrans transA) (cblasTrans transB) m n k pAlpha pA ldA pB ldB pBeta pC ldC
    
    symm side uplo m n alpha pA ldA pB ldB beta pC ldC =
        with alpha $ \pAlpha -> with beta $ \pBeta ->
            zsymm (cblasSide side) (cblasUpLo uplo) m n pAlpha pA ldA pB ldB pBeta pC ldC

    hemm side uplo m n alpha pA ldA pB ldB beta pC ldC =
        with alpha $ \pAlpha -> with beta $ \pBeta ->
            zhemm (cblasSide side) (cblasUpLo uplo) m n pAlpha pA ldA pB ldB pBeta pC ldC
    
    trmm side uplo transA diag m n alpha pA ldA pB ldB =
        with alpha $ \pAlpha -> 
            ztrmm (cblasSide side) (cblasUpLo uplo) (cblasTrans transA) (cblasDiag diag) m n pAlpha pA ldA pB ldB
            
    trsm side uplo transA diag m n alpha pA ldA pB ldB =
        with alpha $ \pAlpha -> 
            ztrsm (cblasSide side) (cblasUpLo uplo) (cblasTrans transA) (cblasDiag diag) m n pAlpha pA ldA pB ldB
            
    syrk uplo transA n k alpha pA ldA beta pC ldC =
        with alpha $ \pAlpha -> with beta $ \pBeta ->
            zsyrk (cblasUpLo uplo) (cblasTrans transA) n k pAlpha pA ldA pBeta pC ldC
            
    syr2k uplo transA n k alpha pA ldA pB ldB beta pC ldC =
        with alpha $ \pAlpha -> with beta $ \pBeta ->
            zsyr2k (cblasUpLo uplo) (cblasTrans transA) n k pAlpha pA ldA pB ldB pBeta pC ldC

    herk uplo transA n k alpha pA ldA beta pC ldC =
        with alpha $ \pAlpha -> with beta $ \pBeta ->
            zherk (cblasUpLo uplo) (cblasTrans transA) n k pAlpha pA ldA pBeta pC ldC
            
    her2k uplo transA n k alpha pA ldA pB ldB beta pC ldC =
        with alpha $ \pAlpha -> with beta $ \pBeta ->
            zher2k (cblasUpLo uplo) (cblasTrans transA) n k pAlpha pA ldA pB ldB pBeta pC ldC