{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.BLAS.Bindings.Double
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--

module Numeric.BLAS.Bindings.Double
    where

import Foreign
import Numeric.BLAS.Bindings.Types

#include "config.h"
#include "f77_func-hsc.h"


---------------------------- Level 1 Routines -------------------------------

foreign import ccall unsafe #f77_func ddot
    ddot :: Ptr CInt
         -> Ptr Double
         -> Ptr CInt
         -> Ptr Double
         -> Ptr CInt
         -> IO Double

foreign import ccall unsafe #f77_func dnrm2
    dnrm2  :: Ptr CInt
           -> Ptr Double
           -> Ptr CInt
           -> IO Double

foreign import ccall unsafe #f77_func dasum
    dasum  :: Ptr CInt
           -> Ptr Double
           -> Ptr CInt
           -> IO Double

foreign import ccall unsafe #f77_func idamax
    idamax :: Ptr CInt
           -> Ptr Double
           -> Ptr CInt
           -> IO CInt

foreign import ccall unsafe #f77_func dscal
    dscal  :: Ptr CInt
           -> Ptr Double
           -> Ptr Double
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func dswap
    dswap  :: Ptr CInt
           -> Ptr Double
           -> Ptr CInt
           -> Ptr Double
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func dcopy
    dcopy  :: Ptr CInt
           -> Ptr Double
           -> Ptr CInt
           -> Ptr Double
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func daxpy
    daxpy  :: Ptr CInt
           -> Ptr Double
           -> Ptr Double
           -> Ptr CInt
           -> Ptr Double
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func drotg
    drotg  :: Ptr Double
           -> Ptr Double
           -> Ptr Double
           -> Ptr Double
           -> IO ()

foreign import ccall unsafe #f77_func drot
    drot :: Ptr CInt
         -> Ptr Double
         -> Ptr CInt
         -> Ptr Double
         -> Ptr CInt
         -> Ptr Double
         -> Ptr Double
         -> IO ()

foreign import ccall unsafe #f77_func drotmg
    drotmg :: Ptr Double
           -> Ptr Double
           -> Ptr Double
           -> Ptr Double
           -> Ptr Double
           -> IO ()

foreign import ccall unsafe #f77_func drotm
    drotm :: Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> IO ()


---------------------------- Level 2 Routines -------------------------------

foreign import ccall unsafe #f77_func dgemv
    dgemv :: BLASTrans
          -> Ptr CInt
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func dgbmv
    dgbmv :: BLASTrans
          -> Ptr CInt
          -> Ptr CInt
          -> Ptr CInt
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func dtrmv
    dtrmv :: BLASUplo
          -> BLASTrans
          -> BLASDiag
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func dtpmv
    dtpmv :: BLASUplo
          -> BLASTrans
          -> BLASDiag
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func dtpsv
    dtpsv :: BLASUplo
          -> BLASTrans
          -> BLASDiag
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func dtbmv
    dtbmv :: BLASUplo
          -> BLASTrans
          -> BLASDiag
          -> Ptr CInt
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func dtrsv
    dtrsv :: BLASUplo
          -> BLASTrans
          -> BLASDiag
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func dtbsv
    dtbsv :: BLASUplo
          -> BLASTrans
          -> BLASDiag
          -> Ptr CInt
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func dsymv
    dsymv :: BLASUplo
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func dsbmv
    dsbmv :: BLASUplo
          -> Ptr CInt
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func dger
    dger  :: Ptr CInt
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func dsyr
    dsyr  :: BLASUplo
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func dsyr2
    dsyr2 :: BLASUplo
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func dspmv
    dspmv :: BLASUplo
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func dspr
    dspr  :: BLASUplo
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> IO ()

foreign import ccall unsafe #f77_func dspr2
    dspr2 :: BLASUplo
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> Ptr CInt
          -> Ptr Double
          -> IO ()


---------------------------- Level 3 Routines -------------------------------

foreign import ccall unsafe #f77_func dgemm
    dgemm  :: BLASTrans
           -> BLASTrans
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr Double
           -> Ptr Double
           -> Ptr CInt
           -> Ptr Double
           -> Ptr CInt
           -> Ptr Double
           -> Ptr Double
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func dsymm
    dsymm  :: BLASSide
           -> BLASUplo
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr Double
           -> Ptr Double
           -> Ptr CInt
           -> Ptr Double
           -> Ptr CInt
           -> Ptr Double
           -> Ptr Double
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func dtrmm
    dtrmm  :: BLASSide
           -> BLASUplo
           -> BLASTrans
           -> BLASDiag
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr Double
           -> Ptr Double
           -> Ptr CInt
           -> Ptr Double
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func dtrsm
    dtrsm  :: BLASSide
           -> BLASUplo
           -> BLASTrans
           -> BLASDiag
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr Double
           -> Ptr Double
           -> Ptr CInt
           -> Ptr Double
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func dsyrk
    dsyrk  :: BLASUplo
           -> BLASTrans
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr Double
           -> Ptr Double
           -> Ptr CInt
           -> Ptr Double
           -> Ptr Double
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func dsyr2k
    dsyr2k :: BLASUplo
           -> BLASTrans
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr Double
           -> Ptr Double
           -> Ptr CInt
           -> Ptr Double
           -> Ptr CInt
           -> Ptr Double
           -> Ptr Double
           -> Ptr CInt
           -> IO ()
