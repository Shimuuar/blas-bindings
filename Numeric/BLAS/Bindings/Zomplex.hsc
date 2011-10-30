{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.BLAS.Bindings.Zomplex
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--

module Numeric.BLAS.Bindings.Zomplex
    where

import Data.Complex( Complex )
import Foreign
import Numeric.BLAS.Bindings.Types

#include "config.h"
#include "f77_func-hsc.h"
#define la_int int

---------------------------- Level 1 Routines -------------------------------

foreign import ccall unsafe #f77_func zdotu
    zdotu :: Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func zdotc
    zdotc :: Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> IO ()


foreign import ccall unsafe #f77_func dznrm2
    znrm2  :: Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO Double

foreign import ccall unsafe #f77_func dzasum
    zasum  :: Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO Double

foreign import ccall unsafe #f77_func izamax
    izamax :: Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO CInt

foreign import ccall unsafe #f77_func zscal
    zscal  :: Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func zswap
    zswap  :: Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func zcopy
    zcopy  :: Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func zaxpy
    zaxpy  :: Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func zrotg
    zrotg  :: Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> IO ()

foreign import ccall unsafe #f77_func zdrot
    zdrot :: Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr Double
          -> Ptr Double
          -> IO ()


---------------------------- Level 2 Routines -------------------------------

foreign import ccall unsafe #f77_func zgemv
    zgemv :: BLASTrans
          -> Ptr CInt
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func zgbmv
    zgbmv :: BLASTrans
          -> Ptr CInt
          -> Ptr CInt
          -> Ptr CInt
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func ztrmv
    ztrmv :: BLASUplo
          -> BLASTrans
          -> BLASDiag
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func ztpmv
    ztpmv :: BLASUplo
          -> BLASTrans
          -> BLASDiag
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func ztpsv
    ztpsv :: BLASUplo
          -> BLASTrans
          -> BLASDiag
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func ztbmv
    ztbmv :: BLASUplo
          -> BLASTrans
          -> BLASDiag
          -> Ptr CInt
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func ztrsv
    ztrsv :: BLASUplo
          -> BLASTrans
          -> BLASDiag
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func ztbsv
    ztbsv :: BLASUplo
          -> BLASTrans
          -> BLASDiag
          -> Ptr CInt
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func zhemv
    zhemv :: BLASUplo
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func zhbmv
    zhbmv :: BLASUplo
          -> Ptr CInt
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func zgeru
    zgeru  :: Ptr CInt
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func zgerc
    zgerc  :: Ptr CInt
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func zher
    zher  :: BLASUplo
          -> Ptr CInt
          -> Ptr Double
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func zher2
    zher2 :: BLASUplo
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func zhpmv
    zhpmv :: BLASUplo
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr (Complex Double)
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> IO ()

foreign import ccall unsafe #f77_func zhpr
    zhpr  :: BLASUplo
          -> Ptr CInt
          -> Ptr Double
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> IO ()

foreign import ccall unsafe #f77_func zhpr2
    zhpr2 :: BLASUplo
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> Ptr CInt
          -> Ptr (Complex Double)
          -> IO ()


---------------------------- Level 3 Routines -------------------------------

foreign import ccall unsafe #f77_func zgemm
    zgemm  :: BLASTrans
           -> BLASTrans
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func zsymm
    zsymm  :: BLASSide
           -> BLASUplo
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func zhemm
    zhemm  :: BLASSide
           -> BLASUplo
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func ztrmm
    ztrmm  :: BLASSide
           -> BLASUplo
           -> BLASTrans
           -> BLASDiag
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func ztrsm
    ztrsm  :: BLASSide
           -> BLASUplo
           -> BLASTrans
           -> BLASDiag
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func zsyrk
    zsyrk  :: BLASUplo
           -> BLASTrans
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func zsyr2k
    zsyr2k :: BLASUplo
           -> BLASTrans
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func zherk
    zherk  :: BLASUplo
           -> BLASTrans
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func zher2k
    zher2k :: BLASUplo
           -> BLASTrans
           -> Ptr CInt
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> Ptr (Complex Double)
           -> Ptr (Complex Double)
           -> Ptr CInt
           -> IO ()
