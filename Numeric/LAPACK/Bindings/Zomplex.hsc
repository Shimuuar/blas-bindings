{-# LANGUAGE  ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.LAPACK.Bindings.Zomplex
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--

module Numeric.LAPACK.Bindings.Zomplex
    where

import Data.Complex( Complex )
import Foreign( Ptr )
import Numeric.BLAS.Bindings.Types
import Numeric.LAPACK.Bindings.Types

#include "f77_func-hsc.h"


foreign import ccall unsafe #f77_func zgeqrf
    zgeqrf :: Ptr CInt -> Ptr CInt -> Ptr (Complex Double)
           -> Ptr CInt -> Ptr (Complex Double) -> Ptr (Complex Double)
           -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe #f77_func zgelqf
    zgelqf :: Ptr CInt -> Ptr CInt -> Ptr (Complex Double)
           -> Ptr CInt -> Ptr (Complex Double) -> Ptr (Complex Double)
           -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe #f77_func zheevr
    zheevr :: LAEigJob -> LAEigRange -> BLASUplo -> Ptr CInt
           -> Ptr (Complex Double) -> Ptr CInt -> Ptr Double -> Ptr Double
           -> Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr CInt -> Ptr Double
           -> Ptr (Complex Double) -> Ptr CInt -> Ptr CInt
           -> Ptr (Complex Double) -> Ptr CInt -> Ptr Double -> Ptr CInt
           -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe #f77_func zlarfg
    zlarfg :: Ptr CInt -> Ptr (Complex Double) -> Ptr (Complex Double)
           -> Ptr CInt -> Ptr (Complex Double) -> IO ()

foreign import ccall unsafe #f77_func zpotrf
    zpotrf :: BLASUplo -> Ptr CInt -> Ptr (Complex Double) -> Ptr CInt
           -> Ptr CInt -> IO ()
           
foreign import ccall unsafe #f77_func zpotrs
    zpotrs :: BLASUplo -> Ptr CInt -> Ptr CInt -> Ptr (Complex Double)
           -> Ptr CInt -> Ptr (Complex Double) -> Ptr CInt -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func zpptrf
    zpptrf :: BLASUplo -> Ptr CInt -> Ptr (Complex Double)
           -> Ptr CInt -> IO ()
           
foreign import ccall unsafe #f77_func zpptrs
    zpptrs :: BLASUplo -> Ptr CInt -> Ptr CInt -> Ptr (Complex Double)
           -> Ptr (Complex Double) -> Ptr CInt -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func zunmqr
    zunmqr :: BLASSide -> BLASTrans -> Ptr CInt -> Ptr CInt
           -> Ptr CInt -> Ptr (Complex Double) -> Ptr CInt
           -> Ptr (Complex Double) -> Ptr (Complex Double) -> Ptr CInt
           -> Ptr (Complex Double) -> Ptr CInt -> Ptr CInt
           -> IO ()

foreign import ccall unsafe #f77_func zunmlq
    zunmlq :: BLASSide -> BLASTrans -> Ptr CInt -> Ptr CInt -> Ptr CInt
           -> Ptr (Complex Double) -> Ptr CInt -> Ptr (Complex Double)
           -> Ptr (Complex Double) -> Ptr CInt -> Ptr (Complex Double)
           -> Ptr CInt -> Ptr CInt -> IO ()

