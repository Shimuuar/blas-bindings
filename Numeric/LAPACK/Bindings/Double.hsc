{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.LAPACK.Bindings.Double
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--

module Numeric.LAPACK.Bindings.Double
    where

import Foreign( Ptr )
import Numeric.BLAS.Bindings.Types
import Numeric.LAPACK.Bindings.Types

#include "f77_func-hsc.h"


foreign import ccall unsafe #f77_func dgeqrf
    dgeqrf :: Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr CInt -> Ptr Double
           -> Ptr Double -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe #f77_func dgelqf
    dgelqf :: Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr CInt -> Ptr Double
           -> Ptr Double -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe #f77_func dlarfg
    dlarfg :: Ptr CInt -> Ptr Double -> Ptr Double -> Ptr CInt -> Ptr Double
           -> IO ()

foreign import ccall unsafe #f77_func dormqr
    dormqr :: BLASSide -> BLASTrans -> Ptr CInt -> Ptr CInt -> Ptr CInt
           -> Ptr Double -> Ptr CInt -> Ptr Double -> Ptr Double -> Ptr CInt
           -> Ptr Double -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe #f77_func dormlq
    dormlq :: BLASSide -> BLASTrans -> Ptr CInt -> Ptr CInt -> Ptr CInt
           -> Ptr Double -> Ptr CInt -> Ptr Double -> Ptr Double -> Ptr CInt
           -> Ptr Double -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe #f77_func dpotrf
    dpotrf :: BLASUplo -> Ptr CInt -> Ptr Double -> Ptr CInt -> Ptr CInt
           -> IO ()
           
foreign import ccall unsafe #f77_func dpotrs
    dpotrs :: BLASUplo -> Ptr CInt -> Ptr CInt -> Ptr Double -> Ptr CInt
           -> Ptr Double -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe #f77_func dpptrf
    dpptrf :: BLASUplo -> Ptr CInt -> Ptr Double -> Ptr CInt
           -> IO ()
           
foreign import ccall unsafe #f77_func dpptrs
    dpptrs :: BLASUplo -> Ptr CInt -> Ptr CInt -> Ptr Double
           -> Ptr Double -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe #f77_func dsyevr
    dsyevr :: LAEigJob -> LAEigRange -> BLASUplo -> Ptr CInt -> Ptr Double
           -> Ptr CInt ->  Ptr Double -> Ptr Double -> Ptr CInt -> Ptr CInt
           -> Ptr Double -> Ptr CInt -> Ptr Double -> Ptr Double -> Ptr CInt
           -> Ptr CInt -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr CInt
           -> Ptr CInt -> IO ()
