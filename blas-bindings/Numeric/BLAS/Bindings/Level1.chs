{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE TypeFamilies             #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.BLAS.Bindings.Level1
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>,
--              2012 Aleksey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Aleksey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability  : experimental
--
-- Vector operations.
--
#include "blas.h"
module Numeric.BLAS.Bindings.Level1 (
    BLAS1(..),
    ) where

import Foreign                  ( Storable, Ptr, peek, with, alloca )
import Foreign.C.Types
import Foreign.Storable.Complex ()
import Data.Complex             ( Complex )

import Numeric.BLAS.Bindings.Types
import Numeric.BLAS.Bindings.Trace



-- | Level 1 BLAS. Vector-vector operations.
class (Storable a, Num a) => BLAS1 a where
  -- | Type of real value which corresponds to type @a@.
  type RealType a :: *
  -- | Copy vector into another vector:
  --
  -- > y ← x
  copy :: Int   -- ^ Number of elements in vectors /x/ and /y/.
       -> Ptr a -- ^ Source vector /x/.
       -> Int   -- ^ Stride for /x/.
       -> Ptr a -- ^ Target vector /y/.
       -> Int   -- ^ Stride for /y/.
       -> IO ()

  -- | Swap content of two vectors:
  --
  -- > y ↔ x
  swap :: Int   -- ^ Number of elements in vectors /x/ and /y/.
       -> Ptr a -- ^ Vector /x/
       -> Int   -- ^ Stride for /x/
       -> Ptr a -- ^ Vector /y/
       -> Int   -- ^ Stride for /x/
       -> IO ()

  -- | Computes the hermitian dot product of vector /a/ and vector
  -- /b/. For real-valued vectors is same as 'dotu'.
  dotc :: Int   -- ^ Number of elements in vectors /a/ and /b/.
       -> Ptr a -- ^ Vector /a/
       -> Int   -- ^ Stride for /a/
       -> Ptr a -- ^ Vector /b/
       -> Int   -- ^ Stride for /b/
       -> IO a

  -- | Computes the dot product of vector /a/ and vector /b/.
  dotu :: Int   -- ^ Number of elements in vectors /a/ and /b/.
       -> Ptr a -- ^ Vector /a/
       -> Int   -- ^ Stride for /a/
       -> Ptr a -- ^ Vector /b/
       -> Int   -- ^ Stride for /b/
       -> IO a

  -- | Computes Euclidean norm of the vector.
  nrm2 :: Int   -- ^ Number of elements in vector.
       -> Ptr a -- ^ Vector /a/
       -> Int   -- ^ Stride for /a/
       -> IO (RealType a)

  -- | Compute sums of absolute values of a vector.
  asum  :: Int   -- ^ Number of elements in vector.
        -> Ptr a -- ^ Vector /a/
        -> Int   -- ^ Stride for /a/
        -> IO (RealType a)

  -- | Finds index of maximum absolute value in the vector.
  iamax :: Int    -- ^ Number of elements in vector.
        -> Ptr a  -- ^ Vector /a/
        -> Int    -- ^ Stride for /a/
        -> IO Int

  -- | Scale all vector by constant:
  --
  -- > x ← αx
  scal  :: Int   -- ^ Number of elements in the vector.
        -> a     -- ^ Scalar /α/
        -> Ptr a -- ^ Vector /x/
        -> Int   -- ^ Stride for /x/
        -> IO ()

  -- | Adds scaled vector to another vector:
  --
  -- > y ← αx + y
  axpy  :: Int   -- ^ Number of elements in vector.
        -> a     -- ^ Scalar /α /
        -> Ptr a -- ^ Vector /x/
        -> Int   -- ^ Stride for /x/
        -> Ptr a -- ^ Vector /y/
        -> Int   -- ^ Stride for /y/
        -> IO ()

  -- | computes the elements of a given plane rotation matrix such
  --   that:
  --
  -- > | c  s |   | a |    | r |
  -- > |-s  c | * | b | =  | 0 |
  rotg  :: Ptr a -- ^ /a/
        -> Ptr a -- ^ /b/
        -> Ptr a -- ^ cosine of rotation angle
        -> Ptr a -- ^ sine of rotation angle
        -> IO ()

  -- | Rotates number of points on two dimensional plane. Coordinates
  --   are modified in place.
  rot   :: Int        -- ^ Number of ordered pairs.
        -> Ptr a      -- ^ Vector of /x/ coordinates.
        -> Int        -- ^ Stride for /x/.
        -> Ptr a      -- ^ Vector of /y/ coordinates.
        -> Int        -- ^ Stride for /y/.
        -> RealType a -- ^ Cosine of rotation angle.
        -> RealType a -- ^ Sine of rotation angle.
        -> IO ()



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance BLAS1 Float where
  type RealType Float = Float
  copy n px incx py incy =
    traceBLAS "copy"
    {#call cblas_scopy #} (toI n) (ptrF px) (toI incx) (ptrF py) (toI incy)
  {-# INLINE copy #-}
  swap n px incx py incy =
    traceBLAS "swap"
    {#call cblas_sswap #} (toI n) (ptrF px) (toI incx) (ptrF py) (toI incy)
  {-# INLINE swap #-}
  dotc n px incx py incy = do
    x <- traceBLAS "dotc"
         {#call cblas_sdot #} (toI n) (ptrF px) (toI incx) (ptrF py) (toI incy)
    return $! fromF x
  {-# INLINE dotc #-}
  dotu = dotc
  {-# INLINE dotu #-}
  nrm2 n px incx = do
    x <- traceBLAS "nrm2"
         {#call cblas_snrm2 #} (toI n) (ptrF px) (toI incx)
    return $! fromF x
  {-# INLINE nrm2 #-}
  asum n px incx = do
    x <- traceBLAS "asum"
         {#call cblas_sasum #} (toI n) (ptrF px) (toI incx)
    return $! fromF x
  {-# INLINE asum #-}
  iamax n px incx = do
    x <- traceBLAS "iamax"
         {#call cblas_isamax #} (toI n) (ptrF px) (toI incx)
    return $! fromIntegral x
  {-# INLINE iamax #-}
  axpy n alpha px incx py incy =
    traceBLAS "axpy"
    {#call cblas_saxpy #} (toI n) (toF alpha) (ptrF px) (toI incx) (ptrF py) (toI incy)
  {-# INLINE axpy #-}
  scal n alpha px incx =
    traceBLAS "scal"
    {#call cblas_sscal #} (toI n) (toF alpha) (ptrF px) (toI incx)
  {-# INLINE scal #-}
  rotg a b c d =
    traceBLAS "rotg"
    {#call cblas_srotg #} (ptrF a) (ptrF b) (ptrF c) (ptrF d)
  {-# INLINE rotg #-}
  rot n px incx py incy c s =
    traceBLAS "rot"
    {#call cblas_srot #} (toI n) (ptrF px) (toI incx) (ptrF py) (toI incy) (toF c) (toF s)
  {-# INLINE rot #-}

instance BLAS1 Double where
  type RealType Double = Double
  copy n px incx py incy =
    traceBLAS "copy"
    {#call cblas_dcopy #} (toI n) (ptrD px) (toI incx) (ptrD py) (toI incy)
  {-# INLINE copy #-}
  swap n px incx py incy =
    traceBLAS "swap"
    {#call cblas_dswap #} (toI n) (ptrD px) (toI incx) (ptrD py) (toI incy)
  {-# INLINE swap #-}
  dotc n px incx py incy = do
    x <- traceBLAS "dotc"
         {#call cblas_ddot #} (toI n) (ptrD px) (toI incx) (ptrD py) (toI incy)
    return $! fromD x
  {-# INLINE dotc #-}
  dotu = dotc
  {-# INLINE dotu #-}
  nrm2 n px incx = do
    x <- traceBLAS "nrm2"
         {#call cblas_dnrm2 #} (toI n) (ptrD px) (toI incx)
    return $! fromD x
  {-# INLINE nrm2 #-}
  asum n px incx = do
    x <- traceBLAS "asum"
         {#call cblas_dasum #} (toI n) (ptrD px) (toI incx)
    return $! fromD x
  {-# INLINE asum #-}
  iamax n px incx = do
    x <- traceBLAS "iamax"
         {#call cblas_idamax #} (toI n) (ptrD px) (toI incx)
    return $! fromIntegral x
  {-# INLINE iamax #-}
  axpy n alpha px incx py incy =
    traceBLAS "axpy"
    {#call cblas_daxpy #} (toI n) (toD alpha) (ptrD px) (toI incx) (ptrD py) (toI incy)
  {-# INLINE axpy #-}
  scal n alpha px incx =
    traceBLAS "scal"
    {#call cblas_dscal #} (toI n) (toD alpha) (ptrD px) (toI incx)
  {-# INLINE scal #-}
  rotg a b c d =
    traceBLAS "rotg"
    {#call cblas_drotg #} (ptrD a) (ptrD b) (ptrD c) (ptrD d)
  {-# INLINE rotg #-}
  rot n px incx py incy c s =
    traceBLAS "rot"
    {#call cblas_drot #} (toI n) (ptrD px) (toI incx) (ptrD py) (toI incy) (toD c) (toD s)
  {-# INLINE rot #-}


instance BLAS1 (Complex Float) where
  type RealType (Complex Float) = Float
  copy n px incx py incy =
    traceBLAS "copy"
    {#call cblas_ccopy #} (toI n) (ptrC px) (toI incx) (ptrC py) (toI incy)
  {-# INLINE copy #-}
  swap n px incx py incy =
    traceBLAS "swap"
    {#call cblas_cswap #} (toI n) (ptrC px) (toI incx) (ptrC py) (toI incy)
  {-# INLINE swap #-}
  dotc n px incx py incy =
    alloca $ \res -> do
      traceBLAS "dotc"
        {#call cblas_cdotc_sub #} (toI n) (ptrC px) (toI incx) (ptrC py) (toI incy) (ptrC res)
      peek res
  {-# INLINE dotc #-}
  dotu n px incx py incy =
    alloca $ \res -> do
      traceBLAS "dotu"
        {#call cblas_cdotu_sub #} (toI n) (ptrC px) (toI incx) (ptrC py) (toI incy) (ptrC res)
      peek res
  {-# INLINE dotu #-}
  nrm2 n px incx = do
    x <- traceBLAS "nrm2"
         {#call cblas_scnrm2 #} (toI n) (ptrC px) (toI incx)
    return $! fromF x
  {-# INLINE nrm2 #-}
  asum n px incx = do
    x <- traceBLAS "asum"
         {#call cblas_scasum #} (toI n) (ptrC px) (toI incx)
    return $! fromF x
  {-# INLINE asum #-}
  iamax n px incx = do
    i <- traceBLAS "iamax"
         {#call cblas_icamax #} (toI n) (ptrC px) (toI incx)
    return $! fromIntegral i
  {-# INLINE iamax #-}
  axpy n alpha px incx py incy =
    with alpha $ \palpha ->
      traceBLAS "axpy"
      {#call cblas_caxpy #} (toI n) (ptrC palpha) (ptrC px) (toI incx) (ptrC py) (toI incy)
  {-# INLINE axpy #-}
  scal n alpha px incx =
    with alpha $ \palpha ->
      traceBLAS "scal"
      {#call cblas_cscal #} (toI n) (ptrC palpha) (ptrC px) (toI incx)
  {-# INLINE scal #-}
  rotg a b c d =
    traceBLAS "rotg"
    {#call cblas_crotg #} (ptrC a) (ptrC b) (ptrC c) (ptrC d)
  {-# INLINE rotg #-}
  rot n px incx py incy c s =
    traceBLAS "rot"
    {#call cblas_csrot #} (toI n) (ptrC px) (toI incx) (ptrC py) (toI incy) (toF c) (toF s)
  {-# INLINE rot #-}


instance BLAS1 (Complex Double) where
  type RealType (Complex Double) = Double
  copy n px incx py incy =
    traceBLAS "copy"
    {#call cblas_zcopy #} (toI n) (ptrZ px) (toI incx) (ptrZ py) (toI incy)
  {-# INLINE copy #-}
  swap n px incx py incy =
    traceBLAS "swap"
    {#call cblas_zswap #} (toI n) (ptrZ px) (toI incx) (ptrZ py) (toI incy)
  {-# INLINE swap #-}
  dotc n px incx py incy =
    alloca $ \res -> do
      traceBLAS "dotc"
        {#call cblas_zdotc_sub #} (toI n) (ptrZ px) (toI incx) (ptrZ py) (toI incy) (ptrZ res)
      peek res
  {-# INLINE dotc #-}
  dotu n px incx py incy =
    alloca $ \res -> do
      traceBLAS "dotu"
        {#call cblas_zdotu_sub #} (toI n) (ptrZ px) (toI incx) (ptrZ py) (toI incy) (ptrZ res)
      peek res
  {-# INLINE dotu #-}
  nrm2 n px incx = do
    x <- traceBLAS "nrm2"
         {#call cblas_dznrm2 #} (toI n) (ptrZ px) (toI incx)
    return $! fromD x
  {-# INLINE nrm2 #-}
  asum n px incx = do
    x <- traceBLAS "asum"
         {#call cblas_dzasum #} (toI n) (ptrZ px) (toI incx)
    return $! fromD x
  {-# INLINE asum #-}
  iamax n px incx = do
    i <- traceBLAS "iamax"
         {#call cblas_izamax #} (toI n) (ptrZ px) (toI incx)
    return $! fromIntegral i
  {-# INLINE iamax #-}
  axpy n alpha px incx py incy =
    with alpha $ \palpha ->
      traceBLAS "axpy"
      {#call cblas_zaxpy #} (toI n) (ptrZ palpha) (ptrZ px) (toI incx) (ptrZ py) (toI incy)
  {-# INLINE axpy #-}
  scal n alpha px incx =
    with alpha $ \palpha ->
      traceBLAS "scal"
      {#call cblas_zscal #} (toI n) (ptrZ palpha) (ptrZ px) (toI incx)
  {-# INLINE scal #-}
  rotg a b c d =
    traceBLAS "rotg"
    {#call cblas_zrotg #} (ptrZ a) (ptrZ b) (ptrZ c) (ptrZ d)
  {-# INLINE rotg #-}
  rot n px incx py incy c s =
    traceBLAS "rot"
    {#call cblas_zdrot #} (toI n) (ptrZ px) (toI incx) (ptrZ py) (toI incy) (toD c) (toD s)
  {-# INLINE rot #-}
