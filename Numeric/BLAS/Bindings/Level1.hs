{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.BLAS.Bindings.Level1
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--
-- Vector operations.
--

module Numeric.BLAS.Bindings.Level1 (
    BLAS1(..),
    ) where

import Foreign                  ( Storable, Ptr, peek, with )
import Foreign.Storable.Complex ()
import Data.Complex             ( Complex )

import Numeric.BLAS.Bindings.Types
import Numeric.BLAS.Bindings.Double
import Numeric.BLAS.Bindings.Zomplex

-- | Types with vector-vector operations.
class (Storable a) => BLAS1 a where
  -- | Copy vector into another vector. @y <- x@
  copy :: Int   -- ^ Number of elements in vector
       -> Ptr a -- ^ Source vector /x/
       -> Int   -- ^ Stride for /x/
       -> Ptr a -- ^ Target vector /y/
       -> Int   -- ^ Stride for /y/
       -> IO ()

  -- | Swap content of two vectors. @y <-> x@
  swap :: Int   -- ^ Number of elements in vector
       -> Ptr a -- ^ Source vector /x/
       -> Int   -- ^ Stride for /x/
       -> Ptr a -- ^ Target vector /y/
       -> Int   -- ^ Stride for /x/
       -> IO ()

  -- | Computes the hermitian dot product of vector /a/ and vector
  -- /b/. For real-valued vectors is same as dotu
  dotc :: Int   -- ^ Number of elements in vectors
       -> Ptr a -- ^ Vector /a/
       -> Int   -- ^ Stride for /a/
       -> Ptr a -- ^ Vector /b/
       -> Int   -- ^ Stride for /b/
       -> IO a

  -- | Computes the dot product of vector /a/ and vector /b/
  dotu :: Int   -- ^ Number of elements in vectors
       -> Ptr a -- ^ Vector /a/
       -> Int   -- ^ Stride for /a/
       -> Ptr a -- ^ Vector /b/
       -> Int   -- ^ Stride for /b/
       -> IO a

  -- | Computes euqlidean norm of the vector
  nrm2 :: Int   -- ^ Number of elements in vector
       -> Ptr a -- ^ Vector /a/
       -> Int   -- ^ Stride for /a/
       -> IO Double

  -- | Compute sums of absolute values of a vector
  asum  :: Int   -- ^ Number of elements in vector
        -> Ptr a -- ^ Vector /a/
        -> Int   -- ^ Stride for /a/
        -> IO Double

  -- | Finds index of maximum absolute value in the vector
  iamax :: Int    -- ^ Number of elements in vector
        -> Ptr a  -- ^ Vector /a/
        -> Int    -- ^ Stride for /a/
        -> IO Int

  -- | Scale all vector by constant
  scal  :: Int   -- ^ Number of elements in the vector
        -> a     -- ^ Scale factor
        -> Ptr a -- ^ Vector /x/
        -> Int   -- ^ Stride for /x/
        -> IO ()

  -- | Adds scaled vector to another vector. @y <- alpha * x + y@
  axpy  :: Int   -- ^ Number of elements in vector
        -> a     -- ^ /alpha/ coefficient
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

  -- | Plane rotation subroutine
  rot   :: Int    -- ^ Number of ordered pairs
        -> Ptr a  -- ^ Vector of /x/ coordinates
        -> Int    -- ^ Stride for /x/
        -> Ptr a  -- ^ Vector of /y/ coordinates
        -> Int    -- ^ Stride for /y/
        -> Double -- ^ Cosine of rotation angle
        -> Double -- ^ Sine of rotation angle
        -> IO ()


instance BLAS1 Double where
    copy n px incx py incy =
        withCI n    $ \pn ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
            dcopy pn px pincx py pincy
    {-# INLINE copy #-}

    swap n px incx py incy =
        withCI n    $ \pn ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
            dswap pn px pincx py pincy
    {-# INLINE swap #-}

    dotc n px incx py incy =
        withCI n    $ \pn ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
            ddot pn px pincx py pincy
    {-# INLINE dotc #-}

    dotu = dotc
    {-# INLINE dotu #-}

    nrm2 n px incx =
        withCI n $ \pn ->
        withCI incx $ \pincx ->
            dnrm2 pn px pincx
    {-# INLINE nrm2 #-}

    asum n px incx =
        withCI n $ \pn ->
        withCI incx $ \pincx ->
            dasum pn px pincx
    {-# INLINE asum #-}

    iamax n px incx =
        withCI n    $ \pn ->
        withCI incx $ \pincx -> do
          i <- idamax pn px pincx
          return $! fromIntegral (i - 1)
    {-# INLINE iamax #-}

    axpy n alpha px incx py incy =
        withCI n     $ \pn ->
        with   alpha $ \palpha ->
        withCI incx  $ \pincx ->
        withCI incy  $ \pincy ->
            daxpy pn palpha px pincx py pincy
    {-# INLINE axpy #-}

    scal n alpha px incx =
        withCI n     $ \pn ->
        with   alpha $ \palpha ->
        withCI incx  $ \pincx ->
            dscal pn palpha px pincx
    {-# INLINE scal #-}

    rotg = drotg
    {-# INLINE rotg #-}

    rot n px incx py incy c s =
        withCI n    $ \pn ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
        with   c    $ \pc ->
        with   s    $ \ps ->
            drot pn px pincx py pincy pc ps
    {-# INLINE rot #-}


instance BLAS1 (Complex Double) where
    copy n px incx py incy =
        withCI n    $ \pn ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
            zcopy pn px pincx py pincy
    {-# INLINE copy #-}

    swap n px incx py incy =
        withCI n    $ \pn ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
            zswap pn px pincx py pincy
    {-# INLINE swap #-}

    dotc n px incx py incy =
        withCI n    $ \pn ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
        with   0    $ \pdotc -> do
          zdotc pdotc pn px pincx py pincy
          peek pdotc
    {-# INLINE dotc #-}

    dotu n px incx py incy =
        withCI n    $ \pn ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
        with   0    $ \pdotu -> do
          zdotu pdotu pn px pincx py pincy
          peek pdotu
    {-# INLINE dotu #-}

    nrm2 n px incx =
        withCI n    $ \pn ->
        withCI incx $ \pincx ->
            znrm2 pn px pincx
    {-# INLINE nrm2 #-}

    asum n px incx =
        withCI n    $ \pn ->
        withCI incx $ \pincx ->
            zasum pn px pincx
    {-# INLINE asum #-}

    iamax n px incx =
        withCI n    $ \pn ->
        withCI incx $ \pincx -> do
            i <- izamax pn px pincx
            return $! fromIntegral (i - 1)
    {-# INLINE iamax #-}

    axpy n alpha px incx py incy =
        withCI n     $ \pn ->
        with   alpha $ \palpha ->
        withCI incx  $ \pincx ->
        withCI incy  $ \pincy ->
            zaxpy pn palpha px pincx py pincy
    {-# INLINE axpy #-}

    scal n alpha px incx =
        withCI n     $ \pn ->
        with   alpha $ \palpha ->
        withCI incx  $ \pincx ->
            zscal pn palpha px pincx
    {-# INLINE scal #-}

    rotg = zrotg
    {-# INLINE rotg #-}

    rot n px incx py incy c s =
        withCI n    $ \pn ->
        withCI incx $ \pincx ->
        withCI incy $ \pincy ->
        with c $ \pc ->
        with s $ \ps ->
            zdrot pn px pincx py pincy pc ps
    {-# INLINE rot #-}
