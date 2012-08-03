-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.BLAS.Bindings
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--
-- Low level bindings to the BLAS library.
--

module Numeric.BLAS.Bindings (
    -- * BLAS typeclasses
    -- ** Level 1
    -- $memory
    module Numeric.BLAS.Bindings.Level1
    -- ** Level 2
  , module Numeric.BLAS.Bindings.Level2
    -- ** Level 3
  , module Numeric.BLAS.Bindings.Level3
    -- * Enumerations
  , RowOrder(..)
  , Trans(..)
  , Uplo(..)
  , Diag(..)
  , Side(..)
  ) where

import Numeric.BLAS.Bindings.Level1
import Numeric.BLAS.Bindings.Level2
import Numeric.BLAS.Bindings.Level3
import Numeric.BLAS.Bindings.Types

-- $memory
--
-- Here memory layout of vectors and matrices is described. Note that
-- I'm not an BLAS expert and this doucmentation may contain errors.
--
--
-- [@Vectors@]
--
-- Vectors in the BLAS are strided which means elements are not laid
-- contigously (stride 1) in memory but separated by some number of
-- other elements. This way vectors could represent both rows and
-- columns of matrix among other things. For example here is vector
-- with stride 3:
--
-- > 1 _ _ 2 _ _ 3 ...
--
--
-- [@Matrices@]
--
-- Matrix layout is more complicated and there are many variants of
-- matrices. First question is whether data is column major or row
-- major. By default FORTRAN (and so BLAS) uses column major and C row
-- major ordering. CBLAS can use both which is chosen by first
-- parameter in functions where it matters ('RowOrder'). Everywhere
-- column major order is assumed.
--
--
-- [@Generic dense matrices@]
--
-- It's simplest matrix and it's laid in memory like that:
--
-- > 1 4 6   ↑
-- > 2 5 7   │ LDA
-- > _ _ _   ↓
--
-- This matrix have 2 rows and 3 columns. Similarly to vectors columns
-- need not be laid out contigously. BLAS distinguish logical (2) and
-- physical (3) sizes of column. This way one could take block out of
-- existing matrix.
--
--
-- [@Dense symmetric/hermitian matrices@]
--
-- Packed symmetric/hermitian matrices have two variants of storage.
--
-- > Upper      Lower
-- > 1 2 4      1 4 6
-- >   3 5      2 5
-- >     6      3
