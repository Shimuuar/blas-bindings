-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.BLAS.Bindings
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>,
--              2012 Aleksey Khudyakov <alexey.skladnoy@gmail.com>
-- License    : BSD3
-- Maintainer : Aleksey Khudyakov <alexey.skladnoy@gmail.com>
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
    -- $summary_2
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
-- Here is incomplete description of memory layout of vectors and
-- matrices.
--
--
-- [@Vectors@]
--
-- Vectors in the BLAS are strided which means elements are not
-- nessesarily laid contigously (stride 1) in memory but separated by
-- some number of other elements. This way vectors could represent
-- both rows and columns of matrix among other things. For example
-- here is vector with stride 3. Elements which are not refernced
-- denoted with @_@.
--
-- > 1 _ _ 2 _ _ 3 ...
--
--
-- [@Matrices@]
--
-- Matrix layout is more complicated and there are many variants of
-- matrices. First option is whether data is stored in column major or
-- row major order. By default FORTRAN (and so BLAS) uses column major
-- and C row major ordering. CBLAS can use both which is chosen by
-- first parameter in functions where it matters
-- ('RowOrder'). Everywhere column major order is assumed.
--
--
-- [@Generic dense matrices@]
--
-- It's simplest matrix and it's laid in memory like that:
--
-- > 1 4 6   ↑         / 1 4 6 \
-- > 2 5 7   │ LDA   ⇒ \ 2 5 7 /
-- > _ _ _   ↓
--
-- This matrix have 2 rows and 3 columns. Similarly to vectors columns
-- need not be laid out contigously. BLAS distinguish logical (2) and
-- physical (3) sizes of column. This way one could take block out of
-- existing matrix.
--
--
-- [@Dense symmetric\/hermitian\/triangular matrices@]
--
-- These matrices are laid in memory just like general matrices only
-- part of the data is actually used. Whether part above or below
-- diagonal is used determined by 'Uplo' parameter. Here is example
-- for symmetric matrix:
--
-- > 1 4 7      1 4 7
-- > _ 5 8   ⇒  4 5 8      Upper
-- > _ _ 9      7 8 9
--
-- > 1 _ _      1 2 3
-- > 2 5 _   ⇒  2 5 6      Lower
-- > 3 6 9      3 6 9

-- $summary_2
--
-- Level 2 BLAS contain a lot of routines. Here is incomplete summary:
--
--  > y ← α·A·x + β·y
--
--  [@Generic dense matrices@] 'gemv'
--
--  [@Hermitian/symmetric matrices@] 'hemv'
--
--  [@Packed hermitian/symmetric matrices@] 'hpmv'
--
--  [@Banded matrices@] 'gbmv'
--
--  [@Hermitian/symmetric banded matrices@] 'hbmv'
