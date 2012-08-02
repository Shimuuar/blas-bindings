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
    -- $memory
    -- ** Level 1
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
-- Vectors in the BLAS are strided
--
--
-- BLAS can work with several kinds of matrices.
--
-- Dense matrices are stored in column major order
--
-- > 1 4
-- > 2 5
-- > 3 6
--
-- Packed symmetric/hermitian matrices have two variants of storage.
--
-- > Upper      Lower
-- > 1 2 4      1 4 6
-- >   3 5      2 5
-- >     6      3
