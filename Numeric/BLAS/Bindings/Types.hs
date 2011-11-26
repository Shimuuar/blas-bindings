{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.BLAS.Bindings.Types
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--

module Numeric.BLAS.Bindings.Types (
    BLASTrans
  , Trans(..)
  , withTrans

  , BLASUplo
  , Uplo(..)
  , withUplo

  , BLASDiag
  , Diag(..)
  , withDiag

  , BLASSide
  , Side(..)
  , withSide

  , CInt
  , withCI
  ) where

import Foreign          (Ptr,with)
import Foreign.C.Types  (CInt)
import Foreign.C.String (CString,withCString)



newtype BLASTrans = BLASTrans CString

-- ^ Describe matrix transformation in matrix-vector or matrix-matrix
--   operation
data Trans = NoTrans   -- ^ Do not transpose matrix
           | Trans     -- ^ Transpose matrix
           | ConjTrans -- ^ Transpose and complex conjugate
           deriving (Eq, Show)

withTrans :: Trans -> (BLASTrans -> IO a) -> IO a
withTrans trans f = flip withCString (f . BLASTrans) $ case trans of
    NoTrans   -> "N"
    Trans     -> "T"
    ConjTrans -> "C"

newtype BLASUplo = BLASUplo CString

-- | Storage mode for symmetric/hermitian/triangular matrix
data Uplo = Upper -- ^ Upper part should be used
          | Lower -- ^ Lower part should be used
          deriving (Eq, Show)

withUplo :: Uplo -> (BLASUplo -> IO a) -> IO a
withUplo uplo f = flip withCString (f . BLASUplo) $ case uplo of
    Upper -> "U"
    Lower -> "L"

newtype BLASSide = BLASSide CString

data Side = LeftSide
          | RightSide
          deriving (Eq, Show)

withSide :: Side -> (BLASSide -> IO a) -> IO a
withSide side f = flip withCString (f . BLASSide) $ case side of
    LeftSide  -> "L"
    RightSide -> "R"

newtype BLASDiag = BLASDiag CString

data Diag = NonUnit
          | Unit
          deriving (Eq, Show)

withDiag :: Diag -> (BLASDiag -> IO a) -> IO a
withDiag diag f = flip withCString (f . BLASDiag) $ case diag of
    NonUnit -> "N"
    Unit    -> "U"


withCI :: Int -> (Ptr CInt -> IO b) -> IO b
withCI = with . fromIntegral
{-# INLINE withCI #-}
