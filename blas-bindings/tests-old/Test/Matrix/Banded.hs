{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Test.QuickCheck.Matrix.Banded
-- Copyright  : Copyright (c) 2008, Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Test.Matrix.Banded (
    banded,
    
    BandedAt(..),
    ListsBanded(..),
    BandedMV(..),
    BandedMVPair(..),
    BandedMM(..),
    BandedMMPair(..),
    ) where

import Debug.Trace
import Control.Monad( forM )

import Test.QuickCheck hiding ( vector )
import Test.QuickCheck.BLAS ( TestElem )
import qualified Test.QuickCheck.BLAS as Test

import Data.Vector.Dense ( Vector, dim )
import Data.Matrix.Dense ( Matrix )
import Data.Matrix.Banded hiding ( banded )
import Data.Elem.BLAS ( Elem, BLAS3 )

banded :: (TestElem e) => 
    (Int,Int) -> (Int,Int) -> Gen (Banded e)
banded mn lu = Test.bandedWith lu mn

instance (TestElem e) => Arbitrary (Banded e) where
    arbitrary = do
        (m,n)   <- Test.shape
        (kl,ku) <- Test.bandwidths (m,n)
        banded (m,n) (kl,ku)

data BandedAt e = BandedAt (Banded e) (Int,Int) deriving (Eq, Show)
instance (TestElem e) => Arbitrary (BandedAt e) where
    arbitrary = sized $ \k ->
        let k' = ceiling (sqrt $ fromInteger $ toInteger k :: Double)
        in do
            m  <- choose (1,k'+1)
            n  <- choose (1,k'+1)
            kl <- choose (0,m-1)
            ku <- choose (0,n-1)
            i  <- choose (0,m-1)
            j  <- choose (0,n-1)
            a  <- banded (m,n) (kl,ku)
            
            return $ BandedAt a (i,j)

data ListsBanded e = ListsBanded !(Int,Int) !(Int,Int) ![[e]] deriving (Eq,Show)
instance (TestElem e) => Arbitrary (ListsBanded e) where
    arbitrary = do
        (m',n') <- Test.shape
        let (m,n) = (m'+1,n'+1)
        (kl,ku) <- Test.bandwidths (m,n)

        ds <- forM [(-kl)..ku] $ \i ->
                  let beginPad = max (-i)    0
                      endPad   = max (m-n+i) 0
                      len      = m - (beginPad+endPad)
                  in do
                      xs <- Test.elems len
                      return $ replicate beginPad 0 ++ xs ++ replicate endPad 0
         
        return $ ListsBanded (m,n) (kl,ku) ds

instance (TestElem e) => CoArbitrary (ListsBanded e) where
    coarbitrary (ListsBanded mn lu ds) = coarbitrary (mn,lu,ds)
   
{-
                    
data MatrixPair m n e = Pair (Matrix (m,n) e) (Matrix (m,n) e) deriving (Eq, Show)

instance (Arbitrary e, Elem e) => Arbitrary (MatrixPair m n e) where
    arbitrary = sized $ \k -> 
        let k' = ceiling (sqrt $ fromInteger $ toInteger k :: Double)
        in do
            m <- choose (0,k')
            n <- choose (0,k')
            a <- Test.matrix (m,n)
            b <- Test.matrix (m,n)
            return $ Pair a b
        
    coarbitrary = undefined
-}  
  
data BandedMV e = BandedMV (Banded e) (Vector e) deriving (Eq, Show)

instance (TestElem e) => Arbitrary (BandedMV e) where
    arbitrary = sized $ \k -> 
        let k' = ceiling (sqrt $ fromInteger $ toInteger k :: Double)
        in do
            m <- choose (0,k')
            n <- choose (0,k')
            kl <- if m == 0 then return 0 else choose (0,m-1)
            ku <- if n == 0 then return 0 else choose (0,n-1)
            a <- banded (m,n) (kl,ku)             
            x <- Test.vector n
            return $ BandedMV a x

data BandedMVPair e = BandedMVPair (Banded e) (Vector e) (Vector e) 
    deriving (Eq, Show)
    
instance (TestElem e) => Arbitrary (BandedMVPair e) where
    arbitrary = do
        (BandedMV a x) <- arbitrary
        y <- Test.vector (dim x)
        return $ BandedMVPair a x y
        
data BandedMM e = BandedMM (Banded e) (Matrix e) deriving (Eq, Show)

instance (TestElem e) => Arbitrary (BandedMM e) where
    arbitrary = sized $ \s ->
        let s' = ceiling (sqrt $ fromInteger $ toInteger s :: Double)
        in do
            m <- choose (0,s')
            k <- choose (0,s')
            n <- choose (0,s')
            kl <- if m == 0 then return 0 else choose (0,m-1)
            ku <- if k == 0 then return 0 else choose (0,k-1)
            a <- banded (m,k) (kl,ku)             
            b <- Test.matrix (k,n)
            return $ BandedMM a b
        
data BandedMMPair e = BandedMMPair (Banded e) (Matrix e) (Matrix e)
    deriving (Eq, Show)
    
instance (TestElem e) => Arbitrary (BandedMMPair e) where
    arbitrary = do
        (BandedMM a b) <- arbitrary
        c <- Test.matrix (shape b)
        return $ BandedMMPair a b c
