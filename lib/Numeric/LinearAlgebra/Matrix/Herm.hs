-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.LinearAlgebra.Matrix.Herm
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--
-- Hermitian views of matrices.
--

module Numeric.LinearAlgebra.Matrix.Herm (
    -- * Hermitian views of matrices
    Herm(..),
    withHerm,
    
    -- * Immutable interface
    
    -- ** Matrix-Vector multiplication
    mulHermMatrixVector,
    mulHermMatrixVectorWithScale,
    mulHermMatrixAddVectorWithScales,
    
    -- ** Matrix-Matrix  multiplication
    mulHermMatrixMatrix,
    mulHermMatrixMatrixWithScale,
    mulHermMatrixAddMatrixWithScales,

    -- ** Updates
    rank1UpdateHermMatrix,
    rank2UpdateHermMatrix,
    rankKUpdateHermMatrix,   
    rank2KUpdateHermMatrix,    

    
    -- * Mutable interface
    
    -- ** Matrix-Vector multiplication
    mulHermMatrixToVector,
    mulHermMatrixToVectorWithScale,
    mulHermMatrixAddToVectorWithScales,
    
    -- ** Matrix-Matrix multiplication
    mulHermMatrixToMatrix,
    mulHermMatrixToMatrixWithScale,
    mulHermMatrixAddToMatrixWithScales,

    -- ** Updates
    rank1UpdateToHermMatrix,
    rank2UpdateToHermMatrix,
    rankKUpdateToHermMatrix,  
    rank2KUpdateToHermMatrix,
    
    ) where

import Control.Monad.ST( ST, runST, unsafeIOToST )
import Text.Printf( printf )

import Numeric.LinearAlgebra.Vector.Base
import Numeric.LinearAlgebra.Vector.STBase
import Numeric.LinearAlgebra.Matrix.Base
import Numeric.LinearAlgebra.Matrix.STBase
import Numeric.LinearAlgebra.Types
import qualified Foreign.BLAS as BLAS

-- | A hermitian view of an underlying matrix.  The view can either be
-- of the upper or lower triangular part of the matrix.  The type arguments
-- are as follows:
--
--     * @m@: the underlyting matrix type.
--
--     * @e@: the element type of the matrix.
--
data Herm m e = Herm Uplo (m e) deriving (Show)

-- | Apply a function to the unerlying 'Uplo' and matrix.
withHerm :: Herm m e -> (Uplo -> m e -> a) -> a
withHerm (Herm u m) f = f u m


-- | @rank1UpdateToHermMatrix alpha x a@ returns
-- @alpha * x * x^H + a@.
rank1UpdateHermMatrix :: (BLAS2 e)
                      => e -> Vector e -> Herm Matrix e -> Herm Matrix e
rank1UpdateHermMatrix alpha x (Herm uplo a) = runST $ do
    ma' <- newCopyMatrix a
    rank1UpdateToHermMatrix alpha x (Herm uplo ma')
    a' <- unsafeFreezeMatrix ma'
    return $ Herm uplo a'

-- | @rank2UpdateToHermMatrix alpha x y a@ returns
-- @alpha * x * y^H + conj(alpha) * y * x^H + a@.
rank2UpdateHermMatrix :: (BLAS2 e)
                      => e -> Vector e -> Vector e -> Herm Matrix e
                      -> Herm Matrix e
rank2UpdateHermMatrix alpha x y (Herm uplo a) = runST $ do
    ma' <- newCopyMatrix a
    rank2UpdateToHermMatrix alpha x y (Herm uplo ma')
    a' <- unsafeFreezeMatrix ma'
    return $ Herm uplo a'

-- | @rankKUpdateHermMatrix alpha trans a beta c@ returns
-- @c := alpha * a * a^H + beta * c@ when @trans@ is @NoTrans@ and
-- @c := alpha * a^H * a + beta * c@ when @trans@ is @ConjTrans@.  The
-- function signals an error when @trans@ is @Trans@.
rankKUpdateHermMatrix :: (BLAS3 e)
                      => e -> Trans -> Matrix e -> e -> Herm Matrix e
                      -> Herm Matrix e
rankKUpdateHermMatrix alpha trans a beta (Herm uplo c) = runST $ do
    mc' <- newCopyMatrix c
    rankKUpdateToHermMatrix alpha trans a beta (Herm uplo mc')
    c' <- unsafeFreezeMatrix mc'
    return $ Herm uplo c'

-- | @rank2KUpdateHermMatrix alpha trans a b beta c@ returns
-- @c := alpha * a * b^H + conj(alpha) * b * a^H + beta * c@ when @trans@ is
-- @NoTrans@ and @c := alpha * b^H * a + conj(alpha) * a^H * b + beta * c@
-- when @trans@ is @ConjTrans@.  The function signals an error when @trans@
-- is @Trans@.
rank2KUpdateHermMatrix :: (BLAS3 e)
                       => e -> Trans -> Matrix e -> Matrix e -> e -> Herm Matrix e
                       -> Herm Matrix e
rank2KUpdateHermMatrix alpha trans a b beta (Herm uplo c) = runST $ do
    mc' <- newCopyMatrix c
    rank2KUpdateToHermMatrix alpha trans a b beta (Herm uplo mc')
    c' <- unsafeFreezeMatrix mc'
    return $ Herm uplo c'

-- | @rank1UpdateToHermMatrix alpha x a@ sets
-- @a := alpha * x * x^H + a@.
rank1UpdateToHermMatrix :: (RVector v, BLAS2 e)
                        => e -> v e -> Herm (STMatrix s) e -> ST s ()
rank1UpdateToHermMatrix alpha x (Herm uplo a)
    | (not . and) [ nx == n, (ma,na) == (n,n) ] = error $
        printf ("rank1UpdateToHermMatrix _ <vector with dim %d>"
                 ++ " (Herm _ <matrix with dim (%d,%d)>):"
                 ++ " invalid dimensions") nx ma na
    | otherwise =
        unsafeIOToST $
        unsafeWithVector x $ \px ->
        unsafeWithMatrix a $ \pa lda ->
            BLAS.her uplo n alpha px 1 pa lda
  where
    nx = dimVector x
    (ma,na) = dimMatrix a
    n = nx

-- | @rank2UpdateToHermMatrix alpha x y a@ sets
-- @a := alpha * x * y^H + conj(alpha) * y * x^H + a@.
rank2UpdateToHermMatrix :: (RVector v1, RVector v2, BLAS2 e)
                        => e -> v1 e -> v2 e -> Herm (STMatrix s) e -> ST s ()
rank2UpdateToHermMatrix alpha x y (Herm uplo a)
    | (not . and) [ nx == n, ny == n, (ma,na) == (n,n) ] = error $
        printf ("rank2UpdateToHermMatrix _ <vector with dim %d>"
                 ++ " <vector with dim %d>"
                 ++ " (Herm _ <matrix with dim (%d,%d)>):"
                 ++ " invalid dimensions") nx ny ma na
    | otherwise =
        unsafeIOToST $
        unsafeWithVector x $ \px ->
        unsafeWithVector x $ \py ->        
        unsafeWithMatrix a $ \pa lda ->
            BLAS.her2 uplo n alpha px 1 py 1 pa lda
  where
    nx = dimVector x
    ny = dimVector y
    (ma,na) = dimMatrix a
    n = nx

-- | @rankKUpdateToHermMatrix alpha trans a beta c@ sets
-- @c := alpha * a * a^H + beta * c@ when @trans@ is @NoTrans@ and
-- @c := alpha * a^H * a + beta * c@ when @trans@ is @ConjTrans@.  The
-- function signals an error when @trans@ is @Trans@.
rankKUpdateToHermMatrix :: (RMatrix m, BLAS3 e)
                        => e -> Trans -> m e -> e -> Herm (STMatrix s) e
                        -> ST s ()
rankKUpdateToHermMatrix alpha trans a beta (Herm uplo c)
    | trans == Trans = error $
        printf ("rankKUpdateToHermMatrix _ %s:"
                 ++ " trans argument must be NoTrans or ConjTrans")
               (show trans)
    | (not . and) [ (mc,nc) == (n,n)
                  , case trans of NoTrans -> (ma,na) == (n,k)
                                  _       -> (ma,na) == (k,n)
                  ] = error $
            printf ("rankKUpdateToHermMatrix _ %s <matrix with dim (%d,%d)> _"
                    ++ " (Herm _ <matrix with dim (%d,%d)>):"
                    ++ " invalid dimensions") (show trans) ma na mc nc
    | otherwise =
        unsafeIOToST $
        unsafeWithMatrix a $ \pa lda ->
        unsafeWithMatrix c $ \pc ldc ->
            BLAS.herk uplo trans n k alpha pa lda beta pc ldc
  where
    (ma,na) = dimMatrix a
    (mc,nc) = dimMatrix c
    (n,k) = if trans == NoTrans then (ma,na) else (na,ma)


-- | @rank2KUpdateToHermMatrix alpha trans a b beta c@ sets
-- @c := alpha * a * b^H + conj(alpha) * b * a^H + beta * c@ when @trans@ is
-- @NoTrans@ and @c := alpha * b^H * a + conj(alpha) * a^H * b + beta * c@
-- when @trans@ is @ConjTrans@.  The function signals an error when @trans@
-- is @Trans@.
rank2KUpdateToHermMatrix :: (RMatrix m1, RMatrix m2, BLAS3 e)
                         => e -> Trans -> m1 e -> m2 e -> e -> Herm (STMatrix s) e
                         -> ST s ()
rank2KUpdateToHermMatrix alpha trans a b beta (Herm uplo c)
    | trans == Trans = error $
        printf ("rank2KUpdateToHermMatrix _ %s:"
                 ++ " trans argument must be NoTrans or ConjTrans")
               (show trans)
    | (not . and) [ (mc,nc) == (n,n)
                  , (mb,nb) == (ma,na)
                  , case trans of NoTrans -> (ma,na) == (n,k)
                                  _       -> (ma,na) == (k,n)
                  ] = error $
            printf ("rank2KUpdateToHermMatrix _ %s <matrix with dim (%d,%d)>"
                    ++ " <matrix with dim (%d,%d)> _"
                    ++ " (Herm _ <matrix with dim (%d,%d)>):"
                    ++ " invalid dimensions") (show trans) ma na mb nb mc nc
    | otherwise =
        unsafeIOToST $
        unsafeWithMatrix a $ \pa lda ->
        unsafeWithMatrix b $ \pb ldb ->
        unsafeWithMatrix c $ \pc ldc ->
            BLAS.her2k uplo trans n k alpha pa lda pb ldb beta pc ldc
  where
    (ma,na) = dimMatrix a
    (mb,nb) = dimMatrix b
    (mc,nc) = dimMatrix c
    (n,k) = if trans == NoTrans then (ma,na) else (na,ma)


-- | @mulHermMatrixVector a x@ returns @a * x@.
mulHermMatrixVector :: (BLAS2 e)
                    => Herm Matrix e
                    -> Vector e
                    -> Vector e
mulHermMatrixVector a x =
    runVector $ do
        y <- newVector_ (dimVector x)
        mulHermMatrixToVector a x y
        return y

-- | @mulHermMatrixVectorWithScale alpha a x@ retunrs @alpha * a * x@.
mulHermMatrixVectorWithScale :: (BLAS2 e)
                             => e
                             -> Herm Matrix e
                             -> Vector e
                             -> Vector e
mulHermMatrixVectorWithScale alpha a x =
    runVector $ do
        y <- newVector_ (dimVector x)
        mulHermMatrixToVectorWithScale alpha a x y
        return y
                       
-- | @mulHermMatrixAddVectorWithScales alpha a x y@
-- returns @alpha * a * x + beta * y@.
mulHermMatrixAddVectorWithScales :: (BLAS2 e)
                                 => e
                                 -> Herm Matrix e
                                 -> Vector e
                                 -> e
                                 -> Vector e
                                 -> Vector e
mulHermMatrixAddVectorWithScales alpha a x beta y =
    runVector $ do
        y' <- newCopyVector y
        mulHermMatrixAddToVectorWithScales alpha a x beta y'
        return y'

-- | @mulHermMatrixMatrix side a b@
-- returns @alpha * a * b@ when @side@ is @LeftSide@ and
-- @alpha * b * a@ when @side@ is @RightSide@.
mulHermMatrixMatrix :: (BLAS3 e)
                    => Side -> Herm Matrix e
                    -> Matrix e
                    -> Matrix e
mulHermMatrixMatrix side a b = 
    runMatrix $ do
        c <- newMatrix_ (dimMatrix b)
        mulHermMatrixToMatrix side a b c
        return c

-- | @mulHermMatrixMatrixWithScale alpha side a b@
-- returns @alpha * a * b@ when @side@ is @LeftSide@ and
-- @alpha * b * a@ when @side@ is @RightSide@.
mulHermMatrixMatrixWithScale :: (BLAS3 e)
                             => e
                             -> Side -> Herm Matrix e
                             -> Matrix e
                             -> Matrix e
mulHermMatrixMatrixWithScale alpha side a b =
    runMatrix $ do
        c <- newMatrix_ (dimMatrix b)
        mulHermMatrixToMatrixWithScale alpha side a b c
        return c

-- | @mulHermMatrixAddMatrixWithScales alpha side a b beta c@
-- returns @alpha * a * b + beta * c@ when @side@ is @LeftSide@ and
-- @alpha * b * a + beta * c@ when @side@ is @RightSide@.
mulHermMatrixAddMatrixWithScales :: (BLAS3 e)
                                 => e
                                 -> Side -> Herm Matrix e
                                 -> Matrix e
                                 -> e
                                 -> Matrix e
                                 -> Matrix e
mulHermMatrixAddMatrixWithScales alpha side a b beta c = 
    runMatrix $ do
        c' <- newCopyMatrix c
        mulHermMatrixAddToMatrixWithScales alpha side a b beta c'
        return c'

-- | @mulHermMatrixToVector a x y@ sets @y := a * x@.
mulHermMatrixToVector :: (RMatrix m, RVector v, BLAS2 e)
                      => Herm m e
                      -> v e
                      -> STVector s e
                      -> ST s ()
mulHermMatrixToVector = mulHermMatrixToVectorWithScale 1

-- | @mulHermMatrixToVectorWithScale alpha a x y@
-- sets @y := alpha * a * x@.
mulHermMatrixToVectorWithScale :: (RMatrix m, RVector v, BLAS2 e)
                               => e
                               -> Herm m e
                               -> v e
                               -> STVector s e
                               -> ST s ()
mulHermMatrixToVectorWithScale alpha a x y =
    mulHermMatrixAddToVectorWithScales alpha a x 0 y

-- | @mulHermMatrixAddToVectorWithScales alpha a x beta y@
-- sets @y := alpha * a * x + beta * y@.
mulHermMatrixAddToVectorWithScales :: (RMatrix m, RVector v, BLAS2 e)
                                   => e
                                   -> Herm m e
                                   -> v e
                                   -> e
                                   -> STVector s e
                                   -> ST s ()
mulHermMatrixAddToVectorWithScales alpha (Herm uplo a) x beta y
    | ma /= na = error $
        printf ("mulHermMatrixAddToVectorWithScales _"
                ++ " (Herm %s <matrix with dim (%d,%d)>)"
                ++ " %s <vector with dim %d>"
                ++ " _"
                ++ " <vector with dim %d>: Herm matrix is not square")
               (show uplo) ma na
               nx ny
               
    | (not . and) [ (ma,na) == (n,n)
                  , nx == n
                  , ny == n
                  ] = error $
        printf ("mulHermMatrixAddToVectorWithScales _"
                ++ " (Herm %s <matrix with dim (%d,%d)>)"
                ++ " %s <vector with dim %d>"
                ++ " _"
                ++ " <vector with dim %d>: dimension mismatch")
               (show uplo) ma na
               nx ny

    | otherwise =
        unsafeIOToST $
            unsafeWithMatrix a $ \pa lda ->
            unsafeWithVector x $ \px ->
            unsafeWithVector y $ \py ->
                BLAS.hemv uplo n alpha pa lda px 1 beta py 1
  where
    (ma,na) = dimMatrix a
    nx = dimVector x
    ny = dimVector y
    n = ny

-- | @mulHermMatrixToMatrix side a b c@
-- sets @c := a * b@ when @side@ is @LeftSide@ and
-- @c := b * a@ when @side@ is @RightSide@.
mulHermMatrixToMatrix :: (RMatrix m1, RMatrix m2, BLAS3 e)
                      => Side -> Herm m1 e
                      -> m2 e
                      -> STMatrix s e
                      -> ST s ()
mulHermMatrixToMatrix = mulHermMatrixToMatrixWithScale 1

-- | @mulHermMatrixToMatrixWithScale alpha side a b c@
-- sets @c := alpha * a * b@ when @side@ is @LeftSide@ and
-- @c := alpha * b * a@ when @side@ is @RightSide@.
mulHermMatrixToMatrixWithScale :: (RMatrix m1, RMatrix m2, BLAS3 e)
                               => e
                               -> Side -> Herm m1 e
                               -> m2 e
                               -> STMatrix s e
                               -> ST s ()
mulHermMatrixToMatrixWithScale alpha side a b c =
    mulHermMatrixAddToMatrixWithScales alpha side a b 0 c

-- | @mulHermMatrixAddToMatrixWithScales alpha side a b beta c@
-- sets @c := alpha * a * b + beta * c@ when @side@ is @LeftSide@ and
-- @c := alpha * b * a + beta * c@ when @side@ is @RightSide@.
mulHermMatrixAddToMatrixWithScales :: (RMatrix m1, RMatrix m2, BLAS3 e)
                                   => e
                                   -> Side -> Herm m1 e
                                   -> m2 e
                                   -> e
                                   -> STMatrix s e
                                   -> ST s ()
mulHermMatrixAddToMatrixWithScales alpha side (Herm uplo a) b beta c
    | ma /= na = error $
        printf ("mulHermMatrixAddToMatrixWithScales _"
                ++ " %s (Herm %s <matrix with dim (%d,%d)>)" 
                ++ " <matrix with dim (%d,%d)>"
                ++ " _"
                ++ " <matrix with dim (%d,%d)>: Herm matrix is not square")
               (show side) (show uplo) ma na
               mb nb
               mc nc
    | (not . and) [ case side of LeftSide  -> (ma,na) == (m,m)
                                 RightSide -> (ma,na) == (n,n)
                  , (mb, nb ) == (m,n)
                  , (mc, nc ) == (m,n)
                  ] = error $
        printf ("mulHermMatrixAddToMatrixWithScales _"
                ++ " %s (Herm %s <matrix with dim (%d,%d)>)" 
                ++ " <matrix with dim (%d,%d)>"
                ++ " _"
                ++ " <matrix with dim (%d,%d)>: dimension mismatch")
               (show side) (show uplo) ma na
               mb nb
               mc nc
    | otherwise =
        unsafeIOToST $
            unsafeWithMatrix a $ \pa lda ->
            unsafeWithMatrix b $ \pb ldb ->
            unsafeWithMatrix c $ \pc ldc ->
                BLAS.hemm side uplo m n alpha pa lda pb ldb beta pc ldc
  where
    (ma,na) = dimMatrix a
    (mb,nb) = dimMatrix b
    (mc,nc) = dimMatrix c
    (m,n) = dimMatrix c
