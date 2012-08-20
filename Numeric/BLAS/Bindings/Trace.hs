{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Numeric.BLAS.Bindings.Trace where

#ifdef TRACE_BLAS_CALLS
import System.IO
import System.IO.Unsafe
#endif



-- | If @TRACE_BLAS_CALLS@ were defined during compilation prints name
--   BLAS call and values of parameters to stderr. Noop otherwise
#ifdef TRACE_BLAS_CALLS
traceBLAS :: (Traceable b, Show a) => String -> (a -> b) -> (a -> b)
{-# NOINLINE traceBLAS #-}
traceBLAS msg f x = traceStr (msg ++ ": ") $ traceWorker (f x)

class Traceable f where
  traceWorker :: f -> f

instance (Show a, Traceable f) => Traceable (a -> f) where
  traceWorker f = \a -> traceStr (show a ++ "  ") $ traceWorker (f a)
  {-# NOINLINE traceWorker #-}
instance Traceable (IO a) where
  traceWorker x = traceStr "\n" x
  {-# NOINLINE traceWorker #-}

-- Same as @trace@ but do not adds trailing @\\n@
traceStr :: String -> a -> a
traceStr msg x = unsafePerformIO $ do
  hPutStr stderr msg
  return x


#else
traceBLAS :: String -> a -> a
{-# INLINE traceBLAS #-}
traceBLAS _ = id
#endif