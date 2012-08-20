{-# LANGUAGE CPP #-}
module Numeric.BLAS.Bindings.Trace where


-- | If @TRACE_BLAS_CALLS@ were defined during compilation prints name
--   BLAS call and values of parameters to stderr. Noop otherwise
#ifdef TRACE_BLAS_CALLS
traceBLAS :: Traceable f => String -> f -> f
traceBLAS msg f = traceStr (msg ++ ": ") $ traceWorker f

class Traceable f where
  traceWorker :: f -> f

instance (Show a, Traceable f) => Traceable (a -> f) where
  traceWorker f = \a -> traceStr (show a ++ " ") (f a)
instance Traceable (IO a) where
  traceWorker = id

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