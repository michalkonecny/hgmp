hgmp
====

Haskell interface to GMP.  Contains type definitions and marshalling functions,
to be able to write FFI bindings using Haskell's Integer and Rational types.
Function bindings may come in a future version.

A simple example illustrating binding to GMP's next probable-prime function:

    {-# LANGUAGE ForeignFunctionInterface #-}

    import Foreign.Ptr (Ptr(..))
    import Numeric.GMP.Types (MPZ)
    import Numeric.GMP.Utils (withInInteger, withOutInteger_)
    import System.IO.Unsafe (unsafePerformIO)

    foreign import ccall safe "__gmpz_nextprime"
      mpz_nextprime :: Ptr MPZ -> Ptr MPZ -> IO ()

    nextPrime :: Integer -> Integer
    nextPrime n =
      unsafePerformIO $
        withOutInteger_ $ \rop ->
          withInInteger n $ \op ->
            mpz_nextprime rop op
