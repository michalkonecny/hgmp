{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import Foreign.Ptr (Ptr(..))
import Numeric.GMP.Types (MPZ)
import Numeric.GMP.Utils (withInInteger, withOutInteger_)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall safe "__gmpz_nextprime"
  mpz_nextprime :: Ptr MPZ -> Ptr MPZ -> IO ()

nextPrimeIO :: Integer -> IO Integer
nextPrimeIO n = do
  withOutInteger_ $ \rop ->
    withInInteger n $ \op ->
      mpz_nextprime rop op

nextPrime :: Integer -> Integer
nextPrime n = unsafePerformIO $ nextPrimeIO n

primes :: Integer -> [Integer]
primes = drop 1 . iterate nextPrime

main :: IO ()
main = do
  [sn] <- getArgs
  n <- readIO sn
  mapM_ print . take 10 . primes $ n
