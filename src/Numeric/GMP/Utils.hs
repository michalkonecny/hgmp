{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- | GMP utilities.
module Numeric.GMP.Utils
  ( -- * Integer marshalling
    withInteger'
  , withInteger
  , peekInteger'
  , peekInteger
  , pokeInteger
  , withOutInteger
    -- * Rational marshalling
  , withRational'
  , withRational
  , peekRational'
  , peekRational
  , pokeRational
  , withOutRational
  ) where

import Control.Exception (bracket_)
import Data.Ratio ((%), numerator, denominator)
import Foreign (allocaBytes, alloca, with, sizeOf, peek)

import GHC.Integer.GMP.Internals
  ( Integer(..)
  , BigNat(..)
  , sizeofBigNat#
  , byteArrayToBigNat#
  , bigNatToInteger
  , bigNatToNegInteger
  )
import GHC.Prim
  ( ByteArray#
  , sizeofByteArray#
  , copyByteArrayToAddr#
  , newByteArray#
  , copyAddrToByteArray#
  , unsafeFreezeByteArray#
  )
import GHC.Exts (Int(..), Ptr(..))
import GHC.Types (IO(..))

import Numeric.GMP.Types

foreign import ccall unsafe "__gmpz_init"
  mpz_init :: Ptr MPZ -> IO ()

foreign import ccall unsafe "__gmpz_clear"
  mpz_clear :: Ptr MPZ -> IO ()

foreign import ccall unsafe "__gmpq_init"
  mpq_init :: Ptr MPQ -> IO ()

foreign import ccall unsafe "__gmpq_clear"
  mpq_clear :: Ptr MPQ -> IO ()

foreign import ccall unsafe "__gmpz_set"
  mpz_set :: Ptr MPZ -> Ptr MPZ -> IO ()

foreign import ccall unsafe "mpz_set_HsInt" -- implemented in wrappers.c
  mpz_set_HsInt :: Ptr MPZ -> Int -> IO ()


-- | Store an 'Integer' into a temporary 'MPZ'.  The action must use it only
--   as an @mpz_srcptr@ (ie, constant/immutable), and must not allow references
--   to it to escape its scope.
withInteger' :: Integer -> (MPZ -> IO r) -> IO r
withInteger' i action = case i of
  S# n# -> alloca $ \src -> do
    -- a bit awkward, TODO figure out how to do this without foreign calls
    mpz_init src
    mpz_set_HsInt src (I# n#)
    z <- peek src
    r <- action z
    mpz_clear src
    return r
  Jp# bn@(BN# ba#) -> withByteArray ba# $ \d _ -> action MPZ
        { mpzAlloc = 0
        , mpzSize = fromIntegral (I# (sizeofBigNat# bn))
        , mpzD = d
        }
  Jn# bn@(BN# ba#) -> withByteArray ba# $ \d _ -> action MPZ
        { mpzAlloc = 0
        , mpzSize = - fromIntegral (I# (sizeofBigNat# bn))
        , mpzD = d
        }

withByteArray :: ByteArray# -> (Ptr a -> Int -> IO r) -> IO r
withByteArray ba# f = do
  let bytes = I# (sizeofByteArray# ba#)
  allocaBytes bytes $ \ptr@(Ptr addr#) -> do
    IO (\s -> (# copyByteArrayToAddr# ba# 0# addr# (sizeofByteArray# ba#) s, () #))
    f ptr bytes


-- | Combination of 'withInteger'' and 'with'.
withInteger :: Integer -> (Ptr MPZ -> IO r) -> IO r
withInteger i action = withInteger' i $ \z -> with z action


-- | Store an 'Integer' into an @mpz_t@, which must have been initialized with
--   @mpz_init@.
pokeInteger :: Ptr MPZ -> Integer -> IO ()
pokeInteger dst (S# n#) = mpz_set_HsInt dst (I# n#)
-- copies twice, once in withInteger, and again in @mpz_set@.
-- could maybe rewrite to do one copy, using gmp's own alloc functions?
pokeInteger dst j = withInteger j $ mpz_set dst


-- | Read an 'Integer' from an 'MPZ'.
peekInteger' :: MPZ -> IO Integer
peekInteger' MPZ{ mpzSize = size, mpzD = d } = do
  if size == 0 then return 0 else
-- This copies once, from 'Ptr' 'MPLimb' to 'ByteArray#'
-- 'byteArrayToBigNat#' hopefully won't need to copy it again
    asByteArray d (fromIntegral (abs size) * sizeOf (undefined :: MPLimb))
      (\ba# -> return $ case fromIntegral (abs size) of
        I# size# -> (if size < 0 then bigNatToNegInteger else bigNatToInteger)
            (byteArrayToBigNat# ba# size#)
      )

asByteArray :: Ptr a -> Int -> (ByteArray# -> IO r) -> IO r
asByteArray (Ptr addr#) (I# bytes#) f = do
  IO $ \s# -> case newByteArray# bytes# s# of
    (# s'#, mba# #) ->
      case unsafeFreezeByteArray# mba# (copyAddrToByteArray# addr# mba# 0# bytes# s'#) of
        (# s''#, ba# #) -> case f ba# of IO r -> r s''#


-- | Combination of 'peek' and 'peekInteger''.
peekInteger :: Ptr MPZ -> IO Integer
peekInteger src = do
  z <- peek src
  peekInteger' z

-- | Allocates and initializes an @mpz_t@, then peeks and clears it after the
--   action.
withOutInteger :: (Ptr MPZ -> IO a) -> IO (Integer, a)
withOutInteger f = alloca $ \ptr -> bracket_ (mpz_init ptr) (mpz_clear ptr) $ do
  a <- f ptr
  z <- peekInteger ptr
  return (z, a)


-- | Store a 'Rational' into a temporary 'MPQ'.  The action must use it only
--   as an @mpq_srcptr@ (ie, constant/immutable), and must not allow references
--   to it to escape its scope.
withRational' :: Rational -> (MPQ -> IO r) -> IO r
withRational' q action =
  withInteger' (numerator q) $ \nz ->
  withInteger' (denominator q) $ \dz ->
  action (MPQ nz dz)


-- | Combination of 'withRational'' and 'with'.
withRational :: Rational -> (Ptr MPQ -> IO r) -> IO r
withRational q action = withRational' q $ \qq -> with qq action


-- | Store a 'Rational' into an @mpq_t@, which must have been initialized with
--   @mpq_init@.
pokeRational :: Ptr MPQ -> Rational -> IO ()
pokeRational ptr q = do
  pokeInteger (mpq_numref ptr) (numerator q)
  pokeInteger (mpq_denref ptr) (denominator q)


-- | Read a 'Rational' from an 'MPQ'.
peekRational' :: MPQ -> IO Rational
peekRational' (MPQ n d) = do
  num <- peekInteger' n
  den <- peekInteger' d
  return (num % den)


-- | Combination of 'peek' and 'peekRational''.
peekRational :: Ptr MPQ -> IO Rational
peekRational src = do
  q <- peek src
  peekRational' q


-- | Allocates and initializes an @mpq_t@, then peeks and clears it after the
--   action.
withOutRational :: (Ptr MPQ -> IO a) -> IO (Rational, a)
withOutRational f = alloca $ \ptr -> bracket_ (mpq_init ptr) (mpq_clear ptr) $ do
  a <- f ptr
  q <- peekRational ptr
  return (q, a)
