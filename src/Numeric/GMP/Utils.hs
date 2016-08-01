{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- | GMP utilities.  A simple example with probable primes:
--
-- > foreign import ccall safe "__gmpz_nextprime"
-- >   mpz_nextprime :: Ptr MPZ -> Ptr MPZ -> IO ()
-- >
-- > nextPrime :: Integer -> IO Integer
-- > nextPrime n =
-- >   withOutInteger_ $ \rop ->
-- >     withInInteger n $ \op ->
-- >       mpz_nextprime rop op
module Numeric.GMP.Utils
  ( -- * Integer marshalling
    withInInteger'
  , withInInteger
  , withInOutInteger
  , withInOutInteger_
  , withOutInteger
  , withOutInteger_
  , peekInteger'
  , peekInteger
  , pokeInteger
    -- * Rational marshalling
  , withInRational'
  , withInRational
  , withInOutRational
  , withInOutRational_
  , withOutRational
  , withOutRational_
  , peekRational'
  , peekRational
  , pokeRational
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
withInInteger' :: Integer -> (MPZ -> IO r) -> IO r
withInInteger' i action = case i of
  S# n# -> alloca $ \src -> bracket_ (mpz_init src) (mpz_clear src) $ do
    -- a bit awkward, TODO figure out how to do this without foreign calls?
    mpz_set_HsInt src (I# n#)
    z <- peek src
    r <- action z
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


-- | Combination of 'withInInteger'' and 'with'.  The action must use it only
--   as an @mpz_srcptr@ (ie, constant/immutable), and must not allow the pointer
--   to escape its scope.  If in doubt about potential mutation by the action,
--   use 'withInOutInteger' instead.
withInInteger :: Integer -> (Ptr MPZ -> IO r) -> IO r
withInInteger i action = withInInteger' i $ \z -> with z action


-- | Allocates and initializes an @mpz_t@, pokes the value, and peeks and clears
--   it after the action.  The pointer must not escape the scope of the action.
withInOutInteger :: Integer -> (Ptr MPZ -> IO a) -> IO (Integer, a)
withInOutInteger n action = withOutInteger $ \z -> do
  pokeInteger z n
  action z


-- | Allocates and initializes an @mpz_t@, pokes the value, and peeks and clears
--   it after the action.  The pointer must not escape the scope of the action.
--   The result of the action is discarded.
withInOutInteger_ :: Integer -> (Ptr MPZ -> IO a) -> IO Integer
withInOutInteger_ n action = do
  (z, _) <- withInOutInteger n action
  return z


-- | Allocates and initializes an @mpz_t@, then peeks and clears it after the
--   action.  The pointer must not escape the scope of the action.
withOutInteger :: (Ptr MPZ -> IO a) -> IO (Integer, a)
withOutInteger action = alloca $ \ptr ->
  bracket_ (mpz_init ptr) (mpz_clear ptr) $ do
    a <- action ptr
    z <- peekInteger ptr
    return (z, a)


-- | Allocates and initializes an @mpz_t@, then peeks and clears it after the
--   action.  The pointer must not escape the scope of the action.  The result
--   of the action is discarded.
withOutInteger_ :: (Ptr MPZ -> IO a) -> IO Integer
withOutInteger_ action = do
  (z, _) <- withOutInteger action
  return z


-- | Store an 'Integer' into an @mpz_t@, which must have been initialized with
--   @mpz_init@.
pokeInteger :: Ptr MPZ -> Integer -> IO ()
pokeInteger dst (S# n#) = mpz_set_HsInt dst (I# n#)
-- copies twice, once in withInteger, and again in @mpz_set@.
-- could maybe rewrite to do one copy, using gmp's own alloc functions?
pokeInteger dst j = withInInteger j $ mpz_set dst


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


-- | Store a 'Rational' into a temporary 'MPQ'.  The action must use it only
--   as an @mpq_srcptr@ (ie, constant/immutable), and must not allow the pointer
--   to escape its scope.
withInRational' :: Rational -> (MPQ -> IO r) -> IO r
withInRational' q action =
  withInInteger' (numerator q) $ \nz ->
  withInInteger' (denominator q) $ \dz ->
  action (MPQ nz dz)


-- | Combination of 'withInRational'' and 'with'.  The action must use it only
--   as an @mpq_srcptr@ (ie, constant/immutable), and must not allow the pointer
--   to escape its scope.  If in doubt about potential mutation by the action,
--   use 'withInOutRational' instead.
withInRational :: Rational -> (Ptr MPQ -> IO r) -> IO r
withInRational q action = withInRational' q $ \qq -> with qq action


-- | Allocates and initializes an @mpq_t@, pokes the value, and peeks and clears
--   it after the action.  The pointer must not escaep the scope of the action.
withInOutRational :: Rational -> (Ptr MPQ -> IO a) -> IO (Rational, a)
withInOutRational n action = withOutRational $ \q -> do
  pokeRational q n
  action q


-- | Allocates and initializes an @mpq_t@, pokes the value, and peeks and clears
--   it after the action.  The pointer must not escaep the scope of the action.
--   The result of the action is discarded.
withInOutRational_ :: Rational -> (Ptr MPQ -> IO a) -> IO Rational
withInOutRational_ n action = do
  (q, _) <- withInOutRational n action
  return q


-- | Allocates and initializes an @mpq_t@, then peeks and clears it after the
--   action.  The pointer must not escape the scope of the action.
withOutRational :: (Ptr MPQ -> IO a) -> IO (Rational, a)
withOutRational action = alloca $ \ptr ->
  bracket_ (mpq_init ptr) (mpq_clear ptr) $ do
    a <- action ptr
    q <- peekRational ptr
    return (q, a)


-- | Allocates and initializes an @mpq_t@, then peeks and clears it after the
--   action.  The pointer must not escape the scope of the action.  The result
--   of the action is discarded.
withOutRational_ :: (Ptr MPQ -> IO a) -> IO Rational
withOutRational_ action = do
  (q, _) <- withOutRational action
  return q


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
