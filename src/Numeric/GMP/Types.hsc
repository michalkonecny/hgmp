#include <ghc-gmp.h>

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | GMP types.
module Numeric.GMP.Types where

import Data.Data
import Data.Typeable
import Data.Bits
import Data.Ix
import Data.Int
import Data.Word

import Foreign (Storable(..), Ptr, nullPtr, plusPtr)
import Foreign.C (CInt)

-- | @mpz_t@
data MPZ = MPZ
  { mpzAlloc :: !CInt
  , mpzSize :: !CInt
  , mpzD :: !(Ptr MPLimb)
  }

instance Storable MPZ where
  sizeOf _ = (#size __mpz_struct)
  alignment _ = (#alignment __mpz_struct)
  peek ptr = do
    alloc <- (#peek __mpz_struct, _mp_alloc) ptr
    size <- (#peek __mpz_struct, _mp_size) ptr
    d <- (#peek __mpz_struct, _mp_d) ptr
    return (MPZ{ mpzAlloc = alloc, mpzSize = size, mpzD = d })
  poke ptr (MPZ{ mpzAlloc = alloc, mpzSize = size, mpzD = d }) = do
    (#poke __mpz_struct, _mp_alloc) ptr alloc
    (#poke __mpz_struct, _mp_size) ptr size
    (#poke __mpz_struct, _mp_d) ptr d

-- | @mpq_t@
data MPQ = MPQ
  { mpqNum :: !MPZ
  , mpqDen :: !MPZ
  }

instance Storable MPQ where
  sizeOf _ = (#size __mpq_struct)
  alignment _ = (#alignment __mpq_struct)
  peek ptr = do
    num <- (#peek __mpq_struct, _mp_num) ptr
    den <- (#peek __mpq_struct, _mp_den) ptr
    return (MPQ{ mpqNum = num, mpqDen = den })
  poke ptr (MPQ{ mpqNum = num, mpqDen = den }) = do
    (#poke __mpq_struct, _mp_num) ptr num
    (#poke __mpq_struct, _mp_den) ptr den

-- | Get pointers to numerator and denominator (these are macros in the C API).
mpq_numref, mpq_denref :: Ptr MPQ -> Ptr MPZ
mpq_numref ptr = plusPtr ptr (#offset __mpq_struct, _mp_num)
mpq_denref ptr = plusPtr ptr (#offset __mpq_struct, _mp_den)

-- | @mpf_t@
data MPF = MPF
  { mpfPrec :: !CInt
  , mpfSize :: !CInt
  , mpfExp :: !MPExp
  , mpfD :: !(Ptr MPLimb)
  }

instance Storable MPF where
  sizeOf _ = (#size __mpf_struct)
  alignment _ = (#alignment __mpf_struct)
  peek ptr = do
    prec <- (#peek __mpf_struct, _mp_prec) ptr
    size <- (#peek __mpf_struct, _mp_size) ptr
    expo <- (#peek __mpf_struct, _mp_exp) ptr
    d <- (#peek __mpf_struct, _mp_d) ptr
    return (MPF{ mpfPrec = prec, mpfSize = size, mpfExp = expo, mpfD = d })
  poke ptr (MPF{ mpfPrec = prec, mpfSize = size, mpfExp = expo, mpfD = d }) = do
    (#poke __mpf_struct, _mp_prec) ptr prec
    (#poke __mpf_struct, _mp_size) ptr size
    (#poke __mpf_struct, _mp_exp) ptr expo
    (#poke __mpf_struct, _mp_d) ptr d

-- | @gmp_randstate_t@
data GMPRandState = GMPRandState
  { gmprsSeed :: !MPZ
  , gmprsAlg :: !GMPRandAlg
  , gmprsAlgData :: !(Ptr ())
  }

instance Storable GMPRandState where
  sizeOf _ = (#size __gmp_randstate_struct)
  alignment _ = (#alignment __gmp_randstate_struct)
  peek ptr = do
    seed <- (#peek __gmp_randstate_struct, _mp_seed) ptr
    alg <- (#peek __gmp_randstate_struct, _mp_alg) ptr
    algdata <- (#peek __gmp_randstate_struct, _mp_algdata._mp_lc) ptr
    return (GMPRandState{ gmprsSeed = seed, gmprsAlg = alg, gmprsAlgData = algdata })
  poke ptr (GMPRandState{ gmprsSeed = seed, gmprsAlg = alg, gmprsAlgData = algdata }) = do
    (#poke __gmp_randstate_struct, _mp_seed) ptr seed
    (#poke __gmp_randstate_struct, _mp_alg) ptr alg
    (#poke __gmp_randstate_struct, _mp_algdata) ptr algdata

-- | @mp_limb_t@
newtype MPLimb = MPLimb (#type mp_limb_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mp_limb_signed_t@
newtype MPLimbSigned = MPLimbSigned (#type mp_limb_signed_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mp_size_t@
newtype MPSize = MPSize (#type mp_size_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mp_exp_t@
newtype MPExp = MPExp (#type mp_exp_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @mp_bitcnt_t@
newtype MPBitCnt = MPBitCnt (#type mp_bitcnt_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)

-- | @gmp_randalg_t@
newtype GMPRandAlg = GMPRandAlg (#type gmp_randalg_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Bits, FiniteBits, Data, Typeable, Storable)
