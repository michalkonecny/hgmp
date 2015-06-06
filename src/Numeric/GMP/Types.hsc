#include <ghc-gmp.h>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Numeric.GMP.Types
  ( MPZ(..)
  , MPQ(..)
  , mpq_numref
  , mpq_denref
  , MPF(..)
  , MPRandState(..)
  , MPLimb(..)
  , MPLimbSigned(..)
  , MPSize(..)
  , MPExp(..)
  , MPBitCnt(..)
  ) where

import Data.Data
import Data.Typeable
import Data.Ix
import Data.Int
import Data.Word

import Foreign (Storable(..), Ptr, nullPtr, plusPtr)
import Foreign.C (CInt)

data MPZ = MPZ
  { mpzAlloc :: !CInt
  , mpzSize :: !CInt
  , mpzD :: !(Ptr MPLimb)
  }

instance Storable MPZ where
  sizeOf _ = (#size __mpz_struct)
  alignment _ = alignment nullPtr -- TODO verify
  peek ptr = do
    alloc <- (#peek __mpz_struct, _mp_alloc) ptr
    size <- (#peek __mpz_struct, _mp_size) ptr
    d <- (#peek __mpz_struct, _mp_d) ptr
    return (MPZ{ mpzAlloc = alloc, mpzSize = size, mpzD = d })
  poke ptr (MPZ{ mpzAlloc = alloc, mpzSize = size, mpzD = d }) = do
    (#poke __mpz_struct, _mp_alloc) ptr alloc
    (#poke __mpz_struct, _mp_size) ptr size
    (#poke __mpz_struct, _mp_d) ptr d

data MPQ = MPQ
  { mpqNum :: !MPZ
  , mpqDen :: !MPZ
  }

instance Storable MPQ where
  sizeOf _ = (#size __mpq_struct)
  alignment _ = alignment (undefined :: MPZ)
  peek ptr = do
    num <- (#peek __mpq_struct, _mp_num) ptr
    den <- (#peek __mpq_struct, _mp_den) ptr
    return (MPQ{ mpqNum = num, mpqDen = den })
  poke ptr (MPQ{ mpqNum = num, mpqDen = den }) = do
    (#poke __mpq_struct, _mp_num) ptr num
    (#poke __mpq_struct, _mp_den) ptr den

mpq_numref, mpq_denref :: Ptr MPQ -> Ptr MPZ
mpq_numref ptr = plusPtr ptr (#offset __mpq_struct, _mp_num)
mpq_denref ptr = plusPtr ptr (#offset __mpq_struct, _mp_den)

data MPF = MPF -- TODO
data MPRandState = MPRandState -- TODO

newtype MPLimb = MPLimb (#type mp_limb_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Data, Typeable, Storable)
newtype MPLimbSigned = MPLimbSigned (#type mp_limb_signed_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Data, Typeable, Storable)
newtype MPSize = MPSize (#type mp_size_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Data, Typeable, Storable)
newtype MPExp = MPExp (#type mp_exp_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Data, Typeable, Storable)
newtype MPBitCnt = MPBitCnt (#type mp_bitcnt_t)
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Num, Integral, Real, Ix, Data, Typeable, Storable)
