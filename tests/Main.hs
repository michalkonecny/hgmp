{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Control.Monad (unless)
import System.Exit (exitFailure)
import Foreign
import Numeric.GMP.Types
import Numeric.GMP.Utils


foreign import ccall safe "__gmpz_init"
  mpz_init :: Ptr MPZ -> IO ()
foreign import ccall safe "__gmpz_clear"
  mpz_clear :: Ptr MPZ -> IO ()
foreign import ccall safe "__gmpz_mul"
  mpz_mul :: Ptr MPZ -> Ptr MPZ -> Ptr MPZ -> IO ()

foreign import ccall safe "__gmpq_init"
  mpq_init :: Ptr MPQ -> IO ()
foreign import ccall safe "__gmpq_clear"
  mpq_clear :: Ptr MPQ -> IO ()
foreign import ccall safe "__gmpq_mul"
  mpq_mul :: Ptr MPQ -> Ptr MPQ -> Ptr MPQ -> IO ()


-- instance Arbitrary Integer has small range
newtype Big = Big{ getBig :: Integer } deriving (Show)
instance Arbitrary Big where
  arbitrary = fmap Big $ choose (-bit 100, bit 100)
  shrink = fmap Big . shrinkIntegral . getBig


prop_IntegerWithPeek' n = ioProperty $ do
  m <- withInteger' n peekInteger'
  return (n == m)

prop_IntegerWithPeek n = ioProperty $ do
  m <- withInteger n peekInteger
  return (n == m)

prop_IntegerMultiply a b = ioProperty $ do
  withInteger a $ \az ->
    withInteger b $ \bz ->
      alloca $ \cz -> do
        mpz_init cz
        mpz_mul cz az bz
        c <- peekInteger cz
        mpz_clear cz
        return (a * b == c)


prop_BigIntegerWithPeek' (Big n) = ioProperty $ do
  m <- withInteger' n peekInteger'
  return (n == m)

prop_BigIntegerWithPeek (Big n) = ioProperty $ do
  m <- withInteger n peekInteger
  return (n == m)

prop_BigIntegerMultiply (Big a) (Big b) = ioProperty $ do
  withInteger a $ \az ->
    withInteger b $ \bz ->
      alloca $ \cz -> do
        mpz_init cz
        mpz_mul cz az bz
        c <- peekInteger cz
        mpz_clear cz
        return (a * b == c)
  

prop_RationalWithPeek' n = ioProperty $ do
  m <- withRational' n peekRational'
  return (n == m)

prop_RationalWithPeek n = ioProperty $ do
  m <- withRational n peekRational
  return (n == m)

prop_RationalMultiply a b = ioProperty $ do
  withRational a $ \aq ->
    withRational b $ \bq ->
      alloca $ \cq -> do
        mpq_init cq
        mpq_mul cq aq bq
        c <- peekRational cq
        mpq_clear cq
        return (a * b == c)


return []
main = do
  r <- $quickCheckAll
  unless r exitFailure
