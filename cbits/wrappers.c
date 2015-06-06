#include <assert.h>
#include <HsFFI.h>
#include <ghc-gmp.h>

void mpz_set_HsInt(mpz_ptr dst, const HsInt n) {
  if (sizeof(HsInt) == sizeof(signed long int)) {
    mpz_set_si(dst, n);
  } else if (sizeof(HsInt) == sizeof(signed long int) + sizeof(unsigned long int)) {
    // Win64, see comments in integer-gmp/src/GHC/Integer/Type.hs
#define lobits (8 * sizeof(unsigned long int))
#define lomask (lobits - 1)
    mpz_set_si(dst, n >> lobits); // warns when branch will be unused
    mpz_mul_2exp(dst, dst, lobits);
    mpz_add_ui(dst, dst, n & lomask);
  } else {
    assert(! "supported HsInt size");
  }
}
