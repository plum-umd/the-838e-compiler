#include <inttypes.h>
#include <gmp.h>
#include <stdlib.h>
#include <stdio.h>
#include "types.h"

void print_bignum(int64_t *h) {
  int64_t len = (h[0] >> int_shift);
  size_t abslen;
  mpz_t integ;

  if(len < 0) { // check if the sign is negative
    abslen = (size_t) (- len); // WARNING: potentially lossy conversion
  } else {
    abslen = (size_t) len;
  }

  mpz_init(integ); // initialize with value 0
  mpz_import(integ, abslen, -1, (size_t) 8, 0, 0, (void*) (h+1));

  if(len < 0) { // if sign is negative, negate integer
    mpz_neg(integ, integ);
  }
  
  mpz_out_str(stdout,10,integ);
  mpz_clear(integ);
}

int64_t bignum_length(int64_t *h) {
  int64_t len = (h[0] >> int_shift);
  int64_t abslen;
  mpz_t integ; // store value
  size_t sz;  // to store the length of bignum (return value)

  if(len < 0) { // check if the sign is negative
    abslen = (size_t) (- len); // WARNING: potentially lossy conversion? size_t = unsigned long (min 32 bits)
  } else {
    abslen = (size_t) len;
  }

  mpz_init(integ); // initialize with value 0
  mpz_import(integ, abslen, -1, (size_t) 8, 0, 0, (void*) (h+1));

  if(len < 0) { // if sign is negative, negate integer
    mpz_neg(integ, integ);
  }
  
  // Value is now fully loaded

  if(mpz_sgn(integ) == 0) {
    mpz_clear(integ);
    return 0;
  }
  else if(mpz_sgn(integ) > 0) {
    sz = mpz_sizeinbase (integ, 2);
    mpz_clear(integ);
    return (int64_t) ((int64_t) sz << int_shift);
  }
  else { // if negative, take absolute value, subtract one and take integer length
    mpz_abs(integ, integ);
    mpz_sub_ui(integ, integ, (unsigned long int) 1); 
    if(mpz_sgn(integ) == 0) {

      mpz_clear(integ);
      return 0;

    } else {

      sz = mpz_sizeinbase (integ, 2);
      mpz_clear(integ);
      return (int64_t) ((int64_t) sz << int_shift);

    }
  }
}


#ifdef CHECK
// $ gcc -DCHECK bignums.c -lgmp
#include <assert.h>

int main(void)
{

  return 0;
}
#endif
