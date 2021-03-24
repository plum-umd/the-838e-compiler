#include <inttypes.h>
#include <gmp.h>
#include <stdlib.h>
#include <stdio.h>
#include "types.h"

int64_t bound = (int64_t) ((int64_t) 1 << (63 - int_shift));

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

int64_t add_or_sub1(int64_t val, int64_t heap, int64_t delta) { // rdi, rsi, rdx
  // if our input is an integer
  if (int_type_tag == (int_type_mask & val)) {
    val = (val >> int_shift) + delta;

    if( val < bound && val >= -bound ) { // if value is in fixnum bounds, return value
      return val << int_shift;
    } else { // out of bounds, build & export bignum
      int64_t* hp = (int64_t *) heap;
      
      if( delta < 0 ) {
        hp[0] = -1 << int_shift;
        hp[1] = -val; // negate since we use absolute value representation, issue when heap overflows?
      } else {
        hp[0] = 1 << int_shift;
        hp[1] = val; // issue when heap overflows?
      }

      return heap | bignum_type_tag;
    }
  } else { // input is bignum
    int64_t* val_point = (int64_t *) (val ^ bignum_type_tag);
    int64_t len = (val_point[0] >> int_shift);
    int64_t abslen;
    mpz_t integ; // store value

    if (len < 0) { // check if the sign is negative
      abslen = (size_t) (- len); // WARNING: potentially lossy conversion? size_t = unsigned long (min 32 bits)
    } else {
      abslen = (size_t) len;
    }

    mpz_init(integ); // initialize with value 0
    mpz_import(integ, abslen, -1, (size_t) 8, 0, 0, (void*) (val_point+1));

    if(len < 0) { // if sign is negative, negate integer
      mpz_neg(integ, integ);
    }
    
    // Value is now fully loaded
    if(delta < 0) {
      mpz_sub_ui(integ, integ, (unsigned long) 1);
    } else {
      mpz_add_ui(integ, integ, (unsigned long) 1);
    }

    if( mpz_cmp_d(integ, (double) bound) >= 0 || mpz_cmp_d(integ, (double) (- bound)) < 0) { // if out of range of fixnum, load into heap
      int64_t* hp = (int64_t *) heap;
      size_t temp = 0;

      mpz_export((void *) (hp + 1), &temp, -1, 8, 0, 0, integ);

      if(mpz_sgn(integ) >= 0) {
        hp[0] = (int64_t) (temp << int_shift);
      } else {
        hp[0] = (int64_t) (- temp << int_shift);
      }
      
      mpz_clear(integ);

      return heap | bignum_type_tag;
    } else { // in fixnum range, load into int64_t and return
      int64_t ret = 0;

      mpz_export((void *) &ret, NULL, 0, 8, 0, 0, integ);
      mpz_clear(integ);

      return ret << int_shift;
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
