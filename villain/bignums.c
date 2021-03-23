#include <inttypes.h>
#include <gmp.h>
#include <stdlib.h>
#include <stdio.h>
#include "types.h"

// Top of heap pointer
int64_t *heappointer = NULL;

// void mpz_init(mpz_t);
// void mpz_init_set_si(mpz_t, long int);
// void mpz_mul_2exp(mpz_t, mpz_t, mp_bitcnt_t);
// void mpz_add(mpz_t, mpz_t, mpz_t);
// void mpz_out_str(FILE *, int, mpz_t);

void print_bignum(int64_t *h) {
  int64_t len = (h[0] >> int_shift);
  int64_t abslen;
  int64_t i;
  mpz_t integ;

  if(len < 0) { // check if the sign is negative
    abslen = - len;
  } else {
    abslen = len;
  }

  mpz_init(integ); // initialize with value 0

  for(i = 1; i <= abslen ; i++) {
    unsigned long shifter = 64;
    int64_t j;
    unsigned long int t_raw = (unsigned long int) h[i];
    mpz_t t;
    mpz_init_set_ui(t, t_raw);

    for(j = 1; j < i; j++) {
      mpz_mul_2exp(t,t,(mp_bitcnt_t) shifter);
    }

    mpz_add(integ,integ,t);
    mpz_clear(t);
  }

  if(len < 0) { // if sign is negative, negate integer
    mpz_neg(integ, integ);
  }
  
  mpz_out_str(stdout,10,integ);

  mpz_clear(integ);
}

int64_t bignum_length(int64_t *h) {
  int64_t len = (h[0] >> int_shift);
  int64_t abslen;
  int64_t i; // scratch
  mpz_t integ; // store value
  size_t sz;  // to store the length of bignum (return value)

  if(len < 0) { // check if the sign is negative
    abslen = - len;
  } else {
    abslen = len;
  }

  mpz_init(integ); // initialize with value 0

  for(i = 1; i <= abslen ; i++) {
    unsigned long shifter = 64;
    int64_t j;
    unsigned long int t_raw = (unsigned long int) h[i];
    mpz_t t;
    mpz_init_set_ui(t, t_raw);

    for(j = 1; j < i; j++) {
      mpz_mul_2exp(t,t,(mp_bitcnt_t) shifter);
    }

    mpz_add(integ,integ,t);
    mpz_clear(t);
  }

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
