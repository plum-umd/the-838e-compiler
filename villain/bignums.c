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
  int i;
  mpz_t integ;

  // printf("Bignum: ");

  if(len < 0) { // if sign is negative, negate length and print negative sign
    len = - len;
    printf("-");
  }

  mpz_init(integ); // initialize with value 0

  for(i = 1; i <= len ; i++) {
    unsigned long shifter = 64;
    int j;
    unsigned long int t_raw = (unsigned long int) h[i];
    mpz_t t;
    mpz_init_set_ui(t, t_raw);

    for(j = 1; j < i; j++) {
      mpz_mul_2exp(t,t,(mp_bitcnt_t) shifter);
    }

    mpz_add(integ,integ,t);
    mpz_clear(t);
  }
  
  mpz_out_str(stdout,10,integ);

  mpz_clear(integ);
}


#ifdef CHECK
// $ gcc -DCHECK bignums.c -lgmp
#include <assert.h>

int main(void)
{

  return 0;
}
#endif
