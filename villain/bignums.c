#include <inttypes.h>
#include <gmp.h>
#include <stdlib.h>
#include <stdio.h>
#include "types.h"

// Top of heap pointer
int64_t *heappointer = NULL;

void print_bignum(int64_t *h) {
  int64_t len = h[0];
  int i;
  mpz_t integ;

  mpz_init(integ); // initialize with value 0

  for(i = 1; i <= len ; i++) {
    unsigned long shifter = 64;
    int j;
    long int t_raw = (long int) h[i];
    mpz_t t;
    mpz_init_set_si(t, t_raw);

    for(j = 0; j < len - i; j++) {
      mpz_mul_2exp(t,t,(mp_bitcnt_t) shifter);
    }

    mpz_add(integ,integ,t);
  }
  
  mpz_out_str(stdout,10,integ);
}

// Used to trick GMP into placing its values on the top of the heap
void * allocate_function (size_t alloc_size) {
  return heappointer;
}

int64_t generate_bignum(int64_t *h) {

  int64_t len = h[0];
  int i;
  mpz_t integ;
  mpz_t ret;
  
  printf("%" PRId64, 8 * (len+1));
  printf("\n");
  printf("%p", h);
  printf("\n");
  
  heappointer = h + 8 * (len + 1); // set heap pointer so integ is initialized there
  printf("%p", heappointer);
  printf("\n");

  mp_set_memory_functions(NULL, NULL, NULL);

  mpz_init(integ); // initialize with value 0

  for(i = 1; i <= len ; i++) {
    unsigned long shifter = 64;
    int j;
    long int t_raw = (long int) h[i];
    mpz_t t;
    mpz_init_set_si(t, t_raw);

    for(j = 0; j < len - i; j++) {
      mpz_mul_2exp(t,t,(mp_bitcnt_t) shifter);
    }

    mpz_add(integ,integ,t);
  }
  
  // print_bignum(integ);
  printf("\n");
  
  mp_set_memory_functions(allocate_function, NULL, NULL); // tell GMP to store on the top of the heap
  mpz_init_set (ret, integ); // place number on the top of the stack

  printf("%p", &ret);
  printf("\n");
  printf("%" PRId64, sizeof(ret));
  printf("\n");

  // TODO: Free integ and t_raw ? 

  return sizeof(ret);

}




// int64_t *str_dup(const int64_t *s)
// {
//   int64_t len = (s[0] >> int_shift);
//   int n = (len % 3 == 0) ? len / 3 : (len / 3 + 1);
//   int64_t *d;

//   // TODO need a way to clean up memory
//   d = calloc(1+n, sizeof(int64_t));
//   if (!d)
//     return NULL;

//   return memcpy(d, s, (1+n) * sizeof(int64_t));
// }

// int64_t str_cmp(const int64_t *s1, const int64_t *s2)
// {
//   int64_t len1 = (s1[0] >> int_shift);
//   int64_t len2 = (s2[0] >> int_shift);
//   int64_t len = len1 < len2 ? len1 : len2;
//   int n = (len % 3 == 0) ? len / 3 : (len / 3 + 1);
//   int i;

//   for (i = 1; i <= n; ++i) {
//     if (s1[i] != s2[i])
//       return s1[i] - s2[i];
//   }
//   if (len1 == len2)
//     return 0;
//   else if (len1 > len2)
//     return s1[i+1];
//   else
//     return s2[i+1];
// }

// static int64_t char_to_bits (char c)
// {
//   return ((int64_t)c << char_shift) | char_type_tag;
// }

// int64_t *str_from_cstr(const char *s)
// {
//   int64_t len = strlen(s);
//   int n = (len % 3 == 0) ? len / 3 : (len / 3 + 1);
//   int64_t *vs = NULL;
//   int i, j, remain;
//   size_t pos = 0;

//   // TODO need a way to clean up memory
//   vs = calloc(1+n, sizeof(int64_t));
//   if (!vs)
//     return NULL;

//   vs[0] = len << int_shift;

//   // the inverse of print_str
//   for (i = 1; i < n; i++) {
//     for (j = 0; j < 3; j++) {
//       vs[i] |= char_to_bits(s[pos++]) << (j * 21);
//     }
//   }
//   remain = (len % 3 == 0) ? 3 : (len % 3);
//   for (j = 0; j < remain; j++){
//     vs[i] |= char_to_bits(s[pos++]) << (j * 21);
//   }
//   return vs;
// }

#ifdef CHECK
// $ gcc -DCHECK bignums.c -lgmp
#include <assert.h>

int main(void)
{

  return 0;
}
#endif
