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
      
      if( val < 0 ) {
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

      if(mpz_sgn(integ) < 0) {
        ret = -ret;
      }

      mpz_clear(integ);

      return ret << int_shift;
    }
  }
}

int64_t integer_leq(int64_t val1, int64_t val2) { // rdi, rsi, rdx
  if (int_type_tag == (int_type_mask & val1) && int_type_tag == (int_type_mask & val2)) {
    // both values are fixnum
    return (val1 <= val2) ? val_true : val_false;
  } else if (bignum_type_tag == (ptr_type_mask & val1) && int_type_tag == (int_type_mask & val2)) {
    // val1 bignum, val2 fixnum
    int64_t* val1_point = (int64_t *) (val1 ^ bignum_type_tag);
    int64_t len = (val1_point[0] >> int_shift);
    int64_t abslen;
    mpz_t integ; // store value

    val2 = val2 >> int_shift; // adjust integer value

    if (len < 0) { // check if the sign is negative
      abslen = (size_t) (- len); // WARNING: potentially lossy conversion? size_t = unsigned long (min 32 bits)
    } else {
      abslen = (size_t) len;
    }

    mpz_init(integ); // initialize with value 0
    mpz_import(integ, abslen, -1, (size_t) 8, 0, 0, (void*) (val1_point+1));

    if(len < 0) { // if sign is negative, negate integer
      mpz_neg(integ, integ);
    }
    
    // Value is now fully loaded
    return (mpz_cmp_d (integ, (double) val2) <= 0) ? val_true : val_false;
  } else if ( int_type_tag == (int_type_mask & val1) && bignum_type_tag == (ptr_type_mask & val2)) {
    // val1 fixnum, val2 bignum
    int64_t* val2_point = (int64_t *) (val2 ^ bignum_type_tag);
    int64_t len = (val2_point[0] >> int_shift);
    int64_t abslen;
    mpz_t integ; // store value

    val1 = val1 >> int_shift; // adjust integer value

    if (len < 0) { // check if the sign is negative
      abslen = (size_t) (- len); // WARNING: potentially lossy conversion? size_t = unsigned long (min 32 bits)
    } else {
      abslen = (size_t) len;
    }

    mpz_init(integ); // initialize with value 0
    mpz_import(integ, abslen, -1, (size_t) 8, 0, 0, (void*) (val2_point+1));

    if(len < 0) { // if sign is negative, negate integer
      mpz_neg(integ, integ);
    }
    
    // Value is now fully loaded
    
    return (mpz_cmp_d (integ, (double) val1) >= 0) ? val_true : val_false;
  } else { 
    // both are bignum
    int64_t* val1_point = (int64_t *) (val1 ^ bignum_type_tag), *val2_point = (int64_t *) (val2 ^ bignum_type_tag);
    int64_t len1 = (val1_point[0] >> int_shift), len2 = (val2_point[0] >> int_shift);
    int64_t abslen1, abslen2;
    mpz_t integ1, integ2; // store value

    if (len1 < 0) { // check if the sign of first integer is negative
      abslen1 = (size_t) (- len1); // WARNING: potentially lossy conversion? size_t = unsigned long (min 32 bits)
    } else {
      abslen1 = (size_t) len1;
    }

    if (len2 < 0) { // check if the sign of second integer is negative
      abslen2 = (size_t) (- len2); // WARNING: potentially lossy conversion? size_t = unsigned long (min 32 bits)
    } else {
      abslen2 = (size_t) len2;
    }

    mpz_init(integ1); // initialize with value 0
    mpz_init(integ2); // initialize with value 0
    mpz_import(integ1, abslen1, -1, (size_t) 8, 0, 0, (void*) (val1_point+1));
    mpz_import(integ2, abslen2, -1, (size_t) 8, 0, 0, (void*) (val2_point+1));

    if(len1 < 0) { // if sign is negative, negate integer 1
      mpz_neg(integ1, integ1);
    }

    if(len2 < 0) { // if sign is negative, negate integer 2
      mpz_neg(integ2, integ2);
    }
    
    // Values are now fully loaded
    return (mpz_cmp(integ1, integ2) <= 0) ? val_true : val_false;
  }
}

int64_t integer_add(int64_t val1, int64_t val2, int64_t heap) { // rdi, rsi, rdx
  if (int_type_tag == (int_type_mask & val1) && int_type_tag == (int_type_mask & val2)) {
    // both values are fixnum
    int64_t ret = (val1 >> int_shift) + (val2 >> int_shift);

    if( ret < bound && ret >= -bound ) { // if value is in fixnum bounds, return value
      return ret << int_shift;
    } else { // out of bounds, build & export bignum
      int64_t* hp = (int64_t *) heap;
      
      if( ret < 0 ) {
        hp[0] = -1 << int_shift;
        hp[1] = -ret; // negate since we use absolute value representation, issue when heap overflows?
      } else {
        hp[0] = 1 << int_shift;
        hp[1] = ret; // issue when heap overflows?
      }

      return heap | bignum_type_tag;
    }
  } else if (bignum_type_tag == (ptr_type_mask & val1) && int_type_tag == (int_type_mask & val2)) {
    // val1 bignum, val2 fixnum
    int64_t* val1_point = (int64_t *) (val1 ^ bignum_type_tag);
    int64_t len = (val1_point[0] >> int_shift);
    int64_t abslen;
    mpz_t integ; // store value

    val2 = val2 >> int_shift; // adjust integer value

    if (len < 0) { // check if the sign is negative
      abslen = (size_t) (- len); // WARNING: potentially lossy conversion? size_t = unsigned long (min 32 bits)
    } else {
      abslen = (size_t) len;
    }

    mpz_init(integ); // initialize with value 0
    mpz_import(integ, abslen, -1, (size_t) 8, 0, 0, (void*) (val1_point+1));

    if(len < 0) { // if sign is negative, negate integer
      mpz_neg(integ, integ);
    }
    
    // Value is now fully loaded, add together
    if( val2 < 0 ) {
      mpz_sub_ui(integ, integ, (unsigned long int) (- val2));
    } else {
      mpz_add_ui(integ, integ, (unsigned long int) val2);
    }

    // return value as bignum or fixnum
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
      
      if(mpz_sgn(integ) < 0) {
        ret = -ret;
      }

      mpz_clear(integ);

      return ret << int_shift;
    }
  } else if ( int_type_tag == (int_type_mask & val1) && bignum_type_tag == (ptr_type_mask & val2)) {
    // val1 fixnum, val2 bignum
    int64_t* val2_point = (int64_t *) (val2 ^ bignum_type_tag);
    int64_t len = (val2_point[0] >> int_shift);
    int64_t abslen;
    mpz_t integ; // store value

    val1 = val1 >> int_shift; // adjust integer value

    if (len < 0) { // check if the sign is negative
      abslen = (size_t) (- len); // WARNING: potentially lossy conversion? size_t = unsigned long (min 32 bits)
    } else {
      abslen = (size_t) len;
    }

    mpz_init(integ); // initialize with value 0
    mpz_import(integ, abslen, -1, (size_t) 8, 0, 0, (void*) (val2_point+1));

    if(len < 0) { // if sign is negative, negate integer
      mpz_neg(integ, integ);
    }
    
    // Value is now fully loaded, add together        
    if( val1 < 0 ) {
      mpz_sub_ui(integ, integ, (unsigned long) (- val1));
    } else {
      mpz_add_ui(integ, integ, (unsigned long) val1);
    }

    // return value as bignum or fixnum
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

      if(mpz_sgn(integ) < 0) {
        ret = -ret;
      }

      mpz_clear(integ);

      return ret << int_shift;
    }
  } else { 
    // both are bignum
    int64_t* val1_point = (int64_t *) (val1 ^ bignum_type_tag), *val2_point = (int64_t *) (val2 ^ bignum_type_tag);
    int64_t len1 = (val1_point[0] >> int_shift), len2 = (val2_point[0] >> int_shift);
    int64_t abslen1, abslen2;
    mpz_t integ1, integ2; // store value

    if (len1 < 0) { // check if the sign of first integer is negative
      abslen1 = (size_t) (- len1); // WARNING: potentially lossy conversion? size_t = unsigned long (min 32 bits)
    } else {
      abslen1 = (size_t) len1;
    }

    if (len2 < 0) { // check if the sign of second integer is negative
      abslen2 = (size_t) (- len2); // WARNING: potentially lossy conversion? size_t = unsigned long (min 32 bits)
    } else {
      abslen2 = (size_t) len2;
    }

    mpz_init(integ1); // initialize with value 0
    mpz_init(integ2); // initialize with value 0
    mpz_import(integ1, abslen1, -1, (size_t) 8, 0, 0, (void*) (val1_point+1));
    mpz_import(integ2, abslen2, -1, (size_t) 8, 0, 0, (void*) (val2_point+1));

    if(len1 < 0) { // if sign is negative, negate integer 1
      mpz_neg(integ1, integ1);
    }

    if(len2 < 0) { // if sign is negative, negate integer 2
      mpz_neg(integ2, integ2);
    }
    
    // Values are now fully loaded
    mpz_add(integ1, integ1, integ2);

    // return value as bignum or fixnum
    if( mpz_cmp_d(integ1, (double) bound) >= 0 || mpz_cmp_d(integ1, (double) (- bound)) < 0) { // if out of range of fixnum, load into heap
      int64_t* hp = (int64_t *) heap;
      size_t temp = 0;

      mpz_export((void *) (hp + 1), &temp, -1, 8, 0, 0, integ1);

      if(mpz_sgn(integ1) >= 0) {
        hp[0] = (int64_t) (temp << int_shift);
      } else {
        hp[0] = (int64_t) (- temp << int_shift);
      }
      
      mpz_clear(integ1);
      mpz_clear(integ2);

      return heap | bignum_type_tag;
    } else { // in fixnum range, load into int64_t and return
      int64_t ret = 0;

      mpz_export((void *) &ret, NULL, 0, 8, 0, 0, integ1);

      if(mpz_sgn(integ1) < 0) {
        ret = -ret;
      }

      mpz_clear(integ1);
      mpz_clear(integ2);

      return ret << int_shift;
    }
  }
}

int64_t integer_sub(int64_t val1, int64_t val2, int64_t heap) { // rdi, rsi, rdx
  if (int_type_tag == (int_type_mask & val1) && int_type_tag == (int_type_mask & val2)) {
    // both values are fixnum
    int64_t ret = (val1 >> int_shift) - (val2 >> int_shift);

    if( ret < bound && ret >= -bound ) { // if value is in fixnum bounds, return value
      return ret << int_shift;
    } else { // out of bounds, build & export bignum
      int64_t* hp = (int64_t *) heap;
      
      if( ret < 0 ) {
        hp[0] = -1 << int_shift;
        hp[1] = -ret; // negate since we use absolute value representation, issue when heap overflows?
      } else {
        hp[0] = 1 << int_shift;
        hp[1] = ret; // issue when heap overflows?
      }

      return heap | bignum_type_tag;
    }
  } else if (bignum_type_tag == (ptr_type_mask & val1) && int_type_tag == (int_type_mask & val2)) {
    // val1 bignum, val2 fixnum
    int64_t* val1_point = (int64_t *) (val1 ^ bignum_type_tag);
    int64_t len = (val1_point[0] >> int_shift);
    int64_t abslen;
    mpz_t integ; // store value

    val2 = val2 >> int_shift; // adjust integer value

    if (len < 0) { // check if the sign is negative
      abslen = (size_t) (- len); // WARNING: potentially lossy conversion? size_t = unsigned long (min 32 bits)
    } else {
      abslen = (size_t) len;
    }

    mpz_init(integ); // initialize with value 0
    mpz_import(integ, abslen, -1, (size_t) 8, 0, 0, (void*) (val1_point+1));

    if(len < 0) { // if sign is negative, negate integer
      mpz_neg(integ, integ);
    }
    
    // Value is now fully loaded, subtract
    if( val2 < 0 ) {
      mpz_add_ui(integ, integ, (unsigned long int) (- val2));
    } else {
      mpz_sub_ui(integ, integ, (unsigned long int) val2);
    }

    // return value as bignum or fixnum
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

      if(mpz_sgn(integ) < 0) {
        ret = -ret;
      }

      mpz_clear(integ);

      return ret << int_shift;
    }
  } else if ( int_type_tag == (int_type_mask & val1) && bignum_type_tag == (ptr_type_mask & val2)) {
    // val1 fixnum, val2 bignum
    int64_t* val2_point = (int64_t *) (val2 ^ bignum_type_tag);
    int64_t len = (val2_point[0] >> int_shift);
    int64_t abslen;
    mpz_t integ; // store value

    val1 = val1 >> int_shift; // adjust integer value

    if (len < 0) { // check if the sign is negative
      abslen = (size_t) (- len); // WARNING: potentially lossy conversion? size_t = unsigned long (min 32 bits)
    } else {
      abslen = (size_t) len;
    }

    mpz_init(integ); // initialize with value 0
    mpz_import(integ, abslen, -1, (size_t) 8, 0, 0, (void*) (val2_point+1));

    if(len < 0) { // if sign is negative, negate integer
      mpz_neg(integ, integ);
    }
    
    // Value is now fully loaded, subtract together (we will do (- (- val2 val1)))
    if( val1 < 0 ) {
      mpz_add_ui(integ, integ, (unsigned long) (- val1));
    } else {
      mpz_sub_ui(integ, integ, (unsigned long) val1);
    }

    mpz_neg(integ, integ); // need to negate since we subtract in opposite direction

    // return value as bignum or fixnum
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
      

      if(mpz_sgn(integ) < 0) {
        ret = -ret;
      }

      mpz_clear(integ);

      return ret << int_shift;
    }
  } else { 
    // both are bignum
    int64_t* val1_point = (int64_t *) (val1 ^ bignum_type_tag), *val2_point = (int64_t *) (val2 ^ bignum_type_tag);
    int64_t len1 = (val1_point[0] >> int_shift), len2 = (val2_point[0] >> int_shift);
    int64_t abslen1, abslen2;
    mpz_t integ1, integ2; // store value

    if (len1 < 0) { // check if the sign of first integer is negative
      abslen1 = (size_t) (- len1); // WARNING: potentially lossy conversion? size_t = unsigned long (min 32 bits)
    } else {
      abslen1 = (size_t) len1;
    }

    if (len2 < 0) { // check if the sign of second integer is negative
      abslen2 = (size_t) (- len2); // WARNING: potentially lossy conversion? size_t = unsigned long (min 32 bits)
    } else {
      abslen2 = (size_t) len2;
    }

    mpz_init(integ1); // initialize with value 0
    mpz_init(integ2); // initialize with value 0
    mpz_import(integ1, abslen1, -1, (size_t) 8, 0, 0, (void*) (val1_point+1));
    mpz_import(integ2, abslen2, -1, (size_t) 8, 0, 0, (void*) (val2_point+1));

    if(len1 < 0) { // if sign is negative, negate integer 1
      mpz_neg(integ1, integ1);
    }

    if(len2 < 0) { // if sign is negative, negate integer 2
      mpz_neg(integ2, integ2);
    }
    
    // Values are now fully loaded
    mpz_sub(integ1, integ1, integ2);

    // return value as bignum or fixnum
    if( mpz_cmp_d(integ1, (double) bound) >= 0 || mpz_cmp_d(integ1, (double) (- bound)) < 0) { // if out of range of fixnum, load into heap
      int64_t* hp = (int64_t *) heap;
      size_t temp = 0;

      mpz_export((void *) (hp + 1), &temp, -1, 8, 0, 0, integ1);

      if(mpz_sgn(integ1) >= 0) {
        hp[0] = (int64_t) (temp << int_shift);
      } else {
        hp[0] = (int64_t) (- temp << int_shift);
      }
      
      mpz_clear(integ1);
      mpz_clear(integ2);

      return heap | bignum_type_tag;
    } else { // in fixnum range, load into int64_t and return
      int64_t ret = 0;

      mpz_export((void *) &ret, NULL, 0, 8, 0, 0, integ1);
      
      if(mpz_sgn(integ1) < 0) {
        ret = -ret;
      }

      mpz_clear(integ1);
      mpz_clear(integ2);

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
