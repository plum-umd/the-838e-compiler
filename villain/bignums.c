#include <inttypes.h>
#include <gmp.h>
#include <stdlib.h>
#include <stdio.h>
#include "types.h"
#include "villain.h"

void load_bignum(mpz_t, int64_t*);
void load_any_to_bignum(mpz_t, int64_t);
int64_t integer_comparison(int64_t, int64_t, int);
int64_t return_bignum_maybe_fixnum(mpz_t, int64_t);
int64_t return_fixnum_maybe_bignum(int64_t, int64_t);

int64_t bound = (int64_t) ((int64_t) 1 << (63 - int_shift));

void print_bignum(vl_bignum* h) {
  mpz_t integ;
  mpz_init(integ); 
  load_bignum(integ, (int64_t *) h);
  mpz_out_str(stdout,10,integ);
  mpz_clear(integ);
}

int64_t bignum_length(int64_t *h) {
  mpz_t integ;
  mpz_init(integ); 
  load_bignum(integ, h); // we already xor'ed out bignum tag
  size_t sz;  // to store the length of bignum (return value)

  // Note: We ignore when sign is near 0 since bignums will not be in that range.
  if(mpz_sgn(integ) > 0) {

    sz = mpz_sizeinbase (integ, 2);
    mpz_clear(integ);
    return (int64_t) ((int64_t) sz << int_shift);

  } else { // if negative, take absolute value, subtract one and take integer length

    mpz_abs(integ, integ);
    mpz_sub_ui(integ, integ, (unsigned long int) 1); 

    sz = mpz_sizeinbase(integ, 2);
    mpz_clear(integ);
    return (int64_t) ((int64_t) sz << int_shift);

  }
}

int64_t add_or_sub1(int64_t val, int64_t heap, int64_t delta) { // rdi, rsi, rdx
  // if our input is an integer
  if (int_type_tag == (int_type_mask & val)) {

    return return_fixnum_maybe_bignum((val >> int_shift) + delta, heap);

  } else { // input is bignum
    int64_t ret;
    mpz_t integ;
    mpz_init(integ); 
    load_bignum(integ, (int64_t *) (val ^ bignum_type_tag)); // load value
    
    // Value is now fully loaded
    if(delta < 0) {
      mpz_sub_ui(integ, integ, (unsigned long) 1);
    } else {
      mpz_add_ui(integ, integ, (unsigned long) 1);
    }

    ret = return_bignum_maybe_fixnum(integ, heap);

    mpz_clear(integ);
    return ret;
  }
}

int64_t integer_g(int64_t val1, int64_t val2) { // rdi, rsi
  return integer_comparison(val1, val2, 1);
}

int64_t integer_geq(int64_t val1, int64_t val2) { // rdi, rsi
  return integer_comparison(val1, val2, 2);
}

int64_t integer_leq(int64_t val1, int64_t val2) { // rdi, rsi
  return integer_comparison(val1, val2, 3);
}

int64_t integer_l(int64_t val1, int64_t val2) { // rdi, rsi
  return integer_comparison(val1, val2, 4);
}

// comparison: 1 for val1 > val2, 2 for val1 >= val2, 3 for val1 <= val2, 4 for val1 < val2
int64_t integer_comparison(int64_t val1, int64_t val2, int comparison) {

  if (int_type_tag == (int_type_mask & val1) && int_type_tag == (int_type_mask & val2)) {
    // both values are fixnum
    switch(comparison){
      case 1:
        return (val1 > val2) ? val_true : val_false;
        break;
      case 2:
        return (val1 >= val2) ? val_true : val_false;
        break;
      case 3:
        return (val1 <= val2) ? val_true : val_false;
        break;
      default:
        return (val1 < val2) ? val_true : val_false;
        break;
    }
  } else if (bignum_type_tag == (ptr_type_mask & val1) && int_type_tag == (int_type_mask & val2)) {
    // val1 bignum, val2 fixnum

    int64_t ret;
    mpz_t integ;
    mpz_init(integ);

    load_bignum(integ, (int64_t *) (val1 ^ bignum_type_tag)); // load value
    val2 = val2 >> int_shift; // adjust integer value

    switch(comparison){
      case 1:
        ret = (mpz_cmp_d (integ, (double) val2) > 0) ? val_true : val_false;
        break;
      case 2:
        ret = (mpz_cmp_d (integ, (double) val2) >= 0) ? val_true : val_false;
        break;
      case 3:
        ret = (mpz_cmp_d (integ, (double) val2) <= 0) ? val_true : val_false;
        break;
      default:
        ret = (mpz_cmp_d (integ, (double) val2) < 0) ? val_true : val_false;
        break;
    }

    mpz_clear(integ);
    return ret;

  } else if ( int_type_tag == (int_type_mask & val1) && bignum_type_tag == (ptr_type_mask & val2)) {
    // val1 fixnum, val2 bignum
    
    int64_t ret;
    mpz_t integ;
    mpz_init(integ); 

    load_bignum(integ, (int64_t *) (val2 ^ bignum_type_tag)); // load value
    val1 = val1 >> int_shift; // adjust integer value
    
    // everything is flipped since we have val2 on the left side
    switch(comparison){
      case 1:
        ret = (mpz_cmp_d (integ, (double) val1) < 0) ? val_true : val_false;
        break;
      case 2:
        ret = (mpz_cmp_d (integ, (double) val1) <= 0) ? val_true : val_false;
        break;
      case 3:
        ret = (mpz_cmp_d (integ, (double) val1) >= 0) ? val_true : val_false;
        break;
      default:
        ret = (mpz_cmp_d (integ, (double) val1) > 0) ? val_true : val_false;
        break;
    }

    mpz_clear(integ);
    return ret;

  } else { 
    // both are bignum
    
    int64_t ret;
    mpz_t integ1, integ2;
    mpz_init(integ1); mpz_init(integ2);

    load_bignum(integ1, (int64_t *) (val1 ^ bignum_type_tag)); // load value
    load_bignum(integ2, (int64_t *) (val2 ^ bignum_type_tag)); // load value
    
    switch(comparison){
      case 1:
        ret = (mpz_cmp(integ1, integ2) > 0) ? val_true : val_false;
        break;
      case 2:
        ret = (mpz_cmp(integ1, integ2) >= 0) ? val_true : val_false;
        break;
      case 3:
        ret = (mpz_cmp(integ1, integ2) <= 0) ? val_true : val_false;
        break;
      default:
        ret = (mpz_cmp(integ1, integ2) < 0) ? val_true : val_false;
        break;
    }

    mpz_clear(integ1);
    mpz_clear(integ2);
    return ret;

  }
}

int64_t integer_add(int64_t val1, int64_t val2, int64_t heap) { // rdi, rsi, rdx
  if (int_type_tag == (int_type_mask & val1) && int_type_tag == (int_type_mask & val2)) {
    // both values are fixnum

    return return_fixnum_maybe_bignum((val1 >> int_shift) + (val2 >> int_shift), heap);

  } else if (bignum_type_tag == (ptr_type_mask & val1) && int_type_tag == (int_type_mask & val2)) {
    // val1 bignum, val2 fixnum
    
    int64_t ret;
    mpz_t integ;
    mpz_init(integ);

    load_bignum(integ, (int64_t *) (val1 ^ bignum_type_tag)); // load value
    val2 = val2 >> int_shift; // adjust integer value

    // add together
    if( val2 < 0 ) {
      mpz_sub_ui(integ, integ, (unsigned long int) (- val2));
    } else {
      mpz_add_ui(integ, integ, (unsigned long int) val2);
    }

    ret = return_bignum_maybe_fixnum(integ, heap);

    mpz_clear(integ);
    return ret;

  } else if ( int_type_tag == (int_type_mask & val1) && bignum_type_tag == (ptr_type_mask & val2)) {
    // val1 fixnum, val2 bignum
    
    int64_t ret;
    mpz_t integ;
    mpz_init(integ);

    load_bignum(integ, (int64_t *) (val2 ^ bignum_type_tag)); // load value
    val1 = val1 >> int_shift; // adjust integer value
    
    // add together        
    if( val1 < 0 ) {
      mpz_sub_ui(integ, integ, (unsigned long) (- val1));
    } else {
      mpz_add_ui(integ, integ, (unsigned long) val1);
    }

    ret = return_bignum_maybe_fixnum(integ, heap);

    mpz_clear(integ);
    return ret;

  } else { 
    // both are bignum

    int64_t ret;
    mpz_t integ1, integ2;
    mpz_init(integ1); mpz_init(integ2);
    
    load_bignum(integ1, (int64_t *) (val1 ^ bignum_type_tag)); // load value
    load_bignum(integ2, (int64_t *) (val2 ^ bignum_type_tag)); // load value
    
    mpz_add(integ1, integ1, integ2);

    ret = return_bignum_maybe_fixnum(integ1, heap);

    mpz_clear(integ1);
    mpz_clear(integ2);
    return ret;

  }
}

int64_t integer_sub(int64_t val1, int64_t val2, int64_t heap) { // rdi, rsi, rdx
  if (int_type_tag == (int_type_mask & val1) && int_type_tag == (int_type_mask & val2)) {
    // both values are fixnum

    return return_fixnum_maybe_bignum((val1 >> int_shift) - (val2 >> int_shift), heap);

  } else if (bignum_type_tag == (ptr_type_mask & val1) && int_type_tag == (int_type_mask & val2)) {
    // val1 bignum, val2 fixnum

    int64_t ret;
    mpz_t integ;
    mpz_init(integ);

    load_bignum(integ, (int64_t *) (val1 ^ bignum_type_tag)); // load value
    val2 = val2 >> int_shift; // adjust integer value

    // subtract
    if( val2 < 0 ) {
      mpz_add_ui(integ, integ, (unsigned long int) (- val2));
    } else {
      mpz_sub_ui(integ, integ, (unsigned long int) val2);
    }

    ret = return_bignum_maybe_fixnum(integ, heap);

    mpz_clear(integ);
    return ret;
    
  } else if ( int_type_tag == (int_type_mask & val1) && bignum_type_tag == (ptr_type_mask & val2)) {
    // val1 fixnum, val2 bignum

    int64_t ret;
    mpz_t integ;
    mpz_init(integ);

    load_bignum(integ, (int64_t *) (val2 ^ bignum_type_tag)); // load value
    val1 = val1 >> int_shift; // adjust integer value

    // subtract (we will do (- (- val2 val1)))
    if( val1 < 0 ) {
      mpz_add_ui(integ, integ, (unsigned long) (- val1));
    } else {
      mpz_sub_ui(integ, integ, (unsigned long) val1);
    }

    mpz_neg(integ, integ); // need to negate since we subtract in opposite direction

    ret = return_bignum_maybe_fixnum(integ, heap);

    mpz_clear(integ);
    return ret;

  } else { 
    // both are bignum

    int64_t ret;
    mpz_t integ1, integ2;
    mpz_init(integ1); mpz_init(integ2);
    
    load_bignum(integ1, (int64_t *) (val1 ^ bignum_type_tag)); // load value
    load_bignum(integ2, (int64_t *) (val2 ^ bignum_type_tag)); // load value
    
    mpz_sub(integ1, integ1, integ2);

    ret = return_bignum_maybe_fixnum(integ1, heap);

    mpz_clear(integ1);
    mpz_clear(integ2);
    return ret;
    
  }
}

int64_t integer_quotient(int64_t val1, int64_t val2, int64_t heap) { // rdi, rsi, rdx
  
  int64_t ret;
  mpz_t integ1, integ2;
  mpz_init(integ1); mpz_init(integ2);
  
  load_any_to_bignum(integ1, val1); // load value
  load_any_to_bignum(integ2, val2); // load value
  
  mpz_tdiv_q(integ1, integ1, integ2);

  ret = return_bignum_maybe_fixnum(integ1, heap);

  mpz_clear(integ1);
  mpz_clear(integ2);
  return ret;

}

int64_t integer_remainder(int64_t val1, int64_t val2, int64_t heap) { // rdi, rsi, rdx
  
  int64_t ret;
  mpz_t integ1, integ2;
  mpz_init(integ1); mpz_init(integ2);
  
  load_any_to_bignum(integ1, val1); // load value
  load_any_to_bignum(integ2, val2); // load value
  
  mpz_tdiv_r(integ1, integ1, integ2);

  ret = return_bignum_maybe_fixnum(integ1, heap);

  mpz_clear(integ1);
  mpz_clear(integ2);
  return ret;

}

void load_any_to_bignum(mpz_t integ, int64_t val) {
  if (int_type_tag == (int_type_mask & val)) {
    val = val >> int_shift;

    if( val < 0 ) {
      int64_t absval = - val;
      mpz_import(integ, 1, 0, 8, 0, 0, &absval);
      mpz_neg(integ, integ);
    } else {
      mpz_import(integ, 1, 0, 8, 0, 0, &val);
    }
    
  } else {
    int64_t* hp = (int64_t *) (val ^ bignum_type_tag);
    int64_t len = (hp[0] >> int_shift);
    size_t abslen;

    if(len < 0) { // check if the sign is negative
      abslen = (size_t) (- len); // WARNING: potentially lossy conversion
    } else {
      abslen = (size_t) len;
    }

    mpz_import(integ, abslen, -1, (size_t) 8, 0, 0, (void*) (hp+1));
    if(len < 0) { // if sign is negative, negate integer
      mpz_neg(integ, integ);
    }
  }
}

void load_bignum(mpz_t integ, int64_t* hp) {
  int64_t len = (hp[0] >> int_shift);
  size_t abslen;

  if(len < 0) { // check if the sign is negative
    abslen = (size_t) (- len); // WARNING: potentially lossy conversion
  } else {
    abslen = (size_t) len;
  }

  mpz_import(integ, abslen, -1, (size_t) 8, 0, 0, (void*) (hp+1));
  if(len < 0) { // if sign is negative, negate integer
    mpz_neg(integ, integ);
  }
}

int64_t return_bignum_maybe_fixnum(mpz_t integ, int64_t heap) {

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

    return heap | bignum_type_tag;
  } else { // in fixnum range, load into int64_t and return
    int64_t ret = 0;

    mpz_export((void *) &ret, NULL, 0, 8, 0, 0, integ);
    
    if(mpz_sgn(integ) < 0) {
      ret = -ret;
    }

    return ret << int_shift;
  }
}

int64_t return_fixnum_maybe_bignum(int64_t val, int64_t heap) {
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
}
