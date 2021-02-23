#include <stdlib.h>

#include "types.h"
#include "villain.h"

vl_type vl_typeof(vl_val x)
{
  switch (x & ptr_type_mask) {
  case box_type_tag:
    return VL_BOX;
  case cons_type_tag:
    return VL_CONS;
  case str_type_tag:
    return VL_STR;
  case symbol_type_tag:
    return VL_SYMBOL;
  case vector_type_tag:
    return VL_VEC;
  case flonum_type_tag:
    return VL_FLONUM;
  default:
    if ((int_type_mask & x) == int_type_tag)
      return VL_INT;
    else if ((char_type_mask & x) == char_type_tag)
      return VL_CHAR;

    switch (x) {
    case val_true:
    case val_false:
      return VL_BOOL;
    case val_eof:
      return VL_EOF;
    case val_empty:
      return VL_EMPTY;
    case val_void:
      return VL_VOID;
    }
  }
  return VL_INVALID;
}

int64_t vl_unwrap_int(vl_val x)
{
  return x >> int_shift;
}
vl_val vl_wrap_int(int64_t i)
{
  return (i << int_shift) | int_type_tag;
}

int vl_unwrap_bool(vl_val x){
  return x == val_true;
}
vl_val vl_wrap_bool(int b){
  return b ? val_true : val_false;
}

vl_char vl_unwrap_char(vl_val x)
{
  return (vl_char)(x >> char_shift);
}
vl_val vl_wrap_char(vl_char c)
{
  return (((vl_val)c) << char_shift) | char_type_tag;
}

/* we need a UTF32 string representation
vl_str* vl_unwrap_str(vl_val x)
{
  return (vl_str *)(x ^ str_type_tag);
}
*/
vl_str* vl_unwrap_str(vl_val x)
{
  int64_t *s = (int64_t *)(x ^ str_type_tag);
  uint64_t len;
  uint64_t n;

  len = (s[0] >> int_shift);

  vl_str *str = vl_calloc(1, sizeof(vl_str) + len*sizeof(vl_char));
  if (!str)
    return NULL;

  str->len = len;
  for (n = 0; n < len; ++n) {
    uint64_t i = n/3 + 1;
    uint64_t j = n % 3;
    str->buf[n] = (0x1FFFFF & (s[i] >> (j*21))) >> char_shift;
  }
  return str;
}
vl_val vl_wrap_str(vl_str *s)
{
  return ((vl_val)s) | str_type_tag;
}

double vl_unwrap_flonum(vl_val x){
  int64_t f = *((int64_t *)(x ^ flonum_type_tag));

  int64_t sign= 1 & (f >> 63) ? -1 : 1;
  int64_t exp = (((1 << 11) - 1) & (f >> 52));
  int64_t mantissa = (((int64_t)1 << 52) - 1) & f;
  double dec_man= 0;

  for (int i = -52; i < 0; i++) {
    if (1 == (1 & mantissa)) {
      dec_man += 1 << i;
    }
    mantissa = mantissa>>1;
  }

  return sign * (1<<(exp - 1023)) * (1 + dec_man);
}
vl_val vl_wrap_flonum(double f){
  // TODO
  return 0;
}

vl_box vl_unwrap_box(vl_val x)
{
  return (vl_box)(x ^ box_type_tag);
}
vl_val vl_wrap_box(vl_box b)
{
  return ((vl_val)b) | box_type_tag;
}

vl_vec* vl_unwrap_vec(vl_val x)
{
  return (vl_vec *)(x ^ vector_type_tag);
}
vl_val vl_wrap_vec(vl_vec *v)
{
  return ((vl_val)v) | str_type_tag;
}

vl_cons* vl_unwrap_cons(vl_val x)
{
  return (vl_cons *)(x ^ cons_type_tag);
}
vl_val vl_wrap_cons(vl_cons *c)
{
  return ((vl_val)c) | cons_type_tag;
}

/* we need a UTF32 string representation
vl_symbol vl_unwrap_symbol(vl_val x)
{
  return (vl_symbol)(x ^ symbol_type_tag);
} */
vl_symbol vl_unwrap_symbol(vl_val x)
{
  int64_t *s = (int64_t *)(x ^ symbol_type_tag);
  uint64_t len;
  uint64_t n;

  len = (s[0] >> int_shift);

  vl_str *str = vl_calloc(1, sizeof(vl_str) + len*sizeof(vl_char));
  if (!str)
    return NULL;

  str->len = len;
  for (n = 0; n < len; ++n) {
    uint64_t i = n/3 + 1;
    uint64_t j = n % 3;
    str->buf[n] = (0x1FFFFF & (s[i] >> (j*21))) >> char_shift;
  }
  return str;
}
vl_val vl_wrap_symbol(vl_symbol s)
{
  return ((vl_val)s) | symbol_type_tag;
}

vl_val vl_wrap_eof(void)
{
  return val_eof;
}

vl_val vl_wrap_empty(void)
{
  return val_empty;
}

vl_val vl_wrap_void(void)
{
  return val_void;
}