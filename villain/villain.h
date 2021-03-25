#ifndef VILLAIN_H
#define VILLAIN_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/* any abstract villain value */
typedef int64_t vl_val;

typedef enum vl_type {
  VL_INVALID = -1,
  /* immediates */
  VL_INT,
  VL_CHAR,
  VL_BOOL,
  VL_EOF,
  VL_VOID,
  VL_EMPTY,
  /* pointers */
  VL_BOX,
  VL_CONS,
  VL_STR,
  VL_SYMBOL,
  VL_VEC,
  VL_FLONUM,
  VL_PORT,
  VL_BIGNUM
} vl_type;

typedef struct {
  vl_val val;
}* vl_box;
typedef uint32_t vl_char;
typedef struct vl_cons {
  vl_val snd;
  vl_val fst;
} vl_cons;
typedef struct vl_vec {
  uint64_t len;
  vl_val buf[];
} vl_vec;
typedef struct vl_str {
  uint64_t len;
  vl_char buf[];
} vl_str;
typedef vl_str* vl_symbol;
typedef struct vl_port {
  FILE *fp;
  uint8_t len;
  uint8_t offset;
  int8_t closed;
  char buf[];
} vl_port;
typedef struct vl_bignum {
  int64_t signlen;
  int64_t buf[];
} vl_bignum;

/* return the type of x */
vl_type vl_typeof(vl_val x);

/**
 * Wrap/unwrap villain values
 *
 * The behavior of unwrap functions are undefined on type mismatch.
 */
int64_t vl_unwrap_int(vl_val x);
vl_val vl_wrap_int(int64_t i);

int vl_unwrap_bool(vl_val x);
vl_val vl_wrap_bool(int b);

vl_char vl_unwrap_char(vl_val x);
vl_val vl_wrap_char(vl_char c);

vl_str* vl_unwrap_str(vl_val x);
vl_val vl_wrap_str(vl_str *s);

double vl_unwrap_flonum(vl_val x);
vl_val vl_wrap_flonum(double f);

vl_box vl_unwrap_box(vl_val x);
vl_val vl_wrap_box(vl_box b);

vl_vec* vl_unwrap_vec(vl_val x);
vl_val vl_wrap_vec(vl_vec *v);

vl_cons* vl_unwrap_cons(vl_val x);
vl_val vl_wrap_cons(vl_cons* c);

vl_symbol vl_unwrap_symbol(vl_val x);
vl_val vl_wrap_symbol(vl_symbol s);

vl_port* vl_unwrap_port(vl_val x);
vl_val vl_wrap_port(vl_port *p);

vl_bignum* vl_unwrap_bignum(vl_val x);
vl_val vl_wrap_bignum(vl_bignum *p);

vl_val vl_wrap_eof(void);
vl_val vl_wrap_empty(void);
vl_val vl_wrap_void(void);

/* Currently the same as malloc and calloc, but they might
 * be useful if we want to add GC later */
#define vl_malloc malloc
#define vl_calloc calloc
#define vl_realloc realloc
#define vl_free free

#endif
