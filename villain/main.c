#include <stdio.h>
#include <inttypes.h>
#include "villain.h"
#include "runtime.h"
#include <math.h>
#include <gmp.h>

FILE* in;
FILE* out;
void (*error_handler)();
int64_t *heap;


void error_exit()
{
  printf("err\n");
  exit(1);
}

void raise_error()
{
  return error_handler();
}

void print_str(vl_str *);
void print_char(vl_char);
void print_cons(vl_cons *);
void print_vector(vl_vec *);
void print_bignum(vl_bignum *);
vl_str *symbol_to_str(vl_symbol s);

void print_result(vl_val x)
{
  switch (vl_typeof(x)) {
  case VL_INT:
    printf("%" PRId64, vl_unwrap_int(x));
    break;
  case VL_CHAR:
    print_char(vl_unwrap_char(x));
    break;
  case VL_BOOL:
    if (vl_unwrap_bool(x))
      printf("#t");
    else
      printf("#f");
    break;
  case VL_EOF:
    printf("#<eof>");
    break;
  case VL_VOID:
    break;
  case VL_EMPTY:
    printf("'()");
    break;
  case VL_BOX:
    printf("#&");
    print_result(vl_unwrap_box(x)->val);
    break;
  case VL_CONS:
    printf("'(");
    print_cons(vl_unwrap_cons(x));
    printf(")");
    break;
  case VL_STR:
    printf("test");
    putchar('"');
    print_str(vl_unwrap_str(x));
    putchar('"');
    break;
  case VL_SYMBOL:
    putchar('\'');
    /* after we have UTF32 string
     * print_str((vl_str *)vl_unwrap_symbol(x)); */
    print_str(symbol_to_str(vl_unwrap_symbol(x)));
    break;
  case VL_VEC:
    print_vector(vl_unwrap_vec(x));
    break;
  case VL_FLONUM:
    printf("%f", vl_unwrap_flonum(x));
    break;
  case VL_PORT:
    printf("#<input-port>");
    break;
  case VL_BIGNUM:
    print_bignum(vl_unwrap_bignum(x));
    break;
  case VL_INVALID:
  default:
    error_exit();
    break;
  }
}

void print_vector(vl_vec *v)
{
  uint64_t i;

  printf("'#(");
  for (i = 0; i < v->len; ++i) {
    print_result(v->buf[i]);

    if (i < v->len - 1)
      putchar(' ');
  }
  printf(")");
}

void print_cons(vl_cons *cons)
{
  print_result(cons->fst);

  switch (vl_typeof(cons->snd)) {
  case VL_EMPTY:
    // nothing
    break;
  case VL_CONS:
    printf(" ");
    print_cons(vl_unwrap_cons(cons->snd));
    break;
  default:
    printf(" . ");
    print_result(cons->snd);
    break;
  }
}

int main(int argc, char** argv)
{
  vl_val result;

  in = stdin;
  out = stdout;
  error_handler = &error_exit;
  heap = vl_calloc(8 * heap_size, 1);

  result = entry(heap);

  print_result(result);
  if (vl_typeof(result) != VL_VOID)
    putchar('\n');

  free(heap);
  return 0;
}
