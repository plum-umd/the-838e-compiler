#include <stdio.h>
#include <inttypes.h>
#include "villain.h"
#include "runtime.h"
#include <math.h>
#include <gmp.h>
#include <string.h>

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

void print_closure(int64_t *closure);

void print_result(vl_val x)
{

  if ((x & 0x7000000000000002) == 0x1000000000000002) {
    // Calling print_closure can cause an infinite loop.  Would be easy enough
    // to avoid by tracking a set of printed closures, but not worth it. 
    //print_closure(x ^ 0x1000000000000002);
    printf("#<closure: 0x%lX>", x);
    return;
  }
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
    printf("Unknown type: 0x%lX", x);
    break;
  }
  printf("");
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

void print_contract(int64_t *contract) {
  if ((((int64_t)contract) & 0x7000000000000002) == 0x1000000000000002) {
    printf("flat");
  } else {
    int64_t count = contract[0];
    printf("fn (%ld) {", count);
    for (int i = 0; i < count; i++) {
      int64_t next = contract[i + 1];
      print_contract(next);
      printf(", ");
    }
    printf("}");
  }
}

void print_contract_list(int64_t *contract_list) {
  if (contract_list == 0) {
  } else {
    print_contract(contract_list[0]);
    print_contract_list((int64_t*)contract_list[1]);
  }
}

void print_closure(int64_t *closure) {
  printf("<Closure: ");
  int64_t free_vars = closure[1];
  printf("vars: %ld env: { ", free_vars);
  int64_t *env = closure + 2;
  for (int i = 0; i < free_vars; i++) {
    print_result(env[i]);
    printf(", ");
  }
  printf("} contracts: { ");
  print_contract_list((int64_t*)closure[2 + free_vars]);
  printf("}>");
}

void println() { printf("\n"); }

void my_memcpy(void *dst, void *src, size_t size) {
  memcpy(dst, src, size);
}
