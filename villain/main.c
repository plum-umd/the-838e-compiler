#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include "types.h"
#include "runtime.h"
#include <math.h>

FILE* in;
FILE* out;
void (*error_handler)();
int64_t *heap;

FILE* in;
FILE* out;
void (*error_handler)();
int64_t *heap;

void print_result(int64_t);
void print_str(int64_t *);

void error_exit() {
  printf("err\n");
  exit(1);
}

void raise_error() {
  return error_handler();
}

int main(int argc, char** argv) {
  in = stdin;
  out = stdout;
  error_handler = &error_exit;
  heap = malloc(8 * heap_size);
  int64_t result = entry(heap);
  print_result(result);
  if (result != val_void) printf("\n");
  free(heap);
  return 0;
}

void print_char(int64_t);
void print_cons(int64_t);

void print_result(int64_t result) {

  if (cons_type_tag == (ptr_type_mask & result)) {
    printf("'(");
    print_cons(result);
    printf(")");
  } else if (box_type_tag == (ptr_type_mask & result)) {
    printf("#&");
    print_result (*((int64_t *)(result ^ box_type_tag)));
  } else if (int_type_tag == (int_type_mask & result)) {
    printf("%" PRId64, result >> int_shift);
  } else if (char_type_tag == (char_type_mask & result)) {
    print_char(result);
  } else if (float_type_tag == (float_type_mask & result)) {        
    int sig = (int)(result >> (float_shift +32));
    int sign= (int)(1 &  (result >> (float_shift + 31)));
    int exp = (int)(( (1 <<  8) - 1) &  (result >> (float_shift + 23)));
    int mantissa = (int)(( (1 <<  23) - 1 )&  (result >> (float_shift)));
    float dec_man= 0;
    for (int i = -23; i<0; i++) {
        if (1 == (1 & mantissa)) {
          dec_man+=pow(2, i);
        }
        mantissa=mantissa>>1;
    }
    float resultFloat=  pow(-1, sign) * pow(2,exp - 127) * (1 + dec_man);
    float roundedResult = (round(resultFloat * (pow(10, sig)))/(pow(10,sig)));
    printf("%f", resultFloat);  
  } else if (str_type_tag == (ptr_type_mask & result)) { 
    printf("\"");
    print_str((int64_t *)(result ^ str_type_tag));
    printf("\"");
  } else if (symbol_type_tag == (ptr_type_mask & result)) {
    printf("'");
    print_str((int64_t *)(result ^ symbol_type_tag));
  } else if (prefab_type_tag == (ptr_type_mask & result)) {
    printf("Hello");	 
  } else {
    switch (result) {
    case val_true:
      printf("#t"); break;
    case val_false:
      printf("#f"); break;
    case val_eof:
      printf("#<eof>"); break;
    case val_empty:
      printf("()"); break;
    case val_void:
      /* nothing */ break;
    }
  }
}

void print_cons(int64_t a) {
  int64_t car = *((int64_t *)((a + 8) ^ cons_type_tag));
  int64_t cdr = *((int64_t *)((a + 0) ^ cons_type_tag));
  print_result(car);
  if (cdr == val_empty) {
    // nothing
  } else if (cons_type_tag == (ptr_type_mask & cdr)) {
    printf(" ");
    print_cons(cdr);
  } else {
    printf(" . ");
    print_result(cdr);
  }
}
