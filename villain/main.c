#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>
#include "types.h"
#include "runtime.h"
#include "char.h"
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
void print_vector(int64_t);

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
  } else if(vector_type_tag == (ptr_type_mask & result)) {
    print_vector(result);
                                                          
  }  else if (int_type_tag == (int_type_mask & result)) {
    printf("%" PRId64, result >> int_shift);
  } else if (char_type_tag == (char_type_mask & result)) {
    print_char(result);
  } else if (flonum_type_tag == (ptr_type_mask & result)) {  
    result = *((int64_t *)(result ^ flonum_type_tag));
    int64_t sign= 1 &  (result >> 63);
    int64_t exp = (( (1 <<  11) - 1) &  (result >> 52));
    int64_t mantissa = ( ( (int64_t)1 <<  52) - 1 )&  result;
    double dec_man= 0;
    for (int i = -52; i<0; i++) {
        if (1 == (1 & mantissa)) {
          dec_man+=pow(2, i);
        }
        mantissa=mantissa>>1;
    }
    double resultFlonum=  pow(-1, sign) * pow(2,exp - 1023) * (1 + dec_man);
    printf("%f", resultFlonum);  
  } else if (str_type_tag == (ptr_type_mask & result)) { 
    printf("\"");
    print_str((int64_t *)(result ^ str_type_tag));
    printf("\"");
  } else if (symbol_type_tag == (ptr_type_mask & result)) {
    printf("'");
    print_str((int64_t *)(result ^ symbol_type_tag));
  } else if (port_type_tag == (ptr_type_mask & result)) {
    printf("#<input-port>");
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

void print_vector(int64_t result) {
  int64_t len = *((int64_t *)(result ^ vector_type_tag));
  int64_t  curr = result+8;
  int64_t i = 0;
  printf("#(");
  while(i < len){ //should be len
     print_result(*((int64_t *)((curr) ^ vector_type_tag)));
     
     curr= curr + 8;
     i+=16;
     if(i < len){
        printf(" ");
              };
  }
  printf(")");

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
