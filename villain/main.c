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
void print_prefab(int64_t *);

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
    printf("'#s(");
    print_prefab((int64_t*)(result ^ prefab_type_tag));
    printf(")");	 
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

void print_prefab(int64_t* value) {
	
  int64_t keyLength = *value;

  //Print all the key data
  print_str((int64_t *)(*(value + 2) ^ symbol_type_tag)); //Print the symbol
  
  int64_t n1 =  (*(value + 3) >> int_shift);
  if(n1 > 0) {
    printf(" %" PRId64, n1); //The number of non automatic values
  }

  int64_t first_aut = *(value + 4);
  int64_t second_aut = *(value + 5);

  if((first_aut >> int_shift) != 0) {
    //A list of size 2 where the first thing is the number of automatic values
    //an the second thing is an arbitrary value for them
    printf(" ");
    printf("(");
    print_result(first_aut);
    printf(" ");
    print_result(second_aut);
    printf(")");
  }


  int i = 0;
  if(keyLength - 4 != 0) {
    printf(" #(");
    for(i = 0; i < (keyLength - 4); i++) {
      //A list of integers representing the mutable fields in the struct
      print_result(*(value + 6 + i));
      if(i != (keyLength - 4 - 1)) {
	      printf(" ");
      }
    }
    printf(")");
  }
  
  //The fields of the struct
  for(i = 0; i < *(value + 1); i++) {
	printf(" ");
	print_result(*(value + 6 + (keyLength - 4)  + i));
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
