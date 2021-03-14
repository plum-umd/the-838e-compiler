/* capi.c: utility functions for C API */
#include "villain.h"
#include "runtime.h"

void vl_check_type(vl_val x, vl_type type)
{
  if (vl_typeof(x) != type)
    error_handler();
}

void vl_check_arity(uint64_t argc, uint64_t arity)
{
  if (argc != arity)
    error_handler();
}

void vl_check_varity(uint64_t argc, uint64_t min, uint64_t max)
{
  if (argc < min || argc > max)
    error_handler();
}
