#include <math.h>
#include "villain.h"

vl_val math_sin(uint64_t argc, vl_val *argv)
{
  double *x;

  vl_check_arity(argc, 1);
  vl_check_type(argv[0], VL_FLONUM);

  x = vl_unwrap_flonum_ptr(argv[0]);

  return vl_wrap_flonum(sin(*x));
}

vl_val math_modulo(uint64_t argc, vl_val *argv)
{
  int64_t n, m;

  vl_check_arity(argc, 2);
  vl_check_type(argv[0], VL_INT);
  vl_check_type(argv[1], VL_INT);

  n = vl_unwrap_int(argv[0]);
  m = vl_unwrap_int(argv[1]);

  while (n < 0) n += m;
  while (n >= m) n -= m;

  return vl_wrap_int(n);
}

vl_val math_lshift(uint64_t argc, vl_val *argv)
{
  vl_check_arity(argc, 2);
  vl_check_type(argv[0], VL_INT);
  vl_check_type(argv[1], VL_INT);

  return argv[0] << vl_unwrap_int(argv[1]);
}

vl_val math_rshift(uint64_t argc, vl_val *argv)
{
  vl_check_arity(argc, 2);
  vl_check_type(argv[0], VL_INT);
  vl_check_type(argv[1], VL_INT);

  return vl_wrap_int(vl_unwrap_int(argv[0]) >> vl_unwrap_int(argv[1]));
}

vl_val math_or(uint64_t argc, vl_val *argv)
{
  vl_check_arity(argc, 2);
  vl_check_type(argv[0], VL_INT);
  vl_check_type(argv[1], VL_INT);

  return argv[0] | argv[1];
}

vl_val math_xor(uint64_t argc, vl_val *argv)
{
  vl_check_arity(argc, 2);
  vl_check_type(argv[0], VL_INT);
  vl_check_type(argv[1], VL_INT);

  return argv[0] ^ argv[1];
}

vl_val math_and(uint64_t argc, vl_val *argv)
{
  vl_check_arity(argc, 2);
  vl_check_type(argv[0], VL_INT);
  vl_check_type(argv[1], VL_INT);

  return argv[0] & argv[1];
}
