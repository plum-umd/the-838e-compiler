/* unistring.c: wrapper for libunistring functions */
#include <unictype.h>
#include <unicase.h>
#include "villain.h"

vl_val char_alphabetic(uint64_t argc, vl_val *argv)
{
  vl_char c;

  vl_check_arity(argc, 1);
  vl_check_type(argv[0], VL_CHAR);

  c = vl_unwrap_char(argv[0]);

  return vl_wrap_bool(uc_is_property_alphabetic(c));
}

vl_val char_whitespace(uint64_t argc, vl_val *argv)
{
  vl_char c;

  vl_check_arity(argc, 1);
  vl_check_type(argv[0], VL_CHAR);

  c = vl_unwrap_char(argv[0]);

  return vl_wrap_bool(uc_is_property_white_space(c));
}

vl_val char_upcase(uint64_t argc, vl_val *argv)
{
  vl_char c;

  vl_check_arity(argc, 1);
  vl_check_type(argv[0], VL_CHAR);

  c = vl_unwrap_char(argv[0]);

  return vl_wrap_char(uc_toupper(c));
}

vl_val char_downcase(uint64_t argc, vl_val *argv)
{
  vl_char c;

  vl_check_arity(argc, 1);
  vl_check_type(argv[0], VL_CHAR);

  c = vl_unwrap_char(argv[0]);

  return vl_wrap_char(uc_tolower(c));
}

vl_val char_titlecase(uint64_t argc, vl_val *argv)
{
  vl_char c;

  vl_check_arity(argc, 1);
  vl_check_type(argv[0], VL_CHAR);

  c = vl_unwrap_char(argv[0]);

  return vl_wrap_char(uc_totitle(c));
}
