#include <stdio.h>
#include <inttypes.h>
#include <wchar.h>
#include "villain.h"
#include "runtime.h"

vl_val read_byte(void)
{
  char c = getc(in);
  return (c == EOF) ? vl_wrap_eof() : vl_wrap_int(c);
}

vl_val peek_byte(void)
{
  char c = getc(in);
  ungetc(c, in);
  return (c == EOF) ? vl_wrap_eof() : vl_wrap_int(c);
}

vl_val write_byte(vl_val c)
{
  putc((char) vl_unwrap_int(c), out);
  return vl_wrap_void();
}

vl_val read_char(void)
{
  wchar_t c = getwc(in);
  return (c == WEOF) ? vl_wrap_eof() : vl_wrap_char(c);
}

vl_val peek_char(void)
{
  wchar_t c = getwc(in);
  ungetwc(c, in);
  return (c == WEOF) ? vl_wrap_eof() : vl_wrap_char(c);
}

vl_val write_char(vl_val c)
{
  putwc((wchar_t) vl_unwrap_char(c), out);
  return vl_wrap_void();
}
