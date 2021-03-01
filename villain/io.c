#include <stdio.h>
#include <wchar.h>
#include <limits.h>
#include "villain.h"
#include "runtime.h"
#include "utf8.h"

#define port_buffer_bytes 8

FILE* open_input_file(vl_val filename)
{
  char *buf;
  FILE *f;
  vl_str *s = vl_unwrap_str(filename);

  buf = vl_calloc((s->len*4)+1, 1);
  if (!buf)
    error_handler();

  utf8_encode_string(s, buf);

  f = fopen(buf, "rb");
  if (!f)
    error_handler();

  vl_free(buf);
  return f;
}

void close_input_port(vl_val port)
{
  vl_port *p = vl_unwrap_port(port);

  if (!p->closed) {
    fclose(p->fp);
    p->closed = 1;
  }
}

static int
populate_buffer(vl_port *p)
{
  if (p->offset < p->len)
    return 1;

  p->len = fread(p->buf, 1, port_buffer_bytes, p->fp);
  p->offset = 0;

  return p->len > 0;
}

vl_val read_byte_port(vl_val port)
{
  int has_bytes;
  char c;
  vl_port *p = vl_unwrap_port(port);

  if (p->closed)
    error_handler();

  has_bytes = populate_buffer(p);
  if (!has_bytes)
    return vl_wrap_eof();

  c = p->buf[p->offset];
  p->offset++;

  return vl_wrap_int(c);
}

vl_val peek_byte_port(vl_val port)
{
  int has_bytes;
  char c;
  vl_port *p = vl_unwrap_port(port);

  if (p->closed)
    error_handler();

  has_bytes = populate_buffer(p);
  if (!has_bytes)
    return vl_wrap_eof();

  c = p->buf[p->offset];

  return vl_wrap_int(c);
}

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
