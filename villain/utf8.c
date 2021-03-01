#include <stdio.h>
#include "types.h"
#include "utf8.h"
#include "char.h"

int utf8_encode_char(vl_char c, char *buffer)
{
  // Output to buffer using UTF-8 encoding of codepoint
  // https://en.wikipedia.org/wiki/UTF-8
  if (c < 128) {
    buffer[0] = (char) c;
    return 1;
  } else if (c < 2048) {
    buffer[0] =  (char)(c >> 6)       | 192;
    buffer[1] = ((char)       c & 63) | 128;
    return 2;
  } else if (c < 65536) {
    buffer[0] =  (char)(c >> 12)      | 224;
    buffer[1] = ((char)(c >> 6) & 63) | 128;
    buffer[2] = ((char)       c & 63) | 128;
    return 3;
  } else {
    buffer[0] =  (char)(c >> 18)       | 240;
    buffer[1] = ((char)(c >> 12) & 63) | 128;
    buffer[2] = ((char)(c >>  6) & 63) | 128;
    buffer[3] = ((char)        c & 63) | 128;
    return 4;
  }
}

void utf8_encode_string(vl_str *s, char *buffer)
{
  uint64_t i;
  for (i = 0; i < s->len; i++) {
    buffer += utf8_encode_char(s->buf[i], buffer);
  }
  *buffer = '\0';
}

void print_codepoint(vl_char c)
{
  static char buffer[5] = {0};
  utf8_encode_char(c, buffer);
  printf("%s", buffer);
}
