#include <inttypes.h>
#include "types.h"
#include "utf8.h"
#include "stdlib.h"
#include "stdio.h"

int utf8_encode_char(int64_t v, char *buffer) {
  int64_t codepoint = v >> char_shift;
  // Output to buffer using UTF-8 encoding of codepoint
  // https://en.wikipedia.org/wiki/UTF-8
  if (codepoint < 128) {
    buffer[0] = (char) codepoint;
    return 1;
  } else if (codepoint < 2048) {
    buffer[0] = (char)(codepoint >> 6) | 192;
    buffer[1] = ((char)codepoint & 63) | 128;
    return 2;
  } else if (codepoint < 65536) {
    buffer[0] = (char)(codepoint >> 12) | 224;
    buffer[1] = ((char)(codepoint >> 6) & 63) | 128;
    buffer[2] = ((char)codepoint & 63) | 128;
    return 3;
  } else {
    buffer[0] = (char)(codepoint >> 18) | 240;
    buffer[1] = ((char)(codepoint >> 12) & 63) | 128;
    buffer[2] = ((char)(codepoint >> 6) & 63) | 128;
    buffer[3] = ((char)codepoint & 63) | 128;
    return 4;
  }
}

void utf8_encode_string(int64_t *str, char *buffer) {
  int64_t len = (str[0] >> int_shift);
  int temp;
  int i, j;
  int n = (len % 3 == 0) ? len / 3 : (len / 3 + 1);
  for (i = 1; i < n; i++) {
    for (j = 0; j < 3; j++) {
      temp = str[i] >> (j * 21);
      int codepoint_len = utf8_encode_char(temp, buffer);
      buffer += codepoint_len;
    }
  }
  i = (len % 3 == 0) ? 3 : (len % 3);
  for (j = 0; j < i; j++){
    temp = str[n] >> (j * 21);
    int codepoint_len = utf8_encode_char(temp, buffer);
    buffer += codepoint_len;
  }
  *buffer = 0;
}

void print_codepoint(int64_t v) {
  static char buffer[5] = {0};
  utf8_encode_char(v, buffer);
  printf("%s", buffer);
}
