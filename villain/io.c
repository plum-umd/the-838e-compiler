#include <stdio.h>
#include <inttypes.h>
#include "types.h"
#include "runtime.h"


// TODO: De-duplicate between here an char.c. I can have char.c alloc a buffer
// that's passed over here and then printf the buffer.

// Output utf8 encoding of a codepoint into buffer.
// Return the number of bytes placed in buffer.
int utf8_encode_char(int64_t v, char *buffer) {
  int64_t codepoint = v >> char_shift;
  // Output to buffer using UTF-8 encoding of codepoint
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

#define untag_port(p) ((int64_t*) (p ^ port_type_tag))
#define port_file(p) ((FILE*) p[0])
#define port_buffer_len(p) (p[1])
#define port_buffer_offset(p) (p[2])
#define port_closed(p) (p[3])
#define port_buffer(p) ((int8_t*)(p + 4))
#define port_buffer_bytes 8

FILE *open_input_file(int64_t untagged_str, char *buffer) {
  // Str is untagged in asm prior to calling here. very confusing. Might be
  // worth an extra or and xor just to avoid this.
  utf8_encode_string((int64_t *) untagged_str,  buffer);
  FILE *f = fopen(buffer, "r");
  if (f == NULL) {
    error_handler();
  }
  return f;
}

void close_input_port(int64_t port_val) {
  int64_t *port = untag_port(port_val);
  if (!port_closed(port)) {
    FILE *f = port_file(port);
    fclose(f);
    port_closed(port) = 1;
  }
}

int64_t read_byte_port(int64_t port_val) {
  int64_t *port = untag_port(port_val);
  if (port_closed(port)) {
    error_handler();
  }
  int8_t byte;
  if (port_buffer_offset(port) < port_buffer_len(port)) {
    byte = port_buffer(port)[port_buffer_offset(port)];
    port_buffer_offset(port)++;
  } else {
    int64_t num_read = fread(port_buffer(port), sizeof(int8_t), port_buffer_bytes, port_file(port));
    port_buffer_len(port) = num_read;
    // Start a 1 because we immediately read a byte
    port_buffer_offset(port) = 1;
    if (num_read > 0) {
      byte = port_buffer(port)[0];
    } else {
      byte = EOF;
    }
  }
  return (byte == EOF) ?
    val_eof :
    (int64_t)(byte << int_shift);
}

int64_t peek_byte_port(int64_t port_val) {
  int64_t *port = untag_port(port_val);
  if (port_closed(port)) {
    error_handler();
  }
  int8_t byte;
  if (port_buffer_offset(port) < port_buffer_len(port)) {
    byte = port_buffer(port)[port_buffer_offset(port)];
  } else {
    int64_t num_read = fread(port_buffer(port), sizeof(int8_t), port_buffer_bytes, port_file(port));
    port_buffer_len(port) = num_read;
    // Start at 0 because we don't consume the byte
    port_buffer_offset(port) = 0;
    if (num_read > 0) {
      byte = port_buffer(port)[0];
    } else {
      byte = EOF;
    }
  }
  return (byte == EOF) ?
    val_eof :
    (int64_t)(byte << int_shift);
}

int64_t read_byte(void) {
  char c = getc(in);
  return (c == EOF) ?
    val_eof :
    (int64_t)(c << int_shift);
}

int64_t peek_byte(void) {
  char c = getc(in);
  ungetc(c, in);
  return (c == EOF) ?
    val_eof :
    (int64_t)(c << int_shift);
}

int64_t write_byte(int64_t c) {
  int64_t codepoint = c >> int_shift;
  putc((char) codepoint, out);
  return 0;
}
