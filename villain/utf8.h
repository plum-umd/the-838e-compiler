#ifndef VILLAIN_UTF8_H
#define VILLAIN_UTF8_H

#include <inttypes.h>

// Output the utf8 encoding of a codepoint into buffer. Returns the number of
// bytes used in the encoding. This will between 1 and 4, so buffer should be
// at least 4 bytes wide.
int utf8_encode_char(int64_t codepoint, char *buffer);

// Output the utf8 encoding of str into the buffer. Codepoints can use up to 4
// bytes. A null terminator is added to the end of the buffer. In total buffer
// should be allocated to hold (4 |str| + 1) bytes.
void utf8_encode_string(int64_t *str, char *buffer);

// Prints the codepoint using utf8 encoding
void print_codepoint(int64_t codepoint);
#endif
