#include <string.h>
#include "types.h"
#include "villain.h"

int64_t *str_dup(const int64_t *s)
{
  int64_t len = (s[0] >> int_shift);
  int n = (len % 3 == 0) ? len / 3 : (len / 3 + 1);
  int64_t *d;

  d = vl_calloc(1+n, sizeof(int64_t));
  if (!d)
    return NULL;

  return memcpy(d, s, (1+n) * sizeof(int64_t));
}

int64_t str_cmp(const int64_t *s1, const int64_t *s2)
{
  int64_t len1 = (s1[0] >> int_shift);
  int64_t len2 = (s2[0] >> int_shift);
  int64_t len = len1 < len2 ? len1 : len2;
  int n = (len % 3 == 0) ? len / 3 : (len / 3 + 1);
  int i;

  for (i = 1; i <= n; ++i) {
    if (s1[i] != s2[i])
      return s1[i] - s2[i];
  }
  if (len1 == len2)
    return 0;
  else if (len1 > len2)
    return s1[i+1];
  else
    return s2[i+1];
}

int64_t *str_from_cstr(const char *s)
{
  int64_t len = strlen(s);
  int n = (len % 3 == 0) ? len / 3 : (len / 3 + 1);
  int64_t *vs = NULL;
  int i, j, remain;
  size_t pos = 0;

  vs = vl_calloc(1+n, sizeof(int64_t));
  if (!vs)
    return NULL;

  vs[0] = vl_wrap_int(len);

  // the inverse of print_str
  for (i = 1; i < n; i++) {
    for (j = 0; j < 3; j++) {
      vs[i] |= vl_wrap_char(s[pos++]) << (j * 21);
    }
  }
  remain = (len % 3 == 0) ? 3 : (len % 3);
  for (j = 0; j < remain; j++){
    vs[i] |= vl_wrap_char(s[pos++]) << (j * 21);
  }
  return vs;
}

#ifdef CHECK
// $ gcc -DCHECK str.c
#include <assert.h>

int main(void)
{
  int64_t *foo = str_from_cstr("foo");
  int64_t *foo_ = str_from_cstr("foo");

  int64_t *foo1 = str_from_cstr("foo1");
  int64_t *foo1_ = str_dup(foo1);

  int64_t *fo = str_from_cstr("fo");
  int64_t *fo_ = str_dup(fo);

  int64_t *bar = str_from_cstr("bar");

  assert(str_cmp(foo,foo_) == 0);
  assert(str_cmp(foo1,foo1_) == 0);
  assert(str_cmp(fo,fo_) == 0);

  assert(str_cmp(foo,foo1) != 0);
  assert(str_cmp(foo,foo1) == str_cmp(foo,foo1_));
  assert(str_cmp(foo,foo1) == str_cmp(foo_,foo1));
  assert(str_cmp(foo,foo1) == str_cmp(foo_,foo1_));

  assert(str_cmp(foo,fo) != 0);
  assert(str_cmp(foo,fo) == str_cmp(foo,fo_));
  assert(str_cmp(foo,fo) == str_cmp(foo_,fo));
  assert(str_cmp(foo,fo) == str_cmp(foo_,fo_));

  assert(str_cmp(foo,bar) != 0);

  return 0;
}
#endif
