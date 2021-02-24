#include <inttypes.h>
#include "str.h"
#include "types.h"
#include "villain.h"

// binary tree node
struct Node {
  int64_t* elem;
  struct Node* left;
  struct Node* right;
};

static struct Node *symbol_tbl = NULL;
static uint64_t gensym_ctr = 0;

int64_t *str_to_symbol(const int64_t *str)
{
  struct Node **curr = &symbol_tbl;

  while (*curr) {
    struct Node *t = *curr;
    int64_t r = str_cmp(str, t->elem);
    if (r == 0) {
      return t->elem;
    } else if (r < 0) {
      curr = &t->left;
    } else {
      curr = &t->right;
    }
  }

  // wasn't found, so insert it
  *curr = vl_calloc(1, sizeof(struct Node));
  struct Node* t = *curr;
  t->elem = str_dup(str);

  return t->elem;
}

vl_str *symbol_to_str(vl_symbol s)
{
  return vl_unwrap_str((vl_val)s | str_type_tag);
}

int64_t *gensym(void)
{
  char s[100]; // uint64_t has maximum 20 digits
  sprintf(s, "g%" PRIu64, gensym_ctr++);
  return str_from_cstr(s); // uninterned symbol
}

#ifdef CHECK
// $ gcc -DCHECK symbol.c str.o
#include <assert.h>

int main(void)
{
  int64_t *foo = str_from_cstr("foo");
  int64_t *foo_ = str_from_cstr("foo");

  int64_t *foo1 = str_from_cstr("foo1");
  int64_t *fo = str_from_cstr("fo");

  assert(str_to_symbol(foo) == str_to_symbol(foo_));
  assert(str_to_symbol(foo1) == str_to_symbol(foo1));
  assert(str_to_symbol(fo) == str_to_symbol(fo));
  assert(str_to_symbol(foo) != str_to_symbol(foo1));
  assert(str_to_symbol(foo) != str_to_symbol(fo));
  assert(str_to_symbol(foo1) != str_to_symbol(fo));

  assert(gensym() != gensym());

  return 0;
}
#endif
