#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// binary tree node
struct Node {
  char* elem;
  struct Node* left;
  struct Node* right;
};

static struct Node *symbol_tbl = NULL;
static uint64_t gensym_ctr = 0;

static int
ndigits(unsigned long x)
{
  int d = 0;
  if (x == 0) return 1;
  while (x) {
    x /= 10;
    d++;
  }
  return d;
}

static char *
mystrdup(const char *s)
{
	size_t l = strlen(s);
	char *d = malloc(l+1);
	return memcpy(d, s, l+1);
}

char *str_to_symbol(const char *str) {
  struct Node **curr = &symbol_tbl;

  while (*curr) {
    struct Node *t = *curr;
    int r = strcmp(str, t->elem);
    if (r == 0) {
      return t->elem;
    } else if (r < 0) {
      curr = &t->left;
    } else {
      curr = &t->right;
    }
  }

  // wasn't found, so insert it
  // TODO: clean up memory
  *curr = malloc(sizeof(struct Node));
  struct Node* t = *curr;
  t->elem = mystrdup(str);

  return t->elem;
}

char *gensym(const char *base) {
  char *s = calloc(strlen(base) + ndigits(gensym_ctr) + 1, 1);
  sprintf(s, "%s%" PRIu64, base, gensym_ctr++);
  return s;
}
