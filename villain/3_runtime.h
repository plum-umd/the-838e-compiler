int64_t entry();
FILE* in;
FILE* out;
void (*error_handler)();

// in words
#define heap_size 1200000
int64_t *heap;
