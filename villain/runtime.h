#ifndef VILLAIN_RUNTIME_H
#define VILLAIN_RUNTIME_H

extern int64_t entry();
extern FILE* in;
extern FILE* out;
extern void (*error_handler)();

// in words
#define heap_size 10000
extern int64_t *heap;

#endif
