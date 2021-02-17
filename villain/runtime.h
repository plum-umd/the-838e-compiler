#ifndef VILLAIN_RUNTIME_H
#define VILLAIN_RUNTIME_H

extern int64_t entry();
extern FILE* in;
extern FILE* out;
extern void (*error_handler)();

// in words
#define heap_size 10000
//#define heap_size 0 // for testing
#define heap_buffer 3 // number of bytes to have as buffer at end of heap. Used to detect heap overflow.
extern int64_t *heap;

#endif
