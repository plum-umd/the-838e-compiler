# Standard Libraries (stdlibs)

Standard libraries are programs that are specially pre-compiled to object files,
and then linked to executable programs (during the executable's compilation).

## Adding Standard Libraries

To add a new standard library, "mystdlib":

1. Create a new file in `villain/lib` called `mystdlib.rkt`.
2. Implement `villain/lib/mystdlib.rkt` in the form

```racket
#lang racket
(provide f1 ...)
(define (f1 ...) ...)
...
```
where `f1`, ... are the functions provided by "mystdlib".

After doing these steps, the ids provided by "mystdlib" should be available to
all programs.

As an example stdlib, the "list" stdlib is incorporated as follows:

- `villain/list.rkt` contains the source code, for example providing the
  `length` function.

## Organization

Standard library ids similar to primitive ids. The important difference, of
course, is that they cannot be treated as primitive when compiling the library
source files themselves. One approach to addressing this would be the write an
entire parallel compilation process for libraries. However, since it will be
very similar in most ways to normal compilation, maintainance of both in
parallel will be error and redundant. So, I have opted for a surgical approach
of modifying just the parts of the normal compilation process that need to take
into account the fact that libraries provide ids that would otherwise treated as
external (note that using external ids in libraries cannot be completely
disabled, since the standard libraries may depend on each other).

### `a86/ast.rkt`

- `label-decls`. External (via instruction `Extern`) labels count as declared.
- `label-uses`. Provided (via instruction `Global`) labels count as used.

### `villain/externs.rkt`

- `externs-f`. include `Extern` for function call (with `symbol->label`, so that
  calling this function looks just like calling any other function) if it is a
  std library function
- `symbol->label`. moved here from `compile.rkt`.
- `stdlib-provided?`. checks whether a symbol is provided by the std library.
- `stdlib-ids`. list of each id provided by a stdlib

## To-Do List

- [ ] "bool" stdlib (according to https://docs.racket-lang.org/reference/booleans.html)
- [ ] "string" stdlib (according to https://docs.racket-lang.org/reference/strings.html)
- [ ] "list" stdlib (according to https://docs.racket-lang.org/reference/pairs.html)
- [ ] "math" stdlib (according to https://docs.racket-lang.org/reference/generic-numbers.html)
- [ ] recognizing stdlib functions passed as higher-order arguments (in the
      `Var` struct somehow)
