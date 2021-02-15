# Libraries

Really, this should be called "standard libraries," becasue this extension does not allow for implementing and exporting arbitrary libraries.

## Organization

Standard library ids similar to primitive ids. The important difference, of course, is that they cannot be treated as primitive when compiling the library source files themselves. One approach to addressing this would be the write an entire parallel compilation process for libraries. However, since it will be very similar in most ways to normal compilation, maintainance of both in parallel will be error and redundant. So, I have opted for a surgical approach of modifying just the parts of the normal compilation process that need to take into account the fact that libraries provide ids that would otherwise treated as external (note that using external ids in libraries cannot be completely disabled, since the standard libraries may depend on each other).

### `Makefile`

- command `std`. libraries are only compiled to object files, rather than to executables.
- command `runtime.o`. links to all `*.o` files in `std/`.

### `a86/ast.rkt`

- `label-uses`. Provided labels count as used.

### `villain/interp.rkt`

- `interp` cannot interpret library
- `interp-env`. `'err` on library (unrecognized) function calls

### `villain/compile.rkt`

- TODO

### `villain/externs.rkt`

### `villain/parse.rkt`

