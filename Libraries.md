# Libraries

(Really, this should be called "standard libraries," becasue this extension does not allow for implementing and exporting arbitrary libraries.)

Standard libraries are programs that are specially pre-compiled to object files, and then linked to executable programs (during the executable's compilation). 

## Adding Standard Libraries

To add a new standard library, "mystdlib":
1. Create a new file in `villain/` called `mystdlib.rkt`.
2. Implement `villain/mystdlib.rkt` in the form `(begin (provide f1 f2 ... fn) (define f1 ...) (define f2 ...) ... (define fn ...))`, where `f1`, `f2`, ... `fn` are the functions provided by "mystdlib".
3. In `villain/externs.rkt", append the ids provide by "mystdlib" to the list of symbols defined by `stdlib-ids`.
4. In `villain/Makefile`:
	1. In the `stdlib` label, append the command `make mystdlib.o`.
	2. In the `runtime.o` label, add `stdlib.o` to the list of object files passed to `ld`'s `-r` option (right before the `-o` option)

After doing these steps, the ids provided by "mystdlib" should be available to all programs. Making an executable via `%.run` will automatically make all stdlibs as well, but you can also separately make all stdlibs via `make stdlib`.

As an example stdlib, the "list" stdlib is incorporated as follows:
- `villain/list.rkt` contains the source code, for example providing the `length` function.
- In `villain/externs.rkt`, the `stdlib-ids` list is extended with the "list" stdlib's provided functions.
- In `villain/Makefile`, the `stdlib` label is appended with `make list.o`.
- In `villain/Makefile`, in the `runtime.o` label, `ld`'s `-r` option's arguments is appended with `list.o`.
With all this done, the test `test/test-programs/villain/length.rkt` uses the "list" stdlib function `length`, and the executable can be successfully made with `make test/test-programs/villain/length.run`.

## Organization

Standard library ids similar to primitive ids. The important difference, of course, is that they cannot be treated as primitive when compiling the library source files themselves. One approach to addressing this would be the write an entire parallel compilation process for libraries. However, since it will be very similar in most ways to normal compilation, maintainance of both in parallel will be error and redundant. So, I have opted for a surgical approach of modifying just the parts of the normal compilation process that need to take into account the fact that libraries provide ids that would otherwise treated as external (note that using external ids in libraries cannot be completely disabled, since the standard libraries may depend on each other).

### `Makefile`

- command `std`. libraries are only compiled to object files, rather than to executables.
- command `runtime.o`. links to all `*.o` files in `std/`.

### `a86/ast.rkt`

- `label-decls`. External (via instruction `Extern`) labels count as declared.
- `label-uses`. Provided (via instruction `Global`) labels count as used.

### `villain/std.rkt`

- `std-provided?`. checks whether a symbol is provided by the std library.

### `villain/interp.rkt`

- `interp`. cannot interpret library
- `interp-env`. `'err` on library (unrecognized) function calls

### `villain/compile.rkt`

- TODO: arity checking for std library functions

### `villain/externs.rkt`

- `externs-f`. include `Extern` for function call (with `symbol->label`, so that calling this function looks just like calling any other function) if it is a std library function
- `symbol->label`. moved here from `compile.rkt`.