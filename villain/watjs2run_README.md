## Creating a .wrun executable with watjs2run executable. 

A %.wrun executable does the following: it makes a %.wasm file from a WebAssembly %.wat file (compiled with compile-wasm.rkt) and then runs it with a node.js Javascript runtime program.  The following describes how watjs2run executable creates the %.wrun file. 

The watjs2run executable is made from watjs2run.c by 

```gcc wat2js2run.c -o wat2js2run```

In the make file makewat, in the rule for making %.wrun from %.wat, watjs2run is called with the argument $@ (%.wrun).  With this argument, it first creates a C file with the name %.c, then runs the following command:

``gcc %.c -o %.wrun``

to make the %.wrun executable, and then deletes %.c.  

The %.c file has the following code:

```
#include <stdlib.h>

int main (int argc, char *argv[]) {
   system("wat2wasm %.wat -o %.wasm");
   system("node (working dir of watjs2run)/jsmain.js %.wasm");
}
```

Therefore, running the %.wrun executable will do the following: it first creates the %.wasm file from %.wat file with wat2wasm utility from wabt, and then runs the node.js file jsmain.js with the command-line argument %.wasm with node.js JavaScript runtime.
