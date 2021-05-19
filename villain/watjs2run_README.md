## Creating a .wrun executable with the watjs2run executable. 

A %.wrun executable does the following: it runs the following command: node [working dir of watjs2run]/jsmain.js %.wasm.
The Node.js JavaScript runtime program (jsmain.js) takes the %.wasm file name as an argument and runs its WebAssembly 
code using the JavaScript API for WebAssembly. 

The %.wasm file is made from %.wat file by the make rule for %.wrun in the makewat make file. The same make rule executes 
watjs2run with the argument of %.wrun file name. The dependency of this make rule is the %.wat file which contains the 
WebAssembly code compiled from %.rkt program.

The watjs2run executable is made from watjs2run.c by 

```gcc wat2js2run.c -o wat2js2run```

The following describes how watjs2run executable creates the %.wrun file. 

In the make file makewat, in the rule for making %.wrun from %.wat, watjs2run is called with the argument $@ (%.wrun).  
With this argument, it first creates a C file with the name %.c, then runs the following command:

``gcc %.c -o %.wrun``

to make the %.wrun executable, and then deletes %.c.  

The %.c file has the following code:

```
#include <stdlib.h>

int main (int argc, char *argv[]) {
   system("node [working directory of watjs2run]/jsmain.js %.wasm");
}
```

Therefore, running the %.wrun executable will runs the Node.js file jsmain.js with the command-line argument %.wasm file name
and runs the Webassembly code of %.wasm in its environment using the Javascirpt API for WebAssembly.
