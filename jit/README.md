# Final Project for CMSC 838E: Partial Evaluator for Iniquity

This repository contains the source code for the partial evaluator developed by Olasubomi Efuniyi and Vyas Gupta. It is the initial steps towards developing a JIT compiler similar to the compiler developed by Ancona, et al., which can be found [here](https://www.researchgate.net/publication/252023163_Automatic_generation_of_JIT_compilers_for_dynamic_languages_in_NET).

## Installing

First, clone the repository, checkout the `jit` branch, then use `raco` to install the package:

```
git clone git@github.com:plum-umd/the-838e-compiler.git
git checkout jit
cd the-838e-compiler
raco pkg install jit/
```

## Testing

To test the repository, 

```
cd the-838e-compiler/jit
raco test test/
```

## Usage

Underneath this directory, there are three paths to running your program: interpreter, compiler, partial-evaluator. Using the interpreter or the compiler works similarly to running the compiler or interpreter for the Villain interpreter and compiler. To use the partial evaluator, use the `eval-file` from `eval-file.rkt`. This function will take a filename as input and a output to the file named in the second parameter. Then, you can run evaluate your program using `racket [filename]`.

If you would like to check the benchmarks, then we provide a script to run the compiler, interpreter and partial evaluator on six example programs provided in the `benchmark` directory. Feel free to add more example programs as `.rkt` files in the `benchmark/original` directory. Then, use these commands to run the benchmarking script:

```
cd the-838e-compiler/jit/benchmark
sh time.sh
```

The script will make the necessary calls to use the interpreter, compiler and partial evaluator, and return the length of runtime for each implementation.
