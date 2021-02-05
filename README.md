# Villain: the CMSC 838E Compiler

This repository contains the source code for Villain: the Racket
compiler developed in CMSC 838E at the University of Maryland, College
Park.

![Image of Villain](https://external-preview.redd.it/IKpBn5nc0EsiKGW9oK9715diei5UWi3WVWe1l7IZ9iY.png?auto=webp&s=034d509d75c9809e37ef52ba037eda3391185741)

## Installing

First, clone the repository, the use `raco` to install the package:

```
git clone git@github.com:plum-umd/the-838e-compiler.git
raco pkg install the-838e-compiler/
```

To test:
```
raco test -p the-838e-compiler
```

## Contributing

To contribute, create a branch (e.g. `string-dev`), and set the remote as upstream:
```
cd the-838e-compiler
git checkout -b 'string-dev'
git push --set-upstream origin string-dev
```
You can commit and push changes.  When you're ready for the changes to be merged into `main`, submit a pull request.
