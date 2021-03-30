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

Check the [Issues](https://github.com/plum-umd/the-838e-compiler/issues) list for
possible things to work on.  If you'd like to work on something else, either discuss
it on Discord or submit a new issue.

When you decide to work on an issue, assign yourself to it so others know you are
working on it.  Several people may decide to work on the same issue.  You can
coordinate your efforts on Discord.  You can either work together or work on separate
branches developing alternate solutions.

## Dependencies

Code emitted by the compiler depends upon the following libraries:

* [`libunistring`](https://www.gnu.org/software/libunistring/)

  To install this library, you can `brew install libunistring-dev` on
  macOS (with Homebrew) or `apt-get install libunistring-dev` on
  Ubuntu, or you can download and compile the source code.

  If you are using Arch Linux, the `libunistring` package does not come with
  the static library of `libunistring` (suffixed `.a`). You need to build it
  from source by

  ```console
  $ curl -OL https://raw.githubusercontent.com/archlinux/svntogit-packages/packages/libunistring/trunk/PKGBUILD
  $ echo "options=('staticlibs')" >> PKGBUILD
  $ makepkg -si
  ```

* [`GMP`](https://gmplib.org/)

  To install this library, you can download and compile the source code from this [`website`](https://gmplib.org/). Following unzipping, below are the instructions to install GMP in a Unix-like environment.

  ```console
  $ cd gmp-6.2.1 (or whatever release)
  $ ./configure
  $ make
  $ make check
  $ make install
  ```
## Reference

- [standard libraries](stdlibs.md)
