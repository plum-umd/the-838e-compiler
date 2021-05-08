## Modifications in this build of wasmtime

The wasmtime executable in this folder is a modified version of wasmtime. It includes three additional functions, print_codepoint, print_int, and error_exit as compared with the standard wasmtime.  This version was built by making the following changes to the following files in a git clone of wasmtime repository (following the method by Radu Matei in https://radu-matei.com/blog/adding-wasi-syscall/ with some changes for the newer version) and then executing the command cargo build --release in the wasmtime directory:

1 - In wasmtime\crates\wasi-common\src\snapshots\preview_1.rs the following functions were added in lines 1101-1122 in implementation 
impl<'a> wasi_snapshot_preview1::WasiSnapshotPreview1 for WasiCtx { } :

    ```rust
    fn print_codepoint(&self, fd: types::Fd) -> Result<(), Error> {
        let y = u32::from(fd);
        let c = std::char::from_u32(y);
        match c {
            Some(x) =>  print!("{}", x),
            None => print!("none"),
        }
        Ok(())
    }

    fn print_int(&self, fd: types::Fd) -> Result<(), Error> {
        let y = u32::from(fd);
        let x = y as i32;
        print!("{}", x);
        Ok(())
    }

    fn error_exit(&self) -> Result<(), Error> {
        println!("'err");
        std::process::exit(1);
    } and wasmtime\crates\wasi-common\src\snapshots\preview_0.rs
    ```

2 - In wasmtime\crates\wasi-common\src\snapshots\preview_0.rs the same code was added in lines 926-947, in addition to the following code in lines 331-333 in use of macro rule convert_flags_bidirectional!(types::Rights, snapshot1_types::Rights, ...) :

    ```rust
    PRINT_CODEPOINT,
    PRINT_INT,
    ERROR_EXIT
    ```

3 - In wasmtime\crates\wasi-common\WASI\phases\old\snapshot_0\witx\wasi_unstable.witx the following code was added in lines 512-527 in (module $wasi_unstable ... ):

  ```rust
  ;;; Print a codepoint character.
  (@interface func (export "print_codepoint")
    (param $fd $fd)
    (result $error (expected (error $errno)))
  )

  ;;; Print an i32 int.
  (@interface func (export "print_int")
    (param $fd $fd)
    (result $error (expected (error $errno)))
  )

  ;;; Print an i32 int.
  (@interface func (export "error_exit")
    (result $error (expected (error $errno)))
  )
  ```

4 - In wasmtime\crates\wasi-common\WASI\phases\snapshot\witx\wasi_snapshot_preview1.witx the same code was added in lines 509-524.

5 - In wasmtime\crates\wasi-common\WASI\phases\old\snapshot_0\witx\typenames.witx the following code was added in lines 273-278 in  ;;; File descriptor rights, determining which actions may be performed. (typename $rights (flags (@witx repr u64) ... )) :

    ```rust
    ;;; The right to invoke `print_codepoint`.
    $print_codepoint
    ;;; The right to invoke `print_int`.
    $print_int
    ;;; The right to invoke `error_exit`.
    $error_exit
    ```

6 - In wasmtime\crates\wasi-common\WASI\phases\snapshot\witx\typenames.witx the same code was added in lines 273-278.

The git clone of wasmtime repository is not included. The wasmtime repository's address is: https://github.com/bytecodealliance/wasmtime and it can be cloned with the following command:  

git clone --recurse-submodules https://github.com/bytecodealliance/wasmtime.git

The rest of this README file is the documentation when the standard wasmtime is installed in a .wasmtime folder with the following command:

```sh
$ curl https://wasmtime.dev/install.sh -sSf | bash```



<div align="center">
  <h1><code>wasmtime</code></h1>

  <p>
    <strong>A standalone runtime for
    <a href="https://webassembly.org/">WebAssembly</a></strong>
  </p>

  <strong>A <a href="https://bytecodealliance.org/">Bytecode Alliance</a> project</strong>

  <p>
    <a href="https://github.com/bytecodealliance/wasmtime/actions?query=workflow%3ACI"><img src="https://github.com/bytecodealliance/wasmtime/workflows/CI/badge.svg" alt="build status" /></a>
    <a href="https://bytecodealliance.zulipchat.com/#narrow/stream/217126-wasmtime"><img src="https://img.shields.io/badge/zulip-join_chat-brightgreen.svg" alt="zulip chat" /></a>
    <img src="https://img.shields.io/badge/rustc-stable+-green.svg" alt="supported rustc stable" />
    <a href="https://docs.rs/wasmtime"><img src="https://docs.rs/wasmtime/badge.svg" alt="Documentation Status" /></a>
  </p>

  <h3>
    <a href="https://bytecodealliance.github.io/wasmtime/">Guide</a>
    <span> | </span>
    <a href="https://bytecodealliance.github.io/wasmtime/contributing.html">Contributing</a>
    <span> | </span>
    <a href="https://wasmtime.dev/">Website</a>
    <span> | </span>
    <a href="https://bytecodealliance.zulipchat.com/#narrow/stream/217126-wasmtime">Chat</a>
  </h3>
</div>

## Installation

The Wasmtime CLI can be installed on Linux and macOS with a small install
script:

```sh
$ curl https://wasmtime.dev/install.sh -sSf | bash
```

Windows or otherwise interested users can download installers and
binaries directly from the [GitHub
Releases](https://github.com/bytecodealliance/wasmtime/releases) page.

## Example

If you've got the [Rust compiler
installed](https://www.rust-lang.org/tools/install) then you can take some Rust
source code:

```rust
fn main() {
    println!("Hello, world!");
}
```

and compile/run it with:

```sh
$ rustup target add wasm32-wasi
$ rustc hello.rs --target wasm32-wasi
$ wasmtime hello.wasm
Hello, world!
```

## Features

* **Lightweight**. Wasmtime is a standalone runtime for WebAssembly that scales
  with your needs. It fits on tiny chips as well as makes use of huge servers.
  Wasmtime can be [embedded] into almost any application too.

* **Fast**. Wasmtime is built on the optimizing [Cranelift] code generator to
  quickly generate high-quality machine code at runtime.

* **Configurable**. Whether you need to precompile your wasm ahead of time,
  generate code blazingly fast with Lightbeam, or interpret it at runtime,
  Wasmtime has you covered for all your wasm-executing needs.

* **WASI**. Wasmtime supports a rich set of APIs for interacting with the host
  environment through the [WASI standard](https://wasi.dev).

* **Standards Compliant**. Wasmtime passes the [official WebAssembly test
  suite](https://github.com/WebAssembly/testsuite), implements the [official C
  API of wasm](https://github.com/WebAssembly/wasm-c-api), and implements
  [future proposals to WebAssembly](https://github.com/WebAssembly/proposals) as
  well. Wasmtime developers are intimately engaged with the WebAssembly
  standards process all along the way too.

[Cranelift]: https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/README.md
[embedded]: https://bytecodealliance.github.io/wasmtime/lang.html

## Language Support

You can use Wasmtime from a variety of different languages through embeddings of
the implementation:

* **[Rust]** - the [`wasmtime` crate]
* **[C]** - the [`wasm.h`, `wasi.h`, and `wasmtime.h` headers][c-headers]
* **[Python]** - the [`wasmtime` PyPI package]
* **[.NET]** - the [`Wasmtime` NuGet package]
* **[Go]** - the [`wasmtime-go` repository]

[Rust]: https://bytecodealliance.github.io/wasmtime/lang-rust.html
[C]: https://bytecodealliance.github.io/wasmtime/examples-c-embed.html
[`wasmtime` crate]: https://crates.io/crates/wasmtime
[c-headers]: https://bytecodealliance.github.io/wasmtime/c-api/
[Python]: https://bytecodealliance.github.io/wasmtime/lang-python.html
[`wasmtime` PyPI package]: https://pypi.org/project/wasmtime/
[.NET]: https://bytecodealliance.github.io/wasmtime/lang-dotnet.html
[`Wasmtime` NuGet package]: https://www.nuget.org/packages/Wasmtime
[Go]: https://bytecodealliance.github.io/wasmtime/lang-go.html
[`wasmtime-go` repository]: https://pkg.go.dev/github.com/bytecodealliance/wasmtime-go

## Documentation

[📚 Read the Wasmtime guide here! 📚][guide]

The [wasmtime guide][guide] is the best starting point to learn about what
Wasmtime can do for you or help answer your questions about Wasmtime. If you're
curious in contributing to Wasmtime, [it can also help you do
that][contributing]!

[contributing]: https://bytecodealliance.github.io/wasmtime/contributing.html
[guide]: https://bytecodealliance.github.io/wasmtime

---

It's Wasmtime.
