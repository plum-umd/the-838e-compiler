## Additions in this build of wasmtime

The wasmtime executable in the bin folder is a modified version of wasmtime. It includes three additional functions, print_codepoint, print_int, and error_exit as compared with the standard wasmtime.  This version was built by making the following changes to the following files in a git clone of wasmtime repository (following the method by Radu Matei in https://radu-matei.com/blog/adding-wasi-syscall/ with some changes for the newer version) and then executing the command cargo build --release in the wasmtime directory:

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

The README.md file in this folder is the documentation when the standard wasmtime is installed in a .wasmtime folder with the following command:

```sh
$ curl https://wasmtime.dev/install.sh -sSf | bash
```
