# What is this?
This is statically typed low level language I built for fun.


# Features
- Defer
- Runtime type-info
- Order-independent declarations.
- Type inference
- Compatible with the C ABI
- ...


# Limitations
It's only been a month since I started working on this, so I didn't have time to do a lot
of things like:
- mutability semantics
- private symbols (all the functions and global variables are public and that can cause name collisions)
- packages
- unammed structures and unions
- IR and machine code generation (the compiler can only generate c for now).


# Example
Here is how a fibonacci function look like in this language
```
fn fib(n: u64): u64 {
    if n < 2 {
        return n;
    }

    return fib(n - 1) + fib(n - 2);
}
```
For more information about the language you can take look at the [demo](examples/demo) file in the examples folder. The example folder contains some working examples as well.


# How to build this project
## Requirements
A working version of clang that support c11.<br>
## Build steps
To build the project follow these steps:
- Run the `build.sh` in linux or `build.bat` on windows.
- Add the generated build directory into the path variable.
- Run `compiler -help` command to see if everything is working.

The project builds correctly in windows, I didn't test it on linux yet.


# Resources
here is some resources that I found usefull
- [compilerbook](https://www3.nd.edu/~dthain/compilerbook/compilerbook.pdf) : a small (~250pages) but really good book about compilers design.
- [bitwise](https://www.youtube.com/playlist?list=PLU94OURih-CiP4WxKSMt3UcwMSDM3aTtX) : a project about building a complete software and hardware stack including a compiler by [Per Vognsen](https://mastodon.social/@pervognsen)
- open source languges: 
    - [hcc](https://github.com/heroseh/hcc)
    - [odin](https://odin-lang.org/)
    - [vlang](https://vlang.io/)