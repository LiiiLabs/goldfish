# Goldfish Scheme
Goldfish Scheme is a Scheme interpreter with the following features:
+ R7RS-small compatible
+ SRFI provided
+ Small and fast

## Why we created Goldfish Scheme
Goldfish Scheme is implemented to overcome the defects of [S7 Scheme](https://ccrma.stanford.edu/software/s7/):
1. Distribute the ready-to-use Goldfish Scheme interpreter and structured REPL on Linux/macOS/Windows
2. Try to implement the [R7RS-small](https://small.r7rs.org) standard
3. Try to provide the useful SRFI in R7RS library format

The implementation of Goldfish aims to follow the rules of simplicity. Currently, Goldfish Scheme only depends on S7 Scheme, tbox and C++ 17 standard library.

Just like S7 Scheme, [`src/goldfish.hpp`](src/goldfish.hpp) and [`src/goldfish.cpp`](src/goldfish.cpp) are the only key source code needed to build the goldfish interpreter binary.


## Installation
Goldfish Scheme is bundled in Mogan Research (since v1.2.8), just [install Mogan Research](https://mogan.app/guide/Install.html) to install Goldfish Scheme.

Besides the Goldfish Scheme interpreter, a nice structured [Goldfish Scheme REPL](https://mogan.app/guide/plugin_goldfish.html) is availabe in Mogan Research.

The following guide will help you build and install Goldfish step by step.

### GNU/Linux
Here are commandlines to build it on Debian bookworm:
```
sudo apt install xmake git unzip curl build-essential
git clone https://gitee.com/LiiiLabs/goldfish.git
# git clone https://github.com/LiiiLabs/goldfish.git
cd goldfish
xmake b goldfish
bin/goldfish --version
```
You can also install it to `/opt`:
```
sudo xmake i -o /opt/goldfish --root
/opt/goldfish/bin/goldfish
```
For uninstallation, just:
```
sudo rm -rf /opt/goldfish
```

## Commandlinefu
This section assumes you have executed `xmake b goldfish` sucessfully and `bin/goldfish` is available.

Without any options, it will print the help message:
```
> bin/goldfish 
Goldfish Scheme 17.10.0 by LiiiLabs
--version       display version
-e              -e '(+ 1 2)'
-l FILE         Load the scheme code on path
FILE            Load the scheme code on path and print the evaluated result
```

`--version` will print the Goldfish Scheme version and the underlying S7 Scheme version:
```
> bin/goldfish --version
Goldfish Scheme 17.10.0 by LiiiLabs
based on S7 Scheme 10.11 (2-July-2024)
```

`-e` helps you evaluate the scheme code on the fly:
```
> bin/goldfish -e "(+ 1 2)"
3
> bin/goldfish -e "(begin (import (srfi srfi-1)) (first (list 1 2 3)))"
1
```

`-l` helps you load the FILE:
```
> bin/goldfish -l tests/demo_error.scm 

;car argument, (), is nil but should be a pair
;    (list)
;    tests/demo_error.scm, line 1, position: 10
; (list)

> bin/goldfish -l tests/demo_no_error.scm
>
```

If no options provided, it will load the FILE and print the eval result:
```
> bin/goldfish  tests/demo_no_error.scm 
tests/demo_no_error.scm => 3
> bin/goldfish  tests/demo_error.scm 

;car argument, (), is nil but should be a pair
;    (list)
;    tests/demo_error.scm, line 1, position: 10
; (list)

tests/demo_error.scm => wrong-type-arg
```
Notice, the FILE and the eval result are separated by ` => `.

## R7RS Standard Libraries
| Library | Description | 
|-----|-------|
| `(scheme base)` | Base library |
| `(scheme case-lambda)` | Provide `case-lambda` |
| `(scheme file)` | File operations |
| `(scheme time)` | Time library |

## SRFI

| Library | Status | Description  |
|------|------|-------|
| `(srfi srfi-1)`   | Part | List Library |
| `(srfi srfi-8)`   | Complete | Provide `receive` |
| `(srfi srfi-9)`   | Complete | Provide `define-record-type` |
| `(srfi srfi-13)`  | Complete | String Library | 
| `(srfi srfi-16)`  | Complete | Provide `case-lambda` |
| `(srfi srfi-39)`  | Complete | Parameter objects |
| `(srfi srfi-78)`  | Part | Lightweigted Test Framework |

## Versioning
Goldfish Scheme x.y.z means that it is using the C++ Standard x, based on S7 Scheme y, and z is the patch version. To clarify, the first version of Goldfish
Scheme is `17.10.0`, it means that it is using the `C++ 17` standard, based on `S7 Scheme 10.x`, the patch version is `0`.

## License
Goldfish Scheme is licensed under Apache 2.0, for some of the code snippets which are derived from the S7 Scheme repo and SRFI have been explicited claimed in the related source files.


