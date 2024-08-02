# Goldfish Scheme
Goldfish Scheme is a Scheme interpreter with the following features:
+ R7RS compatible
+ SRFI provided
+ Small and fast

## Installation
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
Notice, the FILE and the eval result is separated by ` => `.

## R7RS Standard Libraries
| Library | Description | 
|-----|-------|
| `(scheme base)` | Base routines |
| `(scheme case-lambda)` | Provide `case-lambda` |
| `(scheme file)` | File operations |

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

