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

