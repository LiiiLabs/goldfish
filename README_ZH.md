# 金鱼Scheme / [Goldfish Scheme](README.md)
> 让Scheme和Python一样易用且实用！

金鱼Scheme 是一个 Scheme 解释器，具有以下特性：
+ 兼容 R7RS-small 标准
+ 提供类似 Python 的标准库
+ 小巧且快速

## 以简为美
金鱼Scheme仍旧遵循和 S7 Scheme 一样的简约的原则。目前，它仅依赖于 [S7 Scheme](https://ccrma.stanford.edu/software/s7/) 、[tbox](https://gitee.com/tboox/tbox) 和 C++98 范围内的标准库。

与 S7 Scheme 类似，[src/goldfish.hpp](src/goldfish.hpp) 和 [src/goldfish.cpp](src/goldfish.cpp) 是构建金鱼Scheme解释器二进制文件所需的唯一关键源代码。

## 标准库
### 金鱼标准库
形如`(liii xyz)`的是金鱼标准库，模仿Python标准库的函数接口和实现方式，降低用户的学习成本。

| 库 | 描述 | 示例函数 |
| --- | --- | --- |
| [(liii base)](goldfish/liii/base.scm) | 基础库 | `==`, `!=`, `display*` |
| [(liii error)](goldfish/liii/error.scm) | 提供类似Python的错误函数 | `os-error`函数抛出`'os-error`，类似Python的OSError |
| [(liii check)](goldfish/liii/check.scm) | 基于SRFI 78的轻量级测试库加强版 | `check`, `check-catch` |
| [(liii case)](goldfish/liii/case.scm) | 模式匹配 | `case*` |
| [(liii list)](goldfish/liii/list.scm) | 列表函数库 | `list-view`, `fold` |
| [(liii bitwise)](goldfish/liii/bitwise.scm) | 位运算函数库 | `bitwise-and`, `bitwise-or` |
| [(liii string)](goldfish/liii/string.scm) | 字符串函数库 | `string-join` |
| [(liii vector)](goldfish/liii/vector.scm) | 向量函数库 | `vector-index` |
| [(liii stack)](goldfish/liii/stack.scm) | 栈 | `stack-pop!`, `stack-push!` |
| [(liii hash-table)](goldfish/liii/hash-table.scm) | 哈希表 | `hash-table-empty?`, `hash-table-contains?` | 
| [(liii sys)](goldfish/liii/sys.scm) | 库类似于 Python 的 `sys` 模块 | `argv` |
| [(liii os)](goldfish/liii/os.scm) | 库类似于 Python 的 `os` 模块 | `getenv`, `mkdir` |
| [(liii path)](goldfish/liii/path.scm) | 路径函数库 | `path-dir?`, `path-file?` |
| [(liii uuid)](goldfish/liii/uuid.scm) | UUID 生成 | `uuid4` |

### SRFI

| 库 | 状态 | 描述 |
| --- | --- | --- |
| `(srfi srfi-1)` | 部分 | 列表库 |
| `(srfi srfi-8)` | 完整 | 提供 `receive` |
| `(srfi srfi-9)` | 完整 | 提供 `define-record-type` |
| `(srfi srfi-13)` | 完整 | 字符串库 |
| `(srfi srfi-16)` | 完整 | 提供 `case-lambda` |
| `(srfi srfi-39)` | 完整 | 参数对象 |
| `(srfi srfi-78)` | 部分 | 轻量级测试框架 |
| `(srfi srfi-125)` | 部分 | 哈希表 |
| `(srfi srfi-133)` | 部分 | 向量 |
| `(srfi srfi-151)` | 部分 | 位运算 |
| `(srfi srfi-216)` | 部分 | SICP |


### R7RS 标准库

| 库 | 描述 |
| --- | --- |
| `(scheme base)` | 基础库 |
| `(scheme case-lambda)` | 提供 `case-lambda` |
| `(scheme char)` | 字符函数库 |
| `(scheme file)` | 文件操作 |
| `(scheme time)` | 时间库 |

## 安装
金鱼Scheme自 v1.2.8 起已集成在墨干理工套件中，只需[安装墨干](https://mogan.app/zh/guide/Install.html)即可安装 金鱼Scheme。

除了金鱼Scheme解释器外，墨干还提供了一个结构化的[金鱼Scheme REPL](https://mogan.app/guide/plugin_goldfish.html)。

以下是在 Debian bookworm 上构建的命令行指南：

```bash
sudo apt install xmake git unzip curl g++
git clone https://gitee.com/LiiiLabs/goldfish.git
# git clone https://github.com/LiiiLabs/goldfish.git
cd goldfish
xmake b goldfish
bin/goldfish --version
```

您也可以将其安装到 `/opt`：

```bash
sudo xmake i -o /opt/goldfish --root
/opt/goldfish/bin/goldfish
```

卸载时只需：

```bash
sudo rm -rf /opt/goldfish
```

## 命令行技巧
本节假设您已成功执行 `xmake b goldfish` 并且 `bin/goldfish` 可用。

### 零选项
不使用任何选项时，它将打印帮助信息：

```
> bin/goldfish
Goldfish Scheme 17.10.0 by LiiiLabs
--version       display version
-e              -e '(+ 1 2)'
-l FILE         Load the scheme code on path
FILE            Load the scheme code on path and print the evaluated result
```

### 版本选项
`--version` 将打印 金鱼Scheme 的版本和 S7 Scheme 的版本：

```
> bin/goldfish --version
Goldfish Scheme 17.10.0 by LiiiLabs
based on S7 Scheme 10.11 (2-July-2024)
```

### 模式选项
`-m`帮助您指定预加载的标准库。

+ `default`: `-m default`等价于`-m liii`
+ `liii`: 预加载`(liii base)`和`(liii error)`的金鱼Scheme
+ `sicp`: 预加载`(srfi sicp)`和`(scheme base)`的S7 Scheme
+ `r7rs`: 预加载`(scheme base)`的S7 Scheme
+ `s7`: 无任何无加载库的S7 Scheme

### 其它选项
`-e` 帮助您即时求值 Scheme 代码：

```
> bin/goldfish -e "(+ 1 2)"
3
> bin/goldfish -e "(begin (import `srfi srfi-1`) (first (list 1 2 3)))"
1
> bin/goldfish -e "(begin (import `liii sys`) (display (argv)) (newline))" 1 2 3
("bin/goldfish" "-e" "(begin (import `liii sys`) (display (argv)) (newline))" "1" "2" 3)
#\newline
```

`-l` 帮助您加载文件：

```
> bin/goldfish -l tests/demo_error.scm

;car argument, (), is nil but should be a pair
;    (list)
;    tests/demo_error.scm, line 1, position: 10
; (list)

> bin/goldfish -l tests/demo_no_error.scm
> bin/goldfish -l tests/demo_argv.scm 1 2 3
("bin/goldfish" "tests/demo_argv.scm" "1" "2" "3")
```

如果没有提供选项，它将加载文件并打印求值结果：

```
> bin/goldfish tests/demo_no_error.scm
tests/demo_no_error.scm => 3
> bin/goldfish tests/demo_error.scm

;car argument, (), is nil but should be a pair
;    (list)
;    tests/demo_error.scm, line 1, position: 10
; (list)

tests/demo_error.scm => wrong-type-arg
> bin/goldfish tests/demo_argv.scm 1 2 3
("bin/goldfish" "tests/demo_argv.scm" "1" "2" "3")
tests/demo_argv.scm => #\newline
```

注意，文件和求值结果之间用 `=>` 分隔。

## 版本命名规则
金鱼Scheme `x.y.z` 表示它使用的是 tbox 版本 `x`，基于 S7 Scheme `y`，而 `z` 是补丁版本。例如，金鱼Scheme 的第二个版本是 `17.10.1`，表示它使用`tbox 1.7.x`，基于 `S7 Scheme 10.x`，补丁版本是 `1`。

## 为什么我们创建了金鱼Scheme
金鱼Scheme 是为了克服 S7 Scheme 的缺陷而实现的：
1. 在 Linux/macOS/Windows 上分发无须编译即可安装的金鱼Scheme解释器和结构化的REPL
2. 尝试实现 [R7RS-small](https://small.r7rs.org) 标准
3. 尝试以 R7RS 库格式提供有用的 SRFI


## 许可证
金鱼Scheme 根据 Apache 2.0 许可证授权，一些源自 S7 Scheme 和 SRFI 的代码片段已在相关源文件中明确声明。

## 引用

读者可以用以下的BibTeX代码来引用我们的工作.

```
@book{goldfish,
    author = {Da Shen and Nian Liu and Yansong Li and Duolei Wang and Leiyu He and Yingyao Zhou and Shuting Zhao and Shen Wei},
    title = {Goldfish Scheme: A Scheme Interpreter with Python-Like Standard Library},
    publisher = {LIII NETWORK},
    year = {2024},
    url = {https://gitee.com/LiiiLabs/goldfish/releases/download/v17.10.9/Goldfish.pdf}
}
```
