# 金鱼标准库
[金鱼Scheme](https://gitee.com/LiiiLabs/goldfish)是三鲤发起的一个基于S7 Scheme的Scheme解释器。

金鱼标准库分为：
+ R7RS标准库
+ 部分Scheme SRFI的金鱼Scheme实现
+ 金鱼扩展库

## R7RS标准库
| 模块 | 功能 | 
|-----|-------|
| (scheme case-lambda) | 提供`case-lambda` |

## Scheme SRFI

| 模块 | 状态 | 功能  |
|------|------|-------|
| (srfi srfi-1)   | 部分实现 | 列表函数库 |
| (srfi srfi-8)   | 完整实现 | 提供`receive` |
| (srfi srfi-9)   | 完整实现 | 提供`define-record-type` |
| (srfi srfi-16)  | 完整实现 | 提供`case-lambda`，是`(scheme case-lambda)`的接口 |
| (srfi srfi-39)  | 完整实现 | 参数化对象 |
| (srfi srfi-78)  | 部分实现 | 轻量级测试框架`check` |

