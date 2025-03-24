;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;


;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;
(import (liii check)
  (srfi srfi-132))
(display "Testing vector-sorted?\n")

; 测试用例 1：整个向量已排序
(check-true (vector-sorted? < #(1 2 3 4 5)))

; 测试用例 2：整个向量未排序
(check-false (vector-sorted? < #(1 3 2 4 5)))

; 测试用例 3：子向量已排序（从索引 1 开始）
(check-true (vector-sorted? < #(5 1 2 3 4) 1))

; 测试用例 4：子向量未排序（从索引 1 开始）
(check-false (vector-sorted? < #(5 1 3 2 4) 1))

; 测试用例 5：子向量已排序（从索引 1 到 3）
(check-true (vector-sorted? < #(5 1 2 3 4) 1 3))

; 测试用例 6：子向量未排序（从索引 1 到 3）
(check-false (vector-sorted? < #(5 1 3 2 4) 1 4))

; 测试用例 7：无效的起始和结束参数
(check-catch "Invalid start or end parameters"
  (vector-sorted? < #(1 2 3 4 5) 3 2))

;; 测试 list-merge!
(display "Testing list-merge!\n")

;; 测试用例 1：合并两个已排序的列表
(define lis1 '(1 3 5))
(define lis2 '(2 4 6))
(test (list-merge! < lis1 lis2) '(1 2 3 4 5 6))

; 测试用例 2：合并包含重复元素的列表
(define lis3 '(1 1 3))
(define lis4 '(1 2 4))
(test (list-merge! < lis3 lis4) '(1 1 1 2 3 4))

; 测试用例 3：合并空列表
(define lis5 '())
(define lis6 '(1 2 3))
(test (list-merge! < lis5 lis6) '(1 2 3))
(test (list-merge! < lis6 lis5) '(1 2 3))

; 测试用例 4：合并两个空列表
(define lis7 '())
(define lis8 '())
(test (list-merge! < lis7 lis8) '())

; 生成测试报告
(check-report)

