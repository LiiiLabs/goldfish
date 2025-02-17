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

(import (liii red-black-tree) (liii list) (liii check))

(check-set-mode! 'report-failed)

(define (int-cmp x y)
  (cond
    ((= x y) 'eq)
    ((< x y) 'lt)
    (else 'gt)))

(define (construct lst)
  (fold-right (lambda (x t) (red-black-tree-insert t x x))
              (make-red-black-tree int-cmp) lst))
       
(define elements '(5 8 2 9 3 7 0 1 6))
(define tree (construct elements))

(for-each (lambda (x) (check (red-black-tree-find tree x) => x)) 
          elements)

(check-false (red-black-tree-empty? tree))
(check-true (red-black-tree? tree))

(check-false (red-black-tree-find tree 10))

(for-each (lambda (x) (define new-tree (red-black-tree-delete tree x))
                      (check-false (red-black-tree-find new-tree x))
                      (for-each (lambda (y) 
                                        (when (not (= x y))
                                            (check (red-black-tree-find new-tree y) => y))) elements)) 
  elements)


(define tree2 (red-black-tree-insert (red-black-tree-delete tree 6) 2 10))

(check (red-black-tree-find tree2 2) => 10)
(check-false (red-black-tree-find tree2 6))
(check-false (red-black-tree-empty? tree2))
(check-true (red-black-tree? tree2))

(check-true (red-black-tree? (make-red-black-tree int-cmp)))
(check-true (red-black-tree-empty? (make-red-black-tree int-cmp)))

(define tree3 (fold-right (lambda (x t) (red-black-tree-delete t x)) tree elements))

(check-true (red-black-tree-empty? tree3))

(check-report)