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
        (liii sort))

(check-set-mode! 'report-failed)

(define (pair-< x y)
  (< (car x) (car y)))

(define (pair-full-< x y)
  (cond
    ((not (= (car x) (car y))) (< (car x) (car y)))
    (else (< (cdr y) (cdr x)))))

(check-false (list-sorted? < '(1 5 1 0 -1 9 2 4 3)))
(check-false (vector-sorted? < #(1 5 1 0 -1 9 2 4 3)))

(check-true (list-sorted? < (list-sort < '(1 5 1 0 -1 9 2 4 3))))
(check-true (list-sorted? < (list-stable-sort < '(1 5 1 0 -1 9 2 4 3))))
(check-true (list-sorted? pair-< (list-merge pair-< '((1 . 1) (1 . 2) (3 . 1)) '((1 . 3) (2 . 1) (3 . 2) (4 . 1)))))
(check (list-merge pair-< '((1 . 1) (1 . 2) (3 . 1)) '((1 . 3) (2 . 1) (3 . 2) (4 . 1)))
       => '((1 . 1) (1 . 2) (1 . 3) (2 . 1) (3 . 1) (3 . 2) (4 . 1)))
(check-true (list-sorted? pair-full-<
                          (list-merge pair-full-< '((1 . 2) (1 . 1) (3 . 1)) '((1 . 3) (2 . 1) (3 . 2) (4 . 1)))))
(check (list-merge pair-full-< '((1 . 2) (1 . 1) (3 . 1)) '((1 . 3) (2 . 1) (3 . 2) (4 . 1)))
       => '((1 . 3) (1 . 2) (1 . 1) (2 . 1) (3 . 2) (3 . 1) (4 . 1)))

(check-true (vector-sorted? < (vector-sort < #(1 5 1 0 -1 9 2 4 3))))
(check-true (vector-sorted? < (vector-stable-sort < #(1 5 1 0 -1 9 2 4 3))))
(check-true (vector-sorted? pair-<
                            (vector-merge pair-< #((1 . 1) (1 . 2) (3 . 1)) #((1 . 3) (2 . 1) (3 . 2) (4 . 1)))))
(check (vector-merge pair-< #((1 . 1) (1 . 2) (3 . 1)) #((1 . 3) (2 . 1) (3 . 2) (4 . 1)))
       => #((1 . 1) (1 . 2) (1 . 3) (2 . 1) (3 . 1) (3 . 2) (4 . 1)))
(check-true (vector-sorted? pair-full-<
                            (vector-merge pair-full-< #((1 . 2) (1 . 1) (3 . 1)) #((1 . 3) (2 . 1) (3 . 2) (4 . 1)))))
(check (vector-merge pair-full-< #((1 . 2) (1 . 1) (3 . 1)) #((1 . 3) (2 . 1) (3 . 2) (4 . 1)))
       => #((1 . 3) (1 . 2) (1 . 1) (2 . 1) (3 . 2) (3 . 1) (4 . 1)))

(check-report)

