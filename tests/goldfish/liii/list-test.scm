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

(import (liii list)
        (liii check)
        (only (srfi srfi-1) delete-duplicates))

(check-set-mode! 'report-failed)

(check (iota 3) => (list 0 1 2))
(check (iota 3 7) => (list 7 8 9))
(check (iota 2 7 2) => (list 7 9))

(check-catch 'value-error (iota -1))
(check-catch 'type-error (iota 'a))

; (check (circular-list? (circular-list 1 2)) => #t)

(check (null-list? '()) => #t)

(check (null-list? '(1 . 2)) => #f)

(check (null-list? '(1 2)) => #f)

(check (null? 1) => #f)

(check (first '(1 2 3 4 5 6 7 8 9 10)) => 1)
(check (first '(left . right)) => 'left)

(check-catch 'wrong-type-arg (first '()))

(check (second '(1 2 3 4 5 6 7 8 9 10)) => 2)

(check-catch 'wrong-type-arg (second '(left . right)))
(check-catch 'wrong-type-arg (second '(1)))

(check (third '(1 2 3 4 5 6 7 8 9 10)) => 3)

(check-catch 'wrong-type-arg (third '(1 2)))

(check (fourth '(1 2 3 4 5 6)) => 4)

(check (fifth '(1 2 3 4 5 6 7 8 9 10)) => 5)

(check (sixth '(1 2 3 4 5 6 7 8 9 10)) => 6)

(check (seventh '(1 2 3 4 5 6 7 8 9 10)) => 7)

(check (eighth '(1 2 3 4 5 6 7 8 9 10)) => 8)

(check (ninth '(1 2 3 4 5 6 7 8 9 10)) => 9)

(check (tenth '(1 2 3 4 5 6 7 8 9 10)) => 10)

(check (take '(1 2 3 4) 3) => '(1 2 3))
(check (take '(1 2 3 4) 4) => '(1 2 3 4))
(check (take '(1 2 3 . 4) 3) => '(1 2 3))

(check-catch 'wrong-type-arg (take '(1 2 3 4) 5))
(check-catch 'wrong-type-arg (take '(1 2 3 . 4) 4))

(check (drop '(1 2 3 4) 2) => '(3 4))

(check (drop '(1 2 3 4) 4) => '())

(check-catch 'wrong-type-arg (drop '(1 2 3 4) 5))

(check (drop '(1 2 3 . 4) 3) => 4)

(check-catch 'wrong-type-arg (drop '(1 2 3 . 4) 4))

(check (take-right '(1 2 3 4) 3) => '(2 3 4))

(check (take-right '(1 2 3 4) 4) => '(1 2 3 4))

(check
  (catch 'wrong-type-arg
    (lambda () (take-right '(1 2 3 4) 5))
    (lambda args #t))
  => #t)

(check (take-right '(1 2 3 . 4) 3) => '(1 2 3 . 4))

(check
  (catch 'wrong-type-arg
    (lambda () (take-right '(1 2 3 . 4) 4))
    (lambda args #t))
  => #t)

(check (drop-right '(1 2 3 4) 2) => '(1 2))

(check (drop-right '(1 2 3 4) 4) => '())

(check
  (catch 'wrong-type-arg
    (lambda () (drop-right '(1 2 3 4) 5))
    (lambda args #t))
  => #t)

(check
  (catch 'wrong-type-arg
    (lambda () (drop-right '(1 2 3 4) -1))
    (lambda args #t))
  => #t)

(check (drop-right '(1 2 3 . 4) 3) => '())

(check
  (catch 'wrong-type-arg
    (lambda () (drop-right '(1 2 3 . 4) 4))
    (lambda args #t))
  => #t)

(check (last-pair '(a b c)) => '(c))
(check (last-pair '(c)) => '(c))

(check (last-pair '(a b . c)) => '(b . c))
(check (last-pair '(b . c)) => '(b . c))

(check-catch 'wrong-type-arg (last-pair '()))

(check (last '(a b c)) => 'c)
(check (last '(c)) => 'c)

(check (last '(a b . c)) => 'b)
(check (last '(b . c)) => 'b)

(check-catch 'wrong-type-arg (last '()))

(check (count even? '(3 1 4 1 5 9 2 5 6)) => 3)

(check (fold + 0 '(1 2 3 4)) => 10)
(check (fold + 0 '()) => 0)

(check-catch 'type-error (fold 0 + '(1 2 3 4)))

(check (fold cons () '(1 2 3 4)) => '(4 3 2 1))

(check
  (fold (lambda (x count) (if (symbol? x) (+ count 1) count))
        0
        '(a b 1 2 3 4))
  => 2)

(check (fold-right + 0 '(1 2 3 4)) => 10)

(check (fold-right + 0 '()) => 0)

(check
  (fold-right (lambda (x count) (if (symbol? x) (+ count 1) count))
        0
        '(a b 1 2 3 4))
  =>
  2)

(check (fold-right cons () '(1 2 3 4)) => '(1 2 3 4))

(check (reduce + 0 '(1 2 3 4)) => 10)
(check (reduce + 0 '()) => 0)

(check (reduce cons () '(1 2 3 4)) => '(4 3 2 . 1))

(check-catch 'wrong-type-arg 
  (reduce (lambda (x count) (if (symbol? x) (+ count 1) count))
          0
          '(a b 1 2 3 4)))

(check (reduce-right + 0 '(1 2 3 4)) => 10)

(check (reduce-right + 0 '()) => 0)

(check (reduce-right cons () '(1 2 3 4))
       => '(1 2 3 . 4) )

(check
  (reduce-right (lambda (x count) (if (symbol? x) (+ count 1) count))
        0
        '(a b 1 2 3 4))
  => 6)

(check (filter even? '(-2 -1 0 1 2)) => '(-2 0 2))

(check
  (partition symbol? '(one 2 3 four five 6))
  => (cons '(five four one) '(6 3 2)))

(check (remove even? '(-2 -1 0 1 2)) => '(-1 1))

(check (find even? '(3 1 4 1 5 9)) => 4)

(check (find even? '()) => #f)

(check (find even? '(1 3 5 7 9)) => #f)

(check (take-while even? '()) => '())

(check (take-while (lambda (x) #t) '(1 2 3))
  => '(1 2 3))

(check
  (take-while (lambda (x) #f) '(1 2 3))
  => '())

(check
  (take-while (lambda (x) (not (= x 1))) '(1 2 3))
  => '())

(check
  (take-while (lambda (x) (< x 3)) '(1 2 3 0))
  => '(1 2))

(check (drop-while even? '()) => '())

(check (drop-while (lambda (x) #t) '(1 2 3)) => '())

(check (drop-while (lambda (x) #f) '(1 2 3)) => '(1 2 3))

(check
  (drop-while (lambda (x) (not (= x 1))) '(1 2 3))
  => '(1 2 3))

(check (list-index even? '(3 1 4 1 5 9)) => 2)
(check (list-index even? '()) => #f)
(check (list-index even? '(1 3 5 7 9)) => #f)

(check (any integer? '()) => #f)
(check (any integer? '(a 3.14 "3")) => #f)
(check (any integer? '(a 3.14 3)) => #t)

(check (every integer? '()) => #t)
(check (every integer? '(a 3.14 3)) => #f)
(check (every integer? '(1 2 3)) => #t)

(check (delete 1 (list 1 2 3 4)) => (list 2 3 4))

(check (delete 0 (list 1 2 3 4)) => (list 1 2 3 4))

(check (delete #\a (list #\a #\b #\c) char=?)
       => (list #\b #\c))

(check (delete #\a (list #\a #\b #\c) (lambda (x y) #f))
       => (list #\a #\b #\c))

(check (delete 1 (list )) => (list ))

(check
  (catch 'wrong-type-arg
    (lambda ()
      (check (delete 1 (list 1 2 3 4) 'not-pred) => 1))
    (lambda args #t))
  => #t)

(check (delete-duplicates (list 1 1 2 3)) => (list 1 2 3))
(check (delete-duplicates (list 1 2 3)) => (list 1 2 3))
(check (delete-duplicates (list 1 1 1)) => (list 1))

(check (delete-duplicates (list )) => (list ))

(check (delete-duplicates (list 1 1 2 3) (lambda (x y) #f))
       => (list 1 1 2 3))

(check
  (catch 'wrong-type-arg
    (lambda
      ()
      (check (delete-duplicates (list 1 1 2 3) 'not-pred) => 1))
    (lambda args #t))
  => #t)

(check-true (length=? 3 (list 1 2 3)))
(check-false (length=? 2 (list 1 2 3)))
(check-false (length=? 4 (list 1 2 3)))

(check-true (length=? 0 (list )))
(check-catch 'value-error (length=? -1 (list )))

(check-true (length>? '(1 2 3 4 5) 3))
(check-false (length>? '(1 2) 3))
(check-false (length>? '() 0))

(check-true (length>? '(1) 0))
(check-false (length>? '() 1))

(check-false (length>? '(1 2 . 3) 2))
(check-true (length>? '(1 2 . 3) 1))

(check-true (length>=? '(1 2 3 4 5) 3))
(check-false (length>=? '(1 2) 3))
(check-true (length>=? '() 0))

(check-true (length>=? '(1) 0))
(check-false (length>=? '() 1))

(check-false (length>=? '(1 2 . 3) 3))
(check-true (length>=? '(1 2 . 3) 2))

(check ((list-view (list 1 2 3))) => (list 1 2 3))

(check (((list-view (list 1 2 3))
        map (lambda (x) (+ x 1)))) => (list 2 3 4))

(check (((list-view (list 1 2 3))
        map (lambda (x) (+ x 1))
        map (lambda (x) (* x x))))
       => (list 4 9 16))

(check (flatmap (lambda (x) (list x x))
                (list 1 2 3))
  => (list 1 1 2 2 3 3))

(check-catch 'type-error (flatmap 1 (list 1 2 3)))

(check (not-null-list? (list 1)) => #t)
(check (list-not-null? (list 1)) => #t)
(check (list-null? (list 1)) => #f)

(check (not-null-list? (list 1 2 3)) => #t)
(check (list-not-null? (list 1 2 3)) => #t)
(check (list-null? (list 1 2 3)) => #f)

(check (not-null-list? '(a)) => #t)
(check (list-not-null? '(a)) => #t)
(check (list-null? '(a)) => #f)

(check (not-null-list? '(a b c)) => #t)
(check (list-not-null? '(a b c)) => #t)
(check (list-null? '(a b c)) => #f)

(check (not-null-list? ()) => #f)
(check (list-not-null? ()) => #f)
(check (list-null? ()) => #t)

; '(a) is a pair and a list
; '(a . b) is a pair but not a list
(check (not-null-list? '(a . b)) => #f)
(check (list-not-null? '(a . b)) => #f)
(check (list-null? '(a . b)) => #f)

(check-catch 'type-error (not-null-list? 1))
(check (list-not-null? 1) => #f)
(check (list-null? 1) => #f)

; deepest flatten
(check (flatten '((a) () (b ()) () (c))) => '(a b c))
(check (flatten '((a b) c (((d)) e))) => '(a b c d e))
(check (flatten '(a b (() (c)))) => '(a b c))
; depth flatten
(check (flatten '((a) () (b ()) () (c)) 0) => '((a) () (b ()) () (c)))
(check (flatten '((a) () (b ()) () (c)) 1) => '(a b () c))
(check (flatten '((a) () (b ()) () (c)) 2) => '(a b c))
(check (flatten '((a) () (b ()) () (c)) -1) => '(a b c))
(check (flatten '((a b) c (((d)) e)) 0) => '((a b) c (((d)) e)))
(check (flatten '((a b) c (((d)) e)) 1) => '(a b c ((d)) e))
(check (flatten '((a b) c (((d)) e)) 2) => '(a b c (d) e))
(check (flatten '((a b) c (((d)) e)) 3) => '(a b c d e))
(check (flatten '((a b) c (((d)) e)) -1) => '(a b c d e))
(check (flatten '(a b (() (c))) 0) => '(a b (() (c))))
(check (flatten '(a b (() (c))) 1) => '(a b () (c)))
(check (flatten '(a b (() (c))) 2) => '(a b c))
(check (flatten '(a b (() (c))) -1) => '(a b c))
; error depth flatten
(check-catch 'type-error (flatten '((a) () (b ()) () (c)) 'a))
(check-catch 'type-error (flatten '((a) () (b ()) () (c)) (make-vector 1 1)))

(check-report)

