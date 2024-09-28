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

(define-library (liii list)
(export
  ; S7 built-in
  cons car cdr map for-each
  ; SRFI 1: Constructors
  circular-list iota
  ; SRFI 1: Predicates
  null-list? circular-list?
  ; SRFI 1: Selectors
  first second third fourth fifth sixth seventh eighth ninth tenth
  take drop take-right drop-right
  last-pair last
  ; SRFI 1: fold, unfold & map
  count fold fold-right reduce reduce-right
  filter partition remove append-map
  ; SRFI 1: Searching
  find any every list-index
  take-while drop-while
  ; SRFI 1: deleting
  delete
  ; Liii List extensions
  list-view flatmap
  list-null? list-not-null? not-null-list?
  length=? length>? length>=?
  duple invert down up tree-swapper list-product
  flatten copy-list sort
)
(import (srfi srfi-1)
        (liii error))
(begin

(define (length=? x scheme-list)
  (when (< x 0)
    (value-error "length=?: expected non-negative integer x but received ~d" x))
  (cond ((and (= x 0) (null? scheme-list)) #t)
        ((or (= x 0) (null? scheme-list)) #f)
        (else (length=? (- x 1) (cdr scheme-list)))))

(define (length>? lst len)
  (let loop ((lst lst)
             (cnt 0))
    (cond ((null? lst) (< len cnt))
          ((pair? lst) (loop (cdr lst) (+ cnt 1)))
          (else (< len cnt)))))

(define (length>=? lst len)
  (let loop ((lst lst)
             (cnt 0))
    (cond ((null? lst) (<= len cnt))
          ((pair? lst) (loop (cdr lst) (+ cnt 1)))
          (else (<= len cnt)))))

(define (list-view scheme-list)
  (define (f-inner-reducer scheme-list filter filter-func rest-funcs)
    (cond ((null? rest-funcs) (list-view (filter filter-func scheme-list)))
          (else
           (f-inner-reducer (filter filter-func scheme-list)
                            (car rest-funcs)
                            (cadr rest-funcs)
                            (cddr rest-funcs)))))
  (define (f-inner . funcs)
    (cond ((null? funcs) scheme-list)
          ((length=? 2 funcs)
           (list-view ((car funcs) (cadr funcs) scheme-list)))
          ((even? (length funcs))
           (f-inner-reducer scheme-list
                            (car funcs)
                            (cadr funcs)
                            (cddr funcs)))
          (else (error 'wrong-number-of-args
                       "list-view only accepts even number of args"))))
  f-inner)

(define flatmap append-map)

(define (not-null-list? l)
  (cond ((pair? l)
         (or (null? (cdr l)) (pair? (cdr l))))
        ((null? l) #f)
        (else
         (error 'type-error "type mismatch"))))

(define (list-null? l)
  (and (not (pair? l)) (null? l)))

(define (list-not-null? l)
  (and (pair? l)
       (or (null? (cdr l)) (pair? (cdr l)))))

;;; some code from book Essential of Programming Language

;; duple : number X any -> listof(any)
;; usage : (duple n x) = a list with n values x, unsafe with set-car! and
;; set-cdr! 
(define (duple n x)
  (define (duple-iter n x res)
    (if (= n 0)
      res
      (duple-iter (- n 1) x (cons x res))))
  (duple-iter n x '()))

;; invert : listof(any) -> listof(any)
;; usage : (invert '((a b) ...) ) = ((b a) ...)
(define (invert lst)
  (map reverse lst))

;; down : listof(any) -> listof(listof(any))
;; usage : (down x ...) = ((x) ...)
(define (down lst)
  (map list lst))

;; up : listof(listof(any)) -> listof(any)
;; usage : (up '((a ...) ...)) = (a ...)
(define (up lst)
  (define (up2 lst res)
    (if (null? lst)
      res
      (cons (car lst)
            (up2 (cdr lst) res))))
  (cond ((null? lst)
         '())
        ((null? (car lst))
         (up (cdr lst)))
        ((pair? (car lst))
         (up2 (car lst) (up (cdr lst))))
        (else
         (cons (car lst) (up (cdr lst))))))

;; tree-swapper : any X any X treeof(any)
;; usage : (swapper s1 s2 stree) returns a list the same as stree, but with all
;; occurrences of s1 replaced by s2 and all occurrences of s2 replaced by s1.
(define (tree-swapper s1 s2 stree)
  (define (mapper v)
    (if (pair? v)
      (tree-swapper s1 s2 v)
      (cond ((equal? s1 v) s2)
            ((equal? s2 v) s1)
            (else v))))
  (map mapper stree))

;; list-product : listof(any) X listof(any) -> listof(listof(any))
;; usage: (list-product lst1 lst2), where lst1 and lst2 are each a list of
;; symbols without repetitions, returns a list of 2-value-lists that represents
;; the Cartesian product of lst2 and lst2 The 2-value-lists may appear in any
;; order.
(define (list-product lst1 lst2)
  (flatmap
   (lambda (x)
     (map
      (lambda (y)
        (list x y))
      lst2))
   lst1))

;; flatten : listof(any) -> listof(any)
;; usage : (flatten lst) returns a list of the values contained in lst in the
;; order in which they occur when slist is printed. Intuitively, flatten removes
;; all the inner parentheses from its argument.
(define (flatten lst)
  (define (flatten2 lst res)
    (cond ((null? lst)
           res)
          ((null? (car lst))
           (flatten2 (cdr lst) res))
          ((pair? (car lst))
           (flatten2
            (car lst)
            (flatten2 (cdr lst) res)))
          (else
           (cons
            (car lst)
            (flatten2 (cdr lst) res)))))
  (flatten2 lst '()))

;; copy-list : listof(any) -> listof(any)A
;; usage : copy a list of lst
(define (copy-list lst)
  (if (null? lst)
    '()
    (cons (car lst) (copy-list (cdr lst)))))

;; sort : function X listof(any) -> listof(any)
;; usage : (sort pred lst) returns a list of elements sorted by the pred.
(define (sort pred lst)
  (sort! (copy-list lst) pred))


) ; end of begin
) ; end of library

