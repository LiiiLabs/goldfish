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
  circular-list iota list-copy
  ; SRFI 1: Predicates
  null-list? circular-list? proper-list? dotted-list?
  ; SRFI 1: Selectors
  first second third fourth fifth sixth seventh eighth ninth tenth
  take drop take-right drop-right split-at
  last-pair last
  ; SRFI 1: Miscellaneous: length, append, concatenate, reverse, zip & count
  zip count
  ; SRFI 1: fold, unfold & map
  fold fold-right reduce reduce-right
  filter partition remove append-map
  ; SRFI 1: Searching
  find any every list-index
  take-while drop-while
  ; SRFI 1: Deleting
  delete
  ; SRFI 1: Association List
  assoc assq assv alist-cons
  ; Liii List extensions
  flat-map
  list-null? list-not-null? not-null-list?
  length=? length>? length>=? flatten
  case-list case-list? case-list=?
)
(import (srfi srfi-1)
        (srfi srfi-13)
        (liii error)
        (liii case))
(begin

(define (length=? x scheme-list)
  (when (not (integer? x))
    (type-error "length=?: first parameter x must be an integer"))
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

(define flat-map append-map)

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

(define* (flatten lst (depth 1))
  (define (flatten-depth-iter rest depth res-node)
    (if (null? rest)
        res-node
        (let ((first (car rest))
              (tail  (cdr rest)))
          (cond ((and (null? first) (not (= 0 depth)))
                 (flatten-depth-iter tail depth res-node))
                ((or (= depth 0) (not (pair? first)))
                 (set-cdr! res-node (cons first '()))
                 (flatten-depth-iter tail depth (cdr res-node)))
                (else
                 (flatten-depth-iter
                  tail
                  depth
                  (flatten-depth-iter
                   first
                   (- depth 1)
                   res-node)))))))
  (define (flatten-depth lst depth)
    (let ((res (cons #f '())))
      (flatten-depth-iter lst depth res)
      (cdr res)))

  (define (flatten-deepest-iter rest res-node)
    (if (null? rest)
      res-node
      (let ((first (car rest))
            (tail  (cdr rest)))
        (cond ((pair? first)
               (flatten-deepest-iter
                tail
                (flatten-deepest-iter
                 first
                 res-node)))
              ((null? first)
               (flatten-deepest-iter tail res-node))
              (else
               (set-cdr! res-node (cons first '()))
               (flatten-deepest-iter tail (cdr res-node)))))))
  (define (flatten-deepest lst)
    (let ((res (cons #f '())))
      (flatten-deepest-iter lst res)
      (cdr res)))

  (cond ((eq? depth 'deepest)
         (flatten-deepest lst))
        ((integer? depth)
         (flatten-depth lst depth))
        (else
         (type-error
          (string-append
            "flatten: the second argument depth should be symbol "
            "`deepest' or a integer, which will be uesd as depth,"
            " but got a ~A") depth)))
  ) ; end of (define* (flatten))

(define-case-class case-list ((data list?))
  (define (%collect) data)

  (define (%map x . xs)
    (let1 r (case-list (map x data))
      (if (null? xs) r (apply r xs))))
  
  (define (%flat-map x . xs)
    (let1 r (case-list (flat-map x data))
      (if (null? xs) r (apply r xs))))
  
  (define (%filter x . xs)
    (let1 r (case-list (filter x data))
      (if (null? xs) r (apply r xs))))

  (define (%for-each x)
    (for-each x data))

  (define (%take x . xs)
    (typed-define (scala-take (data list?) (n integer?))
      (cond ((< n 0) '())
            ((>= n (length data)) data)
            (else (take data n))))

    (let1 r (case-list (scala-take data x))
      (if (null? xs) r (apply r xs))))

  (define (%take-right x . xs)
    (typed-define (scala-take-right (data list?) (n integer?))
      (cond ((< n 0) '())
            ((>= n (length data)) data)
            (else (take-right data n))))

    (let1 r (case-list (scala-take-right data x))
      (if (null? xs) r (apply r xs))))

  (define (%count . xs)
    (cond ((null? xs) (length data))
          ((length=? 1 xs) (count (car xs) data))
          (else (error 'wrong-number-of-args "case-list%count" xs))))

  (define (%fold initial f)
    (fold f initial data))

  (define (%fold-right initial f)
    (fold-right f initial data))

  (define (%make-string . xs)
    (define (parse-args xs)
      (cond
        ((null? xs) (values "" "" ""))
        ((length=? 1 xs)
         (let1 sep (car xs)
           (if (string? sep)
               (values "" sep "")
               (type-error "case-list%make-string: separator must be a string" sep))))
        ((length=? 2 xs)
         (error 'wrong-number-of-args "case-list%make-string: expected 0, 1, or 3 arguments, but got 2" xs))
        ((length=? 3 xs)
         (let ((start (car xs))
               (sep (cadr xs))
               (end (caddr xs)))
           (if (and (string? start) (string? sep) (string? end))
               (values start sep end)
               (error 'type-error "case-list%make-string: prefix, separator, and suffix must be strings" xs))))
        (else (error 'wrong-number-of-args "case-list%make-string: expected 0, 1, or 3 arguments" xs))))

    (receive (start sep end) (parse-args xs)
      (string-append start (string-join (map object->string data) sep) end)))

)

) ; end of begin
) ; end of library

