;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) John Cowan (2015). All Rights Reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

;;;; Main part of the SRFI 114 reference implementation

;;; "There are two ways of constructing a software design: One way is to
;;; make it so simple that there are obviously no deficiencies, and the
;;; other way is to make it so complicated that there are no *obvious*
;;; deficiencies." --Tony Hoare

(define-library (srfi srfi-128)
(import (scheme base))
(export
  make-comparator
  boolean<? complex<? default-hash
  =? <? >? <=? >=?
)
(begin

(define-record-type comparator
  (make-raw-comparator type-test equality ordering hash ordering? hash?)
  comparator?
  (type-test comparator-type-test-predicate)
  (equality comparator-equality-predicate)
  (ordering comparator-ordering-predicate)
  (hash comparator-hash-function)
  (ordering? comparator-ordered?)
  (hash? comparator-hashable?))

(define (default-hash obj)
  (hash-code obj))

(define (binary=? comparator a b)
  ((comparator-equality-predicate comparator) a b))

(define (binary<? comparator a b)
  ((comparator-ordering-predicate comparator) a b))

(define (binary>? comparator a b)
  (binary<? comparator b a))

(define (binary<=? comparator a b)
  (not (binary>? comparator a b)))

(define (binary>=? comparator a b)
  (not (binary<? comparator a b)))

(define (make-comparator type-test equality ordering hash)
  (make-raw-comparator
    (if (eq? type-test #t) (lambda (x) #t) type-test)
    (if (eq? equality #t) (lambda (x y) (eqv? (ordering x y) 0)) equality)
    (if ordering ordering (lambda (x y) (error "ordering not supported")))
    (if hash hash (lambda (x y) (error "hashing not supported")))
    (if ordering #t #f)
    (if hash #t #f)))

(define (make-eq-comparator)
  (make-comparator #t eq? #f default-hash))

(define (make-eqv-comparator)
  (make-comparator #t eqv? #f default-hash))

(define (make-equal-comparator)
  (make-comparator #t equal? #f default-hash))

(define (make-pair-type-test car-comparator cdr-comparator)
  (lambda (obj)
    (and (pair? obj)
         (comparator-test-type car-comparator (car obj))
         (comparator-test-type cdr-comparator (cdr obj)))))

(define (make-pair=? car-comparator cdr-comparator)
   (lambda (a b)
     (and ((comparator-equality-predicate car-comparator) (car a) (car b))
          ((comparator-equality-predicate cdr-comparator) (cdr a) (cdr b)))))

(define (make-pair<? car-comparator cdr-comparator)
   (lambda (a b)
      (if (=? car-comparator (car a) (car b))
        (<? cdr-comparator (cdr a) (cdr b))
        (<? car-comparator (car a) (car b)))))

(define (make-pair-hash car-comparator cdr-comparator)
   (lambda (obj)
     (let ((acc (make-hasher)))
       (acc (comparator-hash car-comparator (car obj)))
       (acc (comparator-hash cdr-comparator (cdr obj)))
       (acc))))

(define (make-pair-comparator car-comparator cdr-comparator)
   (make-comparator
     (make-pair-type-test car-comparator cdr-comparator)
     (make-pair=? car-comparator cdr-comparator)
     (make-pair<? car-comparator cdr-comparator)
     (make-pair-hash car-comparator cdr-comparator)))

(define (object-type obj)
  (cond
    ((null? obj) 0)
    ((pair? obj) 1)
    ((boolean? obj) 2)
    ((char? obj) 3)
    ((string? obj) 4)
    ((symbol? obj) 5)
    ((number? obj) 6)
    ((vector? obj) 7)
    ((bytevector? obj) 8)
    (else 65535)))

(define (boolean<? a b)
  ;; #f < #t but not otherwise
  (and (not a) b))

(define (complex<? a b)
  (if (= (real-part a) (real-part b))
    (< (imag-part a) (imag-part b))
    (< (real-part a) (real-part b))))

(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

(define (dispatch-ordering type a b)
  (case type
    ((0) 0) ; All empty lists are equal
    ((1) ((make-pair<? (make-default-comparator) (make-default-comparator)) a b))
    ((2) (boolean<? a b))
    ((3) (char<? a b))
    ((4) (string<? a b))
    ((5) (symbol<? a b))
    ((6) (complex<? a b))
    ((7) ((make-vector<? (make-default-comparator) vector? vector-length vector-ref) a b))
    ((8) ((make-vector<? (make-comparator exact-integer? = < default-hash)
                         bytevector? bytevector-length bytevector-u8-ref) a b))
    ; Add more here
    (else (binary<? (registered-comparator type) a b))))

(define (default-ordering a b)
  (let ((a-type (object-type a))
        (b-type (object-type b)))
    (cond
      ((< a-type b-type) #t)
      ((> a-type b-type) #f)
      (else (dispatch-ordering a-type a b)))))

(define %salt% (lambda () 16064047))

(define (=? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary=? comparator a b)
         (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (<? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary<? comparator a b)
         (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (>? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary>? comparator a b)
         (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (<=? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary<=? comparator a b)
         (if (null? objs) #t (loop b (car objs) (cdr objs))))))

(define (>=? comparator a b . objs)
  (let loop ((a a) (b b) (objs objs))
    (and (binary>=? comparator a b)
         (if (null? objs) #t (loop b (car objs) (cdr objs))))))

) ; end of begin
) ; end of define-library

