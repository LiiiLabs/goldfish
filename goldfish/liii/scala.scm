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

(define-library (liii scala)
(import (liii string) (liii vector)
        (liii list) (liii hash-table))
(export
  option option? option=? none
  case-integer case-integer? case-integer=?
  case-string case-string? case-string=?
  case-list case-list? case-list=?
  case-vector case-vector? case-vector=?
  case-hash-table case-hash-table? case-hash-table=?
  box
)
(begin

(define (%apply-one x xs r)
  (let1 result r
    (if (null? xs) r (apply r xs))))

(define-case-class option ((value any?))
  (define (%map f . xs)
    (let1 r (if (null? value)
                (option '())
                (option (f value)))
      (if (null? xs) r (apply r xs))))

  (define (%flat-map f . xs)
    (let1 r (if (null? value)
                (option '())
                (f value))
      (if (null? xs) r (apply r xs))))

  (define (%filter pred . xs)
    (let1 r (if (or (null? value) (not (pred value)))
               (option '())
               (option value))
      (if (null? xs) r (apply r xs))))
  
  (define (%defined?) (not (null? value)))

  (define (%empty?) (null? value))

  (define (%get)
    (if (null? value)
        (value-error "option is empty, cannot get value")
        value))

  (define (%get-or-else default)
    (if (null? value)
        (if (procedure? default) (default) default)
        value))
)
(define (none) (option '()))

(define-case-class case-integer ((data integer?))
  (define (%unbox) data)

(typed-define (%to (n integer?))
  (if (< n data)
      (case-list (list))
      (case-list (iota (+ (- n data) 1) data))))

(typed-define (%until (n integer?))
  (if (<= n data)
      (case-list (list))
      (case-list (iota (+ (- n data)) data))))

)

(define-case-class case-string ((data string?))
  (define (%unbox) data)

(define (%map x . xs)
  (%apply-one x xs
    (case-string (string-map x data))))

(define (%empty?)
  (string-null? data))

(define (%length)
  (string-length data))

)

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

  (define (%find pred)
    (let loop ((lst data))
      (cond
        ((null? lst) (none))
        ((pred (car lst)) (option (car lst)))
        (else (loop (cdr lst))))))

  (define (%count . xs)
    (cond ((null? xs) (length data))
          ((length=? 1 xs) (count (car xs) data))
          (else (error 'wrong-number-of-args "case-list%count" xs))))

  (define (%forall pred)
    (every pred data))

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

(define-case-class case-vector ((data vector?))
  (define (%collect) data)

  (define (%map x . xs)
    (let1 r (case-vector (vector-map x data))
      (if (null? xs) r (apply r xs))))
  
  (define (%filter x . xs)
    (let1 r (case-vector (vector-filter x data))
      (if (null? xs) r (apply r xs))))

  (define (%for-each x)
    (vector-for-each x data))

  (define (%count . xs)
    (cond ((null? xs) (vector-length data))
          ((length=? 1 xs) (vector-count (car xs) data))
          (else (error 'wrong-number-of-args "case-vector%count" xs))))

  (define (%take x . xs)
    (typed-define (scala-take (data vector?) (n integer?))
      (cond
        ((< n 0) (vector))
        ((>= n (vector-length data)) data)
        (else
          (let ((new-vec (make-vector n)))
            (do ((i 0 (+ i 1)))
                ((>= i n) new-vec)
              (vector-set! new-vec i (vector-ref data i)))))))

    (let1 r (case-vector (scala-take data x))
      (if (null? xs) r (apply r xs))))

  (define (%take-right x . xs)
    (typed-define (scala-take-right (data vector?) (n integer?))
      (let ((len (vector-length data)))
        (cond
          ((< n 0) (vector))
          ((>= n len) data)
          (else
            (let ((new-vec (make-vector n)))
              (do ((i (- len n) (+ i 1))
                   (j 0 (+ j 1)))
                  ((>= j n) new-vec)
                (vector-set! new-vec j (vector-ref data i))))))))

    (let1 r (case-vector (scala-take-right data x))
      (if (null? xs) r (apply r xs))))

  (define (%find p)
    (let loop ((i 0))
      (cond
        ((>= i (vector-length data)) (none))
        ((p (vector-ref data i)) (option (vector-ref data i)))
        (else (loop (+ i 1))))))
  (define (%forall p)
    (vector-every p data))

  (define (%fold initial f)
    (vector-fold f initial data))

  (define (%fold-right initial f)
    (vector-fold-right f initial data))

  (define (%make-string . xs)
    (define (parse-args xs)
      (cond
        ((null? xs) (values "" "" ""))
        ((length=? 1 xs)
         (let1 sep (car xs)
           (if (string? sep)
               (values "" sep "")
               (type-error "case-vector%make-string: separator must be a string" sep))))
        ((length=? 2 xs)
         (error 'wrong-number-of-args "case-vector%make-string: expected 0, 1, or 3 arguments, but got 2" xs))
        ((length=? 3 xs)
         (let ((start (car xs))
               (sep (cadr xs))
               (end (caddr xs)))
           (if (and (string? start) (string? sep) (string? end))
               (values start sep end)
               (type-error "case-vector%make-string: prefix, separator, and suffix must be strings" xs))))
        (else (error 'wrong-number-of-args "case-vector%make-string: expected 0, 1, or 3 arguments" xs))))

    (receive (start sep end) (parse-args xs)
      (string-append start (string-join (map object->string (vector->list data)) sep) end)))
)
(define-case-class case-hash-table ((data hash-table?))
  (define (%collect) data)

(define (%map f . xs)
  (%apply-one f xs
    (let1 r (make-hash-table)
      (hash-table-for-each
         (lambda (k v)
           (receive (k1 v1) (f k v)
             (hash-table-set! r k1 v1)))
         data)
      (case-hash-table r))))

(define (%get k)
  (option (hash-table-ref/default data k '())))

(define (%contains k)
  (hash-table-contains? data k))

)

(define (box x)
  (cond ((integer? x) (case-integer x))
        ((string? x) (case-string x))
        ((list? x) (case-list x))
        ((vector? x) (case-vector x))
        ((hash-table? x) (case-hash-table x))
        (else (type-error "box: x must be string?, list?, vector?, hash-table?"))))

) ; end of begin
) ; end of library

