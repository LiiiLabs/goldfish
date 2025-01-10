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

(define-library (liii vector)
(import (srfi srfi-133)
        (srfi srfi-13)
        (liii base))
(export
  ; S7 Scheme built-in
  make-vector vector vector-length vector-ref vector-set! vector->list list->vector
  ; from (scheme base)
  vector-copy vector-fill! vector-copy! vector->string string->vector
  vector-map vector-for-each
  ; from (srfi srfi-133)
  vector-empty?
  vector-fold vector-fold-right
  vector-count
  vector-any vector-every vector-copy vector-copy!
  vector-index vector-index-right vector-skip vector-skip-right vector-partition
  vector-swap! vector-cumulate reverse-list->vector
  vector=
  ; Liii Extras
  vector-filter case-vector case-vector? case-vector=?
)
(begin

(define (vector-filter pred vec)
  (let* ((result-list (vector-fold (lambda (elem acc)
                                     (if (pred elem)
                                         (cons elem acc)
                                         acc))
                                   '()
                                   vec))
         (result-length (length result-list))
         (result-vec (make-vector result-length)))
    (let loop ((i (- result-length 1)) (lst result-list))
      (if (null? lst)
          result-vec
          (begin
            (vector-set! result-vec i (car lst))
            (loop (- i 1) (cdr lst)))))))

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

) ; end of begin
) ; end of define-library

