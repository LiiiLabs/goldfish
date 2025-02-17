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

(define-library (liii threading)
(export ~> ~>> lambda~> lambda~>> thread inject-list-first-pos)
(import (liii list) (liii error))
(begin

(define (thread lst code inject-list)
  (if (null? lst) code (thread (cdr lst) (inject code (car lst) inject-list) inject-list)))

(define (inject code hole inject-list)
  (if (not (list? hole))
      (list hole code)
      (cons (car hole) (inject-list code (cdr hole)))))

(define (inject-list-last-pos code hole)
  (cond ((null? hole) (list code))
        ((eq? (car hole) '_) (cons code (cdr hole)))
        (else (cons (car hole) (inject-list-last-pos code (cdr hole))))))

(define (inject-list-first-pos code hole)
  (define occurs #f)
  (define (traverse hole)
    (cond ((null? hole) '())
          ((eq? (car hole) '_) (set! occurs #t)
                               (cons code (cdr hole)))
          (else (cons (car hole) (traverse (cdr hole))))))
  (define traversed (traverse hole))
  (if occurs traversed (cons code hole)))

(define-macro (~> . args)
  (when (null? args)
    (not-enough-arguments-error "~>: should provide at least 1 argument"))
  (thread (cdr args) (car args) inject-list-first-pos))

(define-macro (~>> . args)
  (when (null? args)
    (not-enough-arguments-error "~>>: should provide at least 1 argument"))
  (thread (cdr args) (car args) inject-list-last-pos))

(define-macro (lambda~> . args)
  (define var (gensym "x"))
    `(lambda (,var) ,(thread args var inject-list-first-pos)))

(define-macro (lambda~>> . args)
  (define var (gensym "x"))
  `(lambda (,var) ,(thread args var inject-list-last-pos)))

) ; end of begin
) ; end of library