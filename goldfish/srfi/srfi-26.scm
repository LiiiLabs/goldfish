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

(define-library (srfi srfi-26)
  (import (scheme base))
  (export cut)
(begin
  
(define-macro (cut . formal-args)
  (let* ((cnt-holes 0)
         (slotted-args (map (lambda (x)
                              (if (eq? '<> x)
                                  (begin (set! cnt-holes (+ 1 cnt-holes)) 'slot) x))
                            formal-args)))
   `(lambda args
    (when (not (= (length args) ,cnt-holes))
      (error 'wrong-number-of-args
             "The number of args should be same with the holes."))
    (let1 formal-call (map (lambda (x) (if (not (eq? x 'slot)) x
                                           (let1 subst-arg (car args)
                                                 (set! args (cdr args))
                                                 subst-arg))) 
                           ',slotted-args)
             (eval formal-call)))))
) ; end of begin
) ; end of define-library
