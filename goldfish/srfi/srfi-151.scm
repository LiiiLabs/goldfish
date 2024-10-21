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

(define-library (srfi srfi-151)
(export
  bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-nor bitwise-nand bit-count
  arithmetic-shift
)
(begin

(define bitwise-not lognot)

(define bitwise-and logand)

(define bitwise-ior logior)

(define bitwise-xor logxor)

(define (bitwise-nor a b)  
        (lognot (bitwise-ior a b)))

(define (bitwise-nand a b)  
        (lognot (bitwise-and a b)))

(define (bit-count n)
  (letrec
   ((logcnt (lambda (n total)
              (if (zero? n)
                  total
                  (logcnt (quotient n 16)
                          (+ (vector-ref '#(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4)
                                         (modulo n 16))
                             total))))))
   (cond
     ((negative? n) (logcnt (bitwise-not n) 0))
     ((positive? n) (logcnt n 0))
     (else 0))))

(define arithmetic-shift ash)

) ; end of begin
) ; end of define-library

