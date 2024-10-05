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

(define-library (liii base64)
(import (liii base))
(export
  string-base64-encode bytevector-base64-encode base64-encode
  string-base64-decode bytevector-base64-decode base64-decode
)
(begin
(define-constant BYTE2BASE64_BV
  (string->utf8 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))

(define-constant BASE64_PAD_BYTE
  (char->integer #\=))

(define bytevector-base64-encode
  (typed-lambda ((bv bytevector?))
    (let* ((input-N (bytevector-length bv))
           (output-N (* 4 (ceiling (/ input-N 3))))
           (output (make-bytevector output-N)))
      (let loop ((i 0) (j 0))
        (when (< i input-N)
          (let* ((b1 (bv i))
                 (b2 (if (< (+ i 1) input-N) (bv (+ i 1)) 0))
                 (b3 (if (< (+ i 2) input-N) (bv (+ i 2)) 0))
                 (combined (logior (ash b1 16) (ash b2 8) b3))
                 (c1 (logand (ash combined -18) #x3F))
                 (c2 (logand (ash combined -12) #x3F))
                 (c3 (logand (ash combined -6) #x3F))
                 (c4 (logand combined #x3F)))
            (bytevector-u8-set! output j (BYTE2BASE64_BV c1))
            (bytevector-u8-set! output (+ j 1) (BYTE2BASE64_BV c2))
            (bytevector-u8-set! output (+ j 2)
              (if (< (+ i 1) input-N) (BYTE2BASE64_BV c3) BASE64_PAD_BYTE))
            (bytevector-u8-set! output (+ j 3)
              (if (< (+ i 2) input-N) (BYTE2BASE64_BV c4) BASE64_PAD_BYTE))
            (loop (+ i 3) (+ j 4)))))
      output)))

(define string-base64-encode
  (typed-lambda ((str string?))
    (utf8->string (bytevector-base64-encode (string->utf8 str)))))

(define (base64-encode x)
  (cond ((string? x)
         (string-base64-encode x))
        ((bytevector? x)
         (bytevector-base64-encode x))
        (else
         (type-error "input must be string or bytevector"))))

(define-constant BASE64_TO_BYTE_V
  (let1 byte2base64-N (bytevector-length BYTE2BASE64_BV)
    (let loop ((i 0) (v (make-vector 256 -1)))
      (if (< i byte2base64-N)
          (begin
            (vector-set! v (BYTE2BASE64_BV i) i)
            (loop (+ i 1) v))
          v))))

(define (bytevector-base64-decode bv)
  (define (decode c1 c2 c3 c4)
    (let* ((b1 (BASE64_TO_BYTE_V c1))
           (b2 (BASE64_TO_BYTE_V c2))
           (b3 (BASE64_TO_BYTE_V c3))
           (b4 (BASE64_TO_BYTE_V c4)))
      (if (or (negative? b1) (negative? b2)
              (and (negative? b3) (!= c3 BASE64_PAD_BYTE))
              (and (negative? b4) (!= c4 BASE64_PAD_BYTE)))
          (value-error "Invalid base64 input")
          (values
            (logior (ash b1 2) (ash b2 -4))
            (logand (logior (ash b2 4) (ash b3 -2)) #xFF)
            (logand (logior (ash b3 6) b4) #xFF)
            (if (negative? b3) 1 (if (negative? b4) 2 3))))))
  
  (let* ((input-N (bytevector-length bv))
         (output-N (* input-N 3/4))
         (output (make-bytevector output-N)))

    (unless (zero? (modulo input-N 4))
      (value-error "length of the input bytevector must be 4X"))
    
    (let loop ((i 0) (j 0))
      (if (< i input-N)
          (receive (r1 r2 r3 cnt)
                   (decode (bv i) (bv (+ i 1)) (bv (+ i 2)) (bv (+ i 3)))
            (bytevector-u8-set! output j r1)
            (when (>= cnt 2)
              (bytevector-u8-set! output (+ j 1) r2))
            (when (>= cnt 3)
              (bytevector-u8-set! output (+ j 2) r3))
            (loop (+ i 4) (+ j cnt)))
          (let ((final (make-bytevector j)))
            (vector-copy! final 0 output 0 j)
            final)))))

(define string-base64-decode
  (typed-lambda ((str string?))
    (utf8->string (bytevector-base64-decode (string->utf8 str)))))

(define (base64-decode x)
  (cond ((string? x)
         (string-base64-decode x))
        ((bytevector? x)
         (bytevector-base64-decode x))
        (else
         (type-error "input must be string or bytevector"))))

) ; end of begin
) ; end of define-library

