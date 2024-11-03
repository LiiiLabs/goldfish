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

(define-library (liii bitwise)
(import (srfi srfi-151)
        (liii error))
(export
  ; from (srfi srfi-151)
  bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-or bitwise-nor bitwise-nand
  bit-count
  arithmetic-shift
  ; S7 built-in
  lognot logand logior logxor
  ash
)
(begin

(define bitwise-or bitwise-ior)
(define (bitwise-andc1 i j)  (bitwise-and (bitwise-not i) j))   
(define (bitwise-andc2 i j)  (bitwise-and i (bitwise-not j)))   
(define (bitwise-orc1  i j)  (bitwise-ior (bitwise-not i) j))
(define (bitwise-orc2  i j)  (bitwise-ior i (bitwise-not j)))

) ; end of begin
) ; end of library

