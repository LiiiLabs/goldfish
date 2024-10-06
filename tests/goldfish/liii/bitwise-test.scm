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

(import (liii check)
        (liii bitwise))

(check-set-mode! 'report-failed)

(check (bitwise-not 0) => -1)
(check (bitwise-not 1) => -2)
(check (bitwise-not #b1000) => -9)
(check (bitwise-not -1) => 0)

(check (bitwise-and 5 3) => 1)  ; 5 (101) AND 3 (011) = 1 (001)
(check (bitwise-and 8 4) => 0)  ; 8 (1000) AND 4 (0100) = 0 (0000)
(check (bitwise-and #b101 #b011) => 1)  ; 5 (101) AND 3 (011) = 1 (001)  
(check (bitwise-and #b1000 #b0100) => 0) ; 8 (1000) AND 4 (0100) = 0 (0000)
(check (bitwise-and #b1100 #b1010) => 8) 

(check (bitwise-ior 5 3) => 7)  ; 5 (101) OR 3 (011) = 7 (111)
(check (bitwise-ior 8 4) => 12) ; 8 (1000) OR 4 (0100) = 12 (1100)
(check (bitwise-ior #b101 #b011) => 7)  ; 5 (101) AND 3 (011) = 1 (001)  
(check (bitwise-ior #b1000 #b0100) => 12) ; 8 (1000) AND 4 (0100) = 0 (0000)
(check (bitwise-ior #b1100 #b0001) => 13)

(check (bitwise-xor 1 1) => 0)
(check (bitwise-xor #b10 #b11) => #b01) ; 2 xor 3 = 1
(check (bitwise-xor #b101010 #b110100) => #b011110) ; 42 xor 20 = 34
(check (bitwise-xor #b0 #b0) => #b0) ; 0 xor 0 = 0
(check (bitwise-xor #b1 #b1) => #b0) ; 1 xor 1 = 0
(check (bitwise-xor #b101 #b111) => #b010) ; 5 xor 7 = 2
(check (bitwise-xor #b1000 #b1001) => #b0001) ; 8 xor 9 = 1
(check (bitwise-xor #b10010101 #b01111001) => #b11101100)

(check (arithmetic-shift #b10 -1) => #b1) ; 2 >> 1 = 1
(check (arithmetic-shift #b10 1) => #b100) ; 2 << 1 = 4
(check (arithmetic-shift #b1000 -2) => #b10) ; 8 >> 2 = 2
(check (arithmetic-shift #b1000 2) => #b100000)
(check (arithmetic-shift #b10000000000000000 -3) => #b10000000000000)
(check (arithmetic-shift #b1000000000000000 3) => #b1000000000000000000)

(check-report)

