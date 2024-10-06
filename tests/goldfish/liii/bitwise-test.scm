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

(check (bitwise-and 5 3) => 1)  ; 5 (101) AND 3 (011) = 1 (001)
(check (bitwise-and 8 4) => 0)  ; 8 (1000) AND 4 (0100) = 0 (0000)
(check (bitwise-and #b101 #b011) => 1)  ; 5 (101) AND 3 (011) = 1 (001)  
(check (bitwise-and #b1000 #b0100) => 0) ; 8 (1000) AND 4 (0100) = 0 (0000)
(check (bitwise-and #b1100 #b1010) => 8) 

(check (bitwise-or 5 3) => 7)  ; 5 (101) OR 3 (011) = 7 (111)
(check (bitwise-or 8 4) => 12) ; 8 (1000) OR 4 (0100) = 12 (1100)
(check (bitwise-or #b101 #b011) => 7)  ; 5 (101) AND 3 (011) = 1 (001)  
(check (bitwise-or #b1000 #b0100) => 12) ; 8 (1000) AND 4 (0100) = 0 (0000)
(check (bitwise-or #b1100 #b0001) => 13)

(check-report)

