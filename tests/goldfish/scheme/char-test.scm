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
        (scheme char))

(check-set-mode! 'report-failed)

(check (char-upcase #\z) => #\Z)
(check (char-upcase #\a) => #\A)

(check (char-upcase #\A) => #\A)
(check (char-upcase #\?) => #\?)
(check (char-upcase #\$) => #\$)
(check (char-upcase #\.) => #\.)
(check (char-upcase #\\) => #\\)
(check (char-upcase #\5) => #\5)
(check (char-upcase #\)) => #\))
(check (char-upcase #\%) => #\%)
(check (char-upcase #\0) => #\0)
(check (char-upcase #\_) => #\_)
(check (char-upcase #\?) => #\?)
(check (char-upcase #\space) => #\space)
(check (char-upcase #\newline) => #\newline)
(check (char-upcase #\null) => #\null)

(check (char-downcase #\A) => #\a)
(check (char-downcase #\Z) => #\z)

(check (char-downcase #\a) => #\a)

(check-true (char-upper-case? #\A))
(check-true (char-upper-case? #\Z))

(check-false (char-upper-case? #\a))
(check-false (char-upper-case? #\z))

(check-true (char-lower-case? #\a))
(check-true (char-lower-case? #\z))

(check-false (char-lower-case? #\A))
(check-false (char-lower-case? #\Z))

(check (digit-value #\1) => 1)
(check (digit-value #\2) => 2)
(check (digit-value #\3) => 3)
(check (digit-value #\4) => 4)
(check (digit-value #\5) => 5)
(check (digit-value #\6) => 6)
(check (digit-value #\7) => 7)
(check (digit-value #\8) => 8)
(check (digit-value #\9) => 9)
(check (digit-value #\0) => 0)
(check (digit-value #\a) => #f)
(check (digit-value #\c) => #f)

(check-report)

