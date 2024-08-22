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
        (liii base))

(check-set-mode! 'report-failed)

(check (== (list 1 2) (list 1 2)) => #t)
(check (!= (list 1 2) (list 1 2)) => #f)

(check (== (list 1 2) (list 1 2)) => #t)
(check (!= (list 1 2) (list 1 2)) => #f)

(check
  (with-output-to-string
    (lambda ()
      (display* "hello world" "\n")))
  => "hello world\n")

(check (in? 1 (list )) => #f)
(check (in? 1 (list 3 2 1)) => #t)
(check (in? #\x "texmacs") => #t)
(check (in? 1 (vector )) => #f)
(check (in? 1 (vector 3 2 1)) => #t)
(check-catch 'type-error (in? 1 "123"))

(check (let1 x 1 x) => 1)
(check (let1 x 1 (+ x 1)) => 2)

(check-true ((compose not zero?) 1))
(check-false ((compose not zero?) 0))

(check-report)

