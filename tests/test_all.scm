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

(import (srfi srfi-78)
        (srfi srfi-1)
        (liii os))

(check-set-mode! 'report-failed)

(when (not (os-windows?))
  (check (file-exists? "/tmp") => #t)
  (check (file-exists? "/not_exists") => #f))

(when (os-windows?)
  (check (file-exists? "C:") => #t))

(check-report)

(define (all-tests)
  (list
    "tests/scheme/case-lambda-test.scm"
    "tests/scheme/process-context-test.scm"
    "tests/srfi/srfi-8-test.scm"
    "tests/srfi/srfi-9-test.scm"
    "tests/srfi/srfi-16-test.scm"
    "tests/srfi/srfi-39-test.scm"
    "tests/liii/os-test.scm"))

(define (goldfish-cmd)
  (if (os-windows?)
    "bin\\goldfish -l "
    "bin/goldfish -l "))

(let ((ret-l
       (map (lambda (x) (begin (newline) (display "> ") (display x) (newline) (os-call x)))
         (map (lambda (x) (string-append (goldfish-cmd) x))
           (all-tests)))))
  (when (find (lambda (x) (not (= x 0))) ret-l)
    (exit -1)))
