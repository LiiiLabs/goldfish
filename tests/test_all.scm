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

(import (liii list)
        (liii string)
        (liii os)
        (liii path))

(define (listdir2 dir)
  (map (lambda (x) (string-append dir "/" x))
    (vector->list (listdir dir))))

; (display (listdir2 "tests"))
(define (all-tests)
  ((case-list (listdir2 "tests/goldfish"))
    :filter path-dir?
    :flat-map listdir2
    :filter (lambda (x) (path-file? x))
    :filter (lambda (x) (not (string-ends? x "srfi-78-test.scm")))
    :collect))

(define (goldfish-cmd)
  (if (os-windows?)
    "bin\\goldfish "
    "bin/goldfish "))

(let ((ret-l
       (map (lambda (x) (os-call x))
         (map (lambda (x) (string-append (goldfish-cmd) x))
           (all-tests)))))
  (when (find (lambda (x) (not (= x 0))) ret-l)
    (exit -1)))
