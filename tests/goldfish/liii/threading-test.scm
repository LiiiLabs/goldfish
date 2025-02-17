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

(import (liii check) (liii threading) (liii list) (liii red-black-tree))

(check-set-mode! 'report-failed)
(check (~> 3 (- 2)) => 1)
(check (~>> 3 (- 2)) => -1)
(check (~> 3 (- 2) -) => -1)
(check (~> 3 (- _ 2)) => 1)
(check (~> 3 (- 2 _)) => -1)
(check (~>> 3 (- _ 2)) => 1)
(check (~>> 3 (- 2 _)) => -1)
(check (~> 1 - -) => 1)


(check (let ((_ 10)) (~> 1 (+ _ _))) => 11)

(check (let ((_ 10)) (~> 1 (+ (+ _ 1)))) => 12)

(check ((lambda~> (* 2) (+ 1)) 4) => 9)
(check ((lambda~>> (map (lambda~> (+ 1))) (fold-right + 0) -) '(1 2 3 4)) => -14)

(define (int-cmp x y)
  (cond
    ((= x y) 'eq)
    ((< x y) 'lt)
    (else 'gt)))

(check (~> (make-red-black-tree int-cmp) (red-black-tree-insert 1 1) (red-black-tree-insert 10 10)
           (red-black-tree-find 10)) => 10)


(check-report)