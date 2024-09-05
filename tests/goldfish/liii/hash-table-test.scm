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
        (liii comparator)
        (liii hash-table)
        (liii base))

(check-set-mode! 'report-failed)

(define empty-ht (make-hash-table))

(let1 ht (make-hash-table)
  (check (ht 'a) => #f)
  (hash-table-set! ht 'a 1)
  (check (ht 'a) => 1))

(let1 ht (make-hash-table (make-default-comparator))
  (hash-table-set! ht 1 2)
  (check (ht 1) => 2))

(let* ((mod10 (lambda (x) (modulo x 10)))
       (digit=? (lambda (x y) (= (modulo x 10) (modulo y 10))))
       (comp (make-comparator number? digit=? #f mod10))
       (ht (make-hash-table comp)))
  (hash-table-set! ht 1 2)
  (hash-table-set! ht 11 3)
  (check (ht 1) => 3)
  (check (ht 11) => 3)
  (check (ht 21) => 3))

(let1 ht (make-hash-table)
  (hash-table-set! ht 'brand 'liii)
  (check (hash-table-contains? ht 'brand) => #t)
  (hash-table-set! ht 'brand #f)
  (check (hash-table-contains? ht 'brand) => #f))

(check (hash-table-empty? empty-ht) => #t)

(let1 test-ht (make-hash-table)
  (hash-table-set! test-ht 'key 'value)
  (check (hash-table-empty? test-ht) => #f))

(let ((empty-h1 (make-hash-table))
      (empty-h2 (make-hash-table)))
  (check (hash-table=? empty-h1 empty-h2) => #t))

(let ((t1 (make-hash-table))
      (t2 (make-hash-table)))
  (hash-table-set! t1 'a 1)
  (hash-table-set! t2 'a 1)
  (check (hash-table=? t1 t2) => #t)
  (hash-table-set! t1 'b 2)
  (check (hash-table=? t1 t2) => #f))

(check (hash-table-ref empty-ht 'key) => #f)

(let1 ht (make-hash-table)
  (hash-table-set! ht 'key 'value)
  (check (hash-table-ref ht 'key) => 'value)
  (check (ht 'key) => 'value))

(let1 ht (make-hash-table)
  (check (hash-table-ref/default ht 'key 'value1) => 'value1)
  (check (hash-table-ref/default ht 'key (+ 1 2)) => 3)

  (hash-table-set! ht 'key 'value)
  (check (hash-table-ref/default ht 'key
           (begin (display "hello")
                  (+ 1 2)))
    => 'value)
) ; end of let1

(let1 ht (make-hash-table)
  (hash-table-set! ht 'k1 'v1 'k2 'v2)
  (check (ht 'k1) => 'v1)
  (check (ht 'k2) => 'v2)
)

(let1 ht (make-hash-table)
  (hash-table-update! ht 'key 'value)
  (check (hash-table-delete! ht 'key) => 1)
  (check-false (hash-table-contains? ht 'key))
  
  (hash-table-update! ht 'key1 'value1)
  (hash-table-update! ht 'key2 'value2)
  (hash-table-update! ht 'key3 'value3)
  (hash-table-update! ht 'key4 'value4)
  (check (hash-table-delete! ht 'key1 'key2 'key3) => 3)
)

(let1 ht (make-hash-table)
  (hash-table-update! ht 'key 'value)
  (check (ht 'key) => 'value)
  (hash-table-update! ht 'key 'value1)
  (check (ht 'key) => 'value1)
  (hash-table-update! ht 'key #f)
  (check (ht 'key) => #f))

(let1 ht (make-hash-table)
  (hash-table-update! ht 'key 'value)
  (hash-table-update! ht 'key1 'value1)
  (hash-table-update! ht 'key2 'value2)
  (hash-table-clear! ht)
  (check-true (hash-table-empty? ht)))

(check (hash-table-size empty-ht) => 0)

(let1 populated-ht (make-hash-table)
  (hash-table-set! populated-ht 'key1 'value1)
  (hash-table-set! populated-ht 'key2 'value2)
  (hash-table-set! populated-ht 'key3 'value3)
  (check (hash-table-size populated-ht) => 3))

(check (hash-table-keys empty-ht) => '())

(let1 ht (make-hash-table)
  (hash-table-set! ht 'k1 'v1)
  (check (hash-table-keys ht) => '(k1)))

(check (hash-table-values empty-ht) => '())

(let1 ht (make-hash-table)
  (hash-table-set! ht 'k1 'v1)
  (check (hash-table-values ht) => '(v1)))

(check-report)

