; Copyright (c) 2024 Da, Nian @ Liii Network Inc.
; All right reserved.

(import (scheme base)
        (srfi srfi-1)
        (liii check)
        (srfi srfi-133))

(check-set-mode! 'report-failed)

(for-each (lambda (p) (check (procedure? p) => #t))
  (list
   vector-empty?
   vector-count
   vector-any vector-every vector-copy vector-copy!
   vector-index vector-index-right vector-partition
   vector-swap!))

(check-true (vector-empty? (vector)))
(check-false (vector-empty? (vector 1)))
(check-catch 'type-error (vector-empty? 1))

(check (vector-count even? #()) => 0)
(check (vector-count even? #(1 3 5 7 9)) => 0)
(check (vector-count even? #(1 3 4 7 8)) => 2)

(check (vector-any even? #()) => #f)
(check (vector-any even? #(1 3 5 7 9)) => #f)
(check (vector-any even? #(1 3 4 7 8)) => #t)

(check (vector-every odd? #()) => #t)
(check (vector-every odd? #(1 3 5 7 9)) => #t)
(check (vector-every odd? #(1 3 4 7 8)) => #f)

(check (vector-index even? #()) => #f)
(check (vector-index even? #(1 3 5 7 9)) => #f)
(check (vector-index even? #(1 3 4 7 8)) => 2)

(check (vector-index-right even? #()) => #f)
(check (vector-index-right even? #(1 3 5 7 9)) => #f)
(check (vector-index-right even? #(1 3 4 7 8)) => 4)

(define (vector-partition->list pred v)
  (let-values (((ret cnt) (vector-partition pred v))) (list ret cnt)))

(check (vector-partition->list even? #()) => '(#() 0))
(check (vector-partition->list even? #(1 3 5 7 9)) => '(#(1 3 5 7 9) 0))
(check (vector-partition->list even? #(1 3 4 7 8)) => '(#(4 8 1 3 7) 2))

(define my-vector (vector 0 1 2 3))
(vector-swap! my-vector 1 2)
(check my-vector => #(0 2 1 3))

(define my-vector (vector 0 1 2 3))
(vector-swap! my-vector 1 1)
(check my-vector => #(0 1 2 3))

(define my-vector (vector 0 1 2 3))
(vector-swap! my-vector 0 (- (vector-length my-vector) 1))
(check my-vector => #(3 1 2 0))

(check-catch
 'out-of-range
 (vector-swap! my-vector 1 (vector-length my-vector)))

(check-report)

