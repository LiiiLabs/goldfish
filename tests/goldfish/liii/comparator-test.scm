(import (srfi srfi-128)
        (liii check))

(check-set-mode! 'report-failed)

(define boolean-comparator
  (make-comparator boolean? eq? boolean<? default-hash))

(check-false (boolean<? #t #t))
(check-false (<? boolean-comparator #t #t))
(check-false (boolean<? #f #f))
(check-false (<? boolean-comparator #f #f))
(check-true (boolean<? #f #t))
(check-true (<? boolean-comparator #f #t))
(check-false (boolean<? #t #f))
(check-false (<? boolean-comparator #t #f))

(check-false (complex<? 1 1))
(check-true (complex<? 0+1i 0+2i))
(check-true (complex<? 1+2i 2+2i))

(check-report)

