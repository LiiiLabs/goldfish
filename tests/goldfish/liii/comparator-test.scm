(import (srfi srfi-128)
        (liii check))

(check-set-mode! 'report-failed)

(check (boolean<? #t #t) => #f)
(check (boolean<? #f #f) => #f)
(check (boolean<? #f #t) => #t)
(check (boolean<? #t #f) => #f)

(check (> (boolean-hash #t) 0) => #t)
(check (boolean-hash #f) => 0)

(check-report)

