; Liii Network Inc.
; All right reserved

(import (liii check)
        (liii hash-table)
        (liii base))

(check-set-mode! 'report-failed)

(let1 ht (make-hash-table)
  (check (ht 'a) => #f)
  (hash-table-set! ht 'a 1)
  (check (ht 'a) => 1))

(check-report)

