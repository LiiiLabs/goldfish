(import (liii check)
        (liii cut))

(check-set-mode! 'report-failed)

(check ((cut list <> 'y <>) 'x 'z) => '(x y z))
(check ((cut + 1 <...>) 2 3) => 6)
(check ((cut + 1 <...>)) => 1)
(check ((cut list <> <> <...>) 1 2 3) => '(1 2 3))
(check ((cut list <> <> <...>) 1 2) => '(1 2))
(check-catch 'wrong-number-of-args ((cut list <> <>) 1))
(check-catch 'wrong-number-of-args ((cut list <> <> <...>) 1))
(check-catch 'syntax-error ((cut list <> <> <...> <>) 1 2 3))

(check-report)
