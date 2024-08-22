(import (srfi srfi-78))

(check-set-mode! 'report-failed)

(check (char-upcase #\A) => #\A)
(check (char-upcase #\a) => #\A)
(check (char-upcase #\?) => #\?)
(check (char-upcase #\$) => #\$)
(check (char-upcase #\.) => #\.)
(check (char-upcase #\\) => #\\)
(check (char-upcase #\5) => #\5)
(check (char-upcase #\)) => #\))
(check (char-upcase #\%) => #\%)
(check (char-upcase #\0) => #\0)
(check (char-upcase #\_) => #\_)
(check (char-upcase #\?) => #\?)
(check (char-upcase #\space) => #\space)
(check (char-upcase #\newline) => #\newline)
(check (char-upcase #\null) => #\null)

(check-report)
(if (check-failed?) (exit -1))

