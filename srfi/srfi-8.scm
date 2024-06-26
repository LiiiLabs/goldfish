;;; Copyright (c) 2024 Da @ Liii Network Inc
;;; All rights reverved.

(provide 'srfi-8)
(provide 'receive)

(define-macro (receive formals expression . body)
  `(call-with-values
    (lambda () (values ,expression))
    (lambda ,formals ,@body)))

