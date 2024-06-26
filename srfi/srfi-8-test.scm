;;; Copyright (c) 2024 Da @ Liii Network Inc
;;; All rights reverved.

(autoload 'check "srfi-78.scm")
(autoload 'receive "srfi-8.scm")

(check
  (receive (a b) (values 1 2) (+ a b))
  =>
  3)

