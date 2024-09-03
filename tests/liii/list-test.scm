(import (liii list)
        (liii check))

(check-set-mode! 'report-failed)

(check ((length= 3 (list 1 2 3))) => #t)
(check ((length= 2 (list 1 2 3))) => #f)
(check ((length= 4 (list 1 2 3))) => #f)

(check ((list-view (list 1 2 3))) => (list 1 2 3))

(check (((list-view (list 1 2 3))
        map (lambda (x) (+ x 1)))) => (list 2 3 4))

(check (((list-view (list 1 2 3))
        map (lambda (x) (+ x 1))
        map (lambda (x) (* x x))))
       => (list 4 9 16))

(check (not-null-list? (list 1)) => #t)
(check (list-not-null? (list 1)) => #t)
(check (list-null? (list 1)) => #f)

(check (not-null-list? (list 1 2 3)) => #t)
(check (list-not-null? (list 1 2 3)) => #t)
(check (list-null? (list 1 2 3)) => #f)

(check (not-null-list? '(a)) => #t)
(check (list-not-null? '(a)) => #t)
(check (list-null? '(a)) => #f)

(check (not-null-list? '(a b c)) => #t)
(check (list-not-null? '(a b c)) => #t)
(check (list-null? '(a b c)) => #f)

(check (not-null-list? ()) => #f)
(check (list-not-null? ()) => #f)
(check (list-null? ()) => #t)

; '(a) is a pair and a list
; '(a . b) is a pair but not a list
(check (not-null-list? '(a . b)) => #f)
(check (list-not-null? '(a . b)) => #f)
(check (list-null? '(a . b)) => #f)

(check-catch 'type-error (not-null-list? 1))
(check (list-not-null? 1) => #f)
(check (list-null? 1) => #f)

(check-report)

