(import (liii check)
        (liii case))

(check-set-mode! 'report-failed)

(define-case-class person
  ((name string? "Bob")
   (age integer?)))

(let1 bob (person :name "Bob" :age 21)
  (check (bob 'name) => "Bob")
  (check (bob 'age) => 21)
  (check ((bob :name "hello") 'name) => "hello")
  (check-catch 'value-error (bob 'sex))
  (check-true (person? bob)))

(check-true (person=? (person "Bob" 21) (person "Bob" 21)))
(check-false (person=? (person "Bob" 21) (person "Bob" 20)))

(check-catch 'type-error (person 1 21))

(let ((bob (person "Bob" 21))
      (get-name (lambda (x)
                 (case* x
                   ((#<person?>) (x 'name))
                   (else (???))))))
  (check (get-name bob) => "Bob")
  (check-catch 'not-implemented-error (get-name 1)))

(define-case-class jerson
  ((name string?)
   (age integer?))
  
  (define (to-string)
    (string-append "I am " name " " (number->string age) " years old!"))
  (define (greet x)
    (string-append "Hi " x ", " (to-string)))
)

(let1 bob (jerson "Bob" 21)
  (check (bob 'to-string) => "I am Bob 21 years old!")
  (check (bob 'greet "Alice") => "Hi Alice, I am Bob 21 years old!"))

; 0 clause BSD, from S7 repo s7test.scm
(define (scase x)
  (case* x
    ((a b) 'a-or-b)
    ((1 2/3 3.0) => (lambda (a) (* a 2)))
    ((pi) 1 123)
    (("string1" "string2"))
    ((#<symbol?>) 'symbol!)
    (((+ x #<symbol?>)) 'got-list)
    ((#(1 x 3)) 'got-vector)
    (((+ #<>)) 'empty)
    (((* #<x:symbol?> #<x>)) 'got-label)
    (((#<> #<x:> #<x>)) 'repeated)
    (((#<symbol?> #<symbol?>)) 'two)
    (((#<x:> #<x>)) 'pair)
    ((#(#<x:> #<x>)) 'vector)
    ((#(#<symbol?> #<...> #<number?>)) 'vectsn)
    ((#(#<...> #<number?>)) 'vectstart)
    ((#(#<string?> #<char-whitespace?> #<...>)) 'vectstr)
    (else 'oops)))

(test (scase 3.0) 6.0)
(test (scase 'pi) 123)
(test (scase "string1") "string1")
(test (scase "string3") 'oops)
(test (scase 'a) 'a-or-b)
(test (scase 'abc) 'symbol!)
(test (scase #()) 'oops)
(test (scase '(+ x z)) 'got-list)
(test (scase #(1 x 3)) 'got-vector)
(test (scase '(+ x 3)) 'oops)
(test (scase '(+ x)) 'empty)
(test (scase '(* z z)) 'got-label)
(test (scase '(* z x)) 'oops)
(test (scase '(+ (abs x) (abs x))) 'repeated)
(test (scase '(+ (abs x) (abs y))) 'oops)
(test (scase '(a b)) 'two)
(test (scase '(1 1)) 'pair)
(test (scase '(1 1 2)) 'oops)
(test (scase #(1 1)) 'vector)
(test (scase #(a b c 3)) 'vectsn)
(test (scase #(1 b 2)) 'vectstart)
(test (scase #("asdf" #\space +nan.0 #<eof>)) 'vectstr)
(test (scase #(a 3)) 'vectsn)
(test (scase #(1)) 'vectstart)
(test (scase #("asdf" #\space)) 'vectstr)
(test (scase #("asdf")) 'oops)

(check-report)

