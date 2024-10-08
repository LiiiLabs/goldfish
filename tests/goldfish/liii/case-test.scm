(import (liii check)
        (liii case))

(check-set-mode! 'report-failed)

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

