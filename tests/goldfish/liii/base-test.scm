;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii check)
        (liii base)
        (liii list))

(check-set-mode! 'report-failed)

(check (if (> 3 2) 3 2) => 3)
(check (if (< 3 2) 3 2) => 2)

(check (if (and (> 3 1) (< 3 4)) 'true-branch 'false-branch) => 'true-branch)
(check (if (or (> 3 4) (< 3 1)) 'true-branch 'false-branch) => 'false-branch)

(check (case '+
         ((+ -) 'p0)
         ((* /) 'p1))
  => 'p0)

(check (case '-
         ((+ -) 'p0)
         ((* /) 'p1))
  => 'p0)

(check (case '*
         ((+ -) 'p0)
         ((* /) 'p1))
  => 'p1)

(check (case '@
         ((+ -) 'p0)
         ((* /) 'p1))
  => #<unspecified>)

(check (case '&
         ((+ -) 'p0)
         ((* /) 'p1))
  => #<unspecified>)

(check-true (and #t #t #t))
(check-false (and #t #f #t))
(check-false (and #f #t #f))
(check-false (and #f #f #f))

(check-true (and))

(check-true (and 1 '() "non-empty" #t))
(check-false (and #f '() "non-empty" #t))
(check-false (and 1 '() "non-empty" #f))

(check-true (and (> 5 3) (< 5 10)))
(check-false (and (> 5 3) (> 5 10)))

(check-catch 'error-name
  (and (error 'error-name "This should not be evaluated") #f))
(check-false (and #f (error "This should not be evaluated")))

(check (and #t 1) => 1)

(check (when #t 1) => 1)

(check (when #f 1 ) => #<unspecified>)

(check (when (> 3 1) 1 ) => 1)

(check (when (> 1 3) 1 ) => #<unspecified>)

(check (let ((x 1)) x) => 1)

(check (let ((x 1) (y 2)) (+ x y)) => 3)

(check (let ((x 1))
         (let ((x 2))
           x)) => 2)

(check (let ((x 1))
         (if (> x 0)
             x
             -x)) => 1)

(define (test-letrec)
  (letrec ((even?
             (lambda (n)
               (if (= n 0)
                   #t
                   (odd? (- n 1)))))
            (odd?
             (lambda (n)
               (if (= n 0)
                   #f
                   (even? (- n 1))))))
    (list (even? 10) (odd? 10))))

(check (test-letrec) => (list #t #f))

(check-catch 'wrong-type-arg
  (letrec ((a 1) (b (+ a 1))) (list a b)))

(check
  (letrec* ((a 1) (b (+ a 1))) (list a b))
  => (list 1 2))

(check (let-values (((ret) (+ 1 2))) (+ ret 4)) => 7)
(check (let-values (((a b) (values 3 4))) (+ a b)) => 7)

(check (and-let* ((hi 3) (ho #f)) (+ hi 1)) => #f)
(check (and-let* ((hi 3) (ho #t)) (+ hi 1)) => 4)

(check
  (do ((i 0 (+ i 1)))
      ((= i 5) i))
  => 5)

(check
  (do ((i 0 (+ i 1))
       (sum 0 (+ sum i)))
      ((= i 5) sum))
  => 10)

(check
  (do ((i 0))
      ((= i 5) i)
    (set! i (+ i 1)))
  => 5)

(check
  (let1 vec (make-vector 5)
    (do ((i 0 (+ i 1)))
        ((= i 5) vec)
      (vector-set! vec i i)))
  => #(0 1 2 3 4))

(let ()
  (define-values (value1 value2) (values 1 2))
  (check value1 => 1)
  (check value2 => 2))

(define-record-type :pare
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

(check (pare? (kons 1 2)) => #t)
(check (pare? (cons 1 2)) => #f)
(check (kar (kons 1 2)) => 1)
(check (kdr (kons 1 2)) => 2)

(check
 (let ((k (kons 1 2)))
   (set-kar! k 3)
   (kar k))
  => 3)

(define-record-type :person
  (make-person name age)
  person?
  (name get-name set-name!)
  (age get-age))

(check (person? (make-person "Da" 3)) => #t)
(check (get-age (make-person "Da" 3)) => 3)
(check (get-name (make-person "Da" 3)) => "Da")
(check
  (let ((da (make-person "Da" 3)))
    (set-name! da "Darcy")
    (get-name da))
  => "Darcy")

(check-true (exact? 1))
(check-true (exact? 1/2))
(check-false (exact? 0.3))
; (check-true (exact? #e3.0))

(let1 zero-int 0
  (check-true (and (integer? zero-int) (zero? zero-int))))
(let1 zero-exact (- 1/2 1/2)
  (check-true (and (exact? zero-exact) (zero? zero-exact))))
(let1 zero-inexact 0.0
  (check-true (and (inexact? zero-inexact) (zero? zero-inexact))))

(check-false (zero? 1+1i))
(check-false (zero? #b11))

(check-catch 'wrong-type-arg (zero? #\A))
(check-catch 'wrong-type-arg (zero? #t))
(check-catch 'wrong-type-arg (zero? #f))

(check-true (positive? 1))
(check-true (positive? 0.1))
(check-true (positive? 1/2))
(check-true (positive? +inf.0))
(check-true (positive? 1+0i))

(check-false (positive? 0))
(check-false (positive? -1))
(check-false (positive? -1.1))
(check-false (positive? -1/2))
(check-false (positive? -inf.0))
(check-false (positive? +nan.0))

(check-catch 'wrong-type-arg (positive? 1+1i))
(check-catch 'wrong-type-arg (positive? #\A))
(check-catch 'wrong-type-arg (positive? #t))
(check-catch 'wrong-type-arg (positive? "not-a-number"))
(check-catch 'wrong-type-arg (positive? 'symbol))
(check-catch 'wrong-type-arg (positive? '(1 2 3)))

(check-true (negative? -1))
(check-true (negative? -0.1))
(check-true (negative? -1/2))
(check-true (negative? -inf.0))
(check-true (negative? -1+0i))

(check-false (negative? 0))
(check-false (negative? 1))
(check-false (negative? 1.1))
(check-false (negative? 1/2))
(check-false (negative? +inf.0))
(check-false (negative? -nan.0))

(check-catch 'wrong-type-arg (negative? -1-1i))
(check-catch 'wrong-type-arg (negative? #\A))
(check-catch 'wrong-type-arg (negative? #t))
(check-catch 'wrong-type-arg (negative? "not-a-number"))
(check-catch 'wrong-type-arg (negative? 'symbol))
(check-catch 'wrong-type-arg (negative? '(1 2 3)))

(check-true (odd? 1))
(check-false (odd? 0))

(check-catch 'wrong-type-arg (odd? 1+i))
(check-catch 'wrong-type-arg (odd? 1.0))
(check-catch 'wrong-type-arg (odd? 0.0))
(check-catch 'wrong-type-arg (odd? #\A))
(check-catch 'wrong-type-arg (odd? #t))
(check-catch 'wrong-type-arg (odd? #f))

(check-true (even? 0))
(check-false (even? 1))

(check-catch 'wrong-type-arg (even? 0.0))
(check-catch 'wrong-type-arg (even? 1.0))
(check-catch 'wrong-type-arg (even? 1+i))
(check-catch 'wrong-type-arg (even? #\A))
(check-catch 'wrong-type-arg (even? #t))
(check-catch 'wrong-type-arg (even? #f))

(check (max 1) => 1)
(check (max 1 2) => 2)
(check (max 1 2 3) => 3)
(check (max 3 2 1) => 3)
(check (max -1 -2 -3) => -1)
(check (max 0 0 0) => 0)
(check (max 1.5 2.5 3.5) => 3.5)
(check (max 1/2 3/4 1/4) => 3/4)

(check-catch 'wrong-number-of-args (max))

(check-catch 'wrong-type-arg (max 1 "2" 3))
(check-catch 'wrong-type-arg (max 1 #\a 3))
(check-catch 'wrong-type-arg (max 1 'symbol 3))

(check-catch 'wrong-type-arg (max #t #f))
(check-catch 'wrong-type-arg (max 1 #t 3))

(check-catch 'wrong-type-arg (max 1 '(2) 3))
(check-catch 'wrong-type-arg (max '(1 2 3) '(4 5 6)))

(check (min 7) => 7)
(check (min 7 3) => 3)
(check (min -1 0 1 7 -5 3) => -5)
(check (min -5 -10 -7 -3) => -10)

(check (min +inf.0 7) => 7)
(check (min -inf.0 7) => -inf.0)

(check-catch 'wrong-number-of-args (min))

(check-catch 'wrong-type-arg (min 'hello 7))

(check-catch 'wrong-type-arg (min "world" 7))

(check-catch 'wrong-type-arg (min #t 7))
(check-catch 'wrong-type-arg (min #f 7))

(check-catch 'wrong-type-arg (min '(1 3 5) 7))
(check-catch 'wrong-type-arg (min '() 7))

(check-catch 'wrong-type-arg (min 1+2i 2))

(check (+ 1 2) => 3)
(check (+ ) => 0)

(check (+ #x7fffffffffffffff 1) => #x8000000000000000)

(check (floor 1.1) => 1.0)
(check (floor 1) => 1)
(check (floor 1/2) => 0)
(check (floor 0) => 0)
(check (floor -1) => -1)
(check (floor -1.2) => -2.0)

(check (s7-floor 1.1) => 1)
(check (s7-floor -1.2) => -2)

(check (ceiling 1.1) => 2.0)
(check (ceiling 1) => 1)
(check (ceiling 1/2) => 1)
(check (ceiling 0) => 0)
(check (ceiling -1) => -1)
(check (ceiling -1.2) => -1.0)

(check (s7-ceiling 1.1) => 2)
(check (s7-ceiling -1.2) => -1)

(check (truncate 1.1) => 1.0)
(check (truncate 1) => 1)
(check (truncate 1/2) => 0)
(check (truncate 0) => 0)
(check (truncate -1) => -1)
(check (truncate -1.2) => -1.0)

(check (s7-truncate 1.1) => 1)
(check (s7-truncate -1.2) => -1)

(check (round 1.1) => 1.0)
(check (round 1.5) => 2.0)
(check (round 1) => 1)
(check (round 1/2) => 0)
(check (round 0) => 0)
(check (round -1) => -1)
(check (round -1.2) => -1.0)
(check (round -1.5) => -2.0)

(check (floor-quotient 11 2) => 5)
(check (floor-quotient 11 -2) => -6)
(check (floor-quotient -11 2) => -6)
(check (floor-quotient -11 -2) => 5)

(check (floor-quotient 10 2) => 5)
(check (floor-quotient 10 -2) => -5)
(check (floor-quotient -10 2) => -5)
(check (floor-quotient -10 -2) => 5)

(check-catch 'division-by-zero (floor-quotient 11 0))
(check-catch 'division-by-zero (floor-quotient 0 0))

(check (floor-quotient 0 2) => 0)
(check (floor-quotient 0 -2) => 0)

(check (quotient 11 2) => 5)
(check (quotient 11 -2) => -5)
(check (quotient -11 2) => -5)
(check (quotient -11 -2) => 5)

(check-catch 'division-by-zero (quotient 11 0))
(check-catch 'division-by-zero (quotient 0 0))
(check-catch 'wrong-type-arg (quotient 1+i 2))

(check (gcd) => 0)
(check (gcd 0) => 0)
(check (gcd 1) => 1)
(check (gcd 2) => 2)
(check (gcd -1) => 1)

(check (gcd 0 1) => 1)
(check (gcd 1 0) => 1)
(check (gcd 1 2) => 1)
(check (gcd 1 10) => 1)
(check (gcd 2 10) => 2)
(check (gcd -2 10) => 2)

(check (gcd 2 3 4) => 1)
(check (gcd 2 4 8) => 2)
(check (gcd -2 4 8) => 2)

(check (lcm) => 1)
(check (lcm 1) => 1)
(check (lcm 0) => 0)
(check (lcm 32 -36) =>  288)
(check (lcm 32 -36.0) => 288.0)
(check (lcm 2 4) => 4)
(check (lcm 2 4.0) => 4.0)
(check (lcm 2.0 4.0) => 4.0)
(check (lcm 2.0 4) => 4.0)

(check (square 2) => 4)

(check (number->string 123) => "123")
(check (number->string -456) => "-456")

(check (number->string 123 2) => "1111011")

(check (number->string 123 8) => "173")

(check (number->string 255 16) => "ff")

(check-catch 'wrong-type-arg (number->string 123 'not-a-number))

(check (number->string 1/2) => "1/2")
(check (number->string 1/2 2) => "1/10")
(check (number->string 3/4 2) => "11/100")

(check (number->string 123.456) => "123.456")

(check (number->string 1+2i) => "1.0+2.0i")
(check (number->string 0+2i) => "0.0+2.0i")

(check-true (boolean=? #t #t))
(check-true (boolean=? #f #f))
(check-true (boolean=? #t #t #t))
(check-false (boolean=? #t #f))
(check-false (boolean=? #f #t))

(check-true (symbol? 'foo))
(check-true (symbol? (car '(foo bar))))
(check-true (symbol? 'nil))

(check-false (symbol? "bar"))
(check-false (symbol? #f))
(check-false (symbol? '()))
(check-false (symbol? '123))

(check-catch 'wrong-number-of-args (symbol=? 'a))
(check-catch 'wrong-number-of-args (symbol=? 1))

(check-true (symbol=? 'a 'a))
(check-true (symbol=? 'foo 'foo))
(check-false (symbol=? 'a 'b))
(check-false (symbol=? 'foo 'bar))

(check-true (symbol=? 'bar 'bar 'bar))

(check-true (symbol=? (string->symbol "foo") (string->symbol "foo")))
(check-false (symbol=? (string->symbol "foo") (string->symbol "bar")))

(check-false (symbol=? 1 1))
(check-false (symbol=? 'a 1))
(check-false (symbol=? (string->symbol "foo") 1))

(check-false (symbol=? 'a 'b '()))

(check (symbol->string `MathAgape) => "MathAgape")
(check (symbol->string 'goldfish-scheme) => "goldfish-scheme")

(check (symbol->string (string->symbol "MathApage")) => "MathApage")
(check (symbol->string (string->symbol "Hello World")) => "Hello World")

(check (string->symbol "MathAgape") => `MathAgape)
(check-false (equal? (string->symbol "123") '123))
(check (string->symbol "+") => '+)

(check (string->symbol (symbol->string `MathAgape)) => `MathAgape)

(check (char? #\A) => #t)
(check (char? 1) => #f)

(check (char=? #\A #\A) => #t)
(check (char=? #\A #\A #\A) => #t)
(check (char=? #\A #\a) => #f)

(check (char->integer #\A) => 65)
(check (char->integer #\a) => 97)
(check (char->integer #\newline) => 10)
(check (char->integer #\space) => 32)
(check (char->integer #\tab) => 9)

(check (integer->char 65) => #\A)   
(check (integer->char 97) => #\a)  
(check (integer->char 48) => #\0)
(check (integer->char 36) => #\$)

(check (bytevector 1) => #u8(1))
(check (bytevector) => #u8())
(check (bytevector 1 2 3) => #u8(1 2 3))

(check (bytevector 255) => #u8(255))
(check-catch 'wrong-type-arg (bytevector 256))
(check-catch 'wrong-type-arg (bytevector -1))

(check-true (bytevector? #u8(0)))
(check-true (bytevector? #u8()))

(check (make-bytevector 3 0) => #u8(0 0 0))
(check (make-bytevector 3 3) => #u8(3 3 3))

(check (bytevector-append #u8() #u8()) => #u8())
(check (bytevector-append #u8() #u8(1)) => #u8(1))
(check (bytevector-append #u8(1) #u8()) => #u8(1))

(check (u8-string-length "中文") => 2)

(check (utf8->string (bytevector #x48 #x65 #x6C #x6C #x6F)) => "Hello")
(check (utf8->string #u8(#xC3 #xA4)) => "ä")
(check (utf8->string #u8(#xE4 #xB8 #xAD)) => "中")
(check (utf8->string #u8(#xF0 #x9F #x91 #x8D)) => "👍")

(check-catch 'value-error (utf8->string (bytevector #xFF #x65 #x6C #x6C #x6F)))

(check (string->utf8 "Hello") => (bytevector #x48 #x65 #x6C #x6C #x6F))
(check (utf8->string (string->utf8 "Hello" 1 2)) => "e")
(check (utf8->string (string->utf8 "Hello" 0 2)) => "He")
(check (utf8->string (string->utf8 "Hello" 2)) => "llo")
(check (utf8->string (string->utf8 "Hello" 2 5)) => "llo")

(check-catch 'out-of-range (string->utf8 "Hello" 2 6))

(check (utf8->string (string->utf8 "汉字书写")) => "汉字书写")
(check (utf8->string (string->utf8 "汉字书写" 1)) => "字书写")
(check (utf8->string (string->utf8 "汉字书写" 2)) => "书写")
(check (utf8->string (string->utf8 "汉字书写" 3)) => "写")

(check-catch 'out-of-range (string->utf8 "汉字书写" 4))

(check (string->utf8 "ä") => #u8(#xC3 #xA4))
(check (string->utf8 "中") => #u8(#xE4 #xB8 #xAD))
(check (string->utf8 "👍") => #u8(#xF0 #x9F #x91 #x8D))

(check (u8-substring "汉字书写" 0 1) => "汉")
(check (u8-substring "汉字书写" 0 4) => "汉字书写")
(check (u8-substring "汉字书写" 0) => "汉字书写")

(check (apply + (list 3 4)) => 7)
(check (apply + (list 2 3 4)) => 9)

(check (values 4) => 4)
(check (values) => #<unspecified>)

(check (+ (values 1 2 3) 4) => 10)

(check (string-ref ((lambda () (values "abcd" 2)))) => #\c)

(check (+ (call/cc (lambda (ret) (ret 1 2 3))) 4) => 10)

(check (call-with-values (lambda () (values 4 5))
                         (lambda (x y) x))
       => 4)

(check (*) => 1)
(check (call-with-values * -) => -1)

(check
  (receive (a b) (values 1 2) (+ a b))
  => 3)

(guard (condition
         (else
          (display "condition: ")
          (write condition)
          (newline)
          'exception))
  (+ 1 (raise 'an-error)))
; PRINTS: condition: an-error

(guard (condition
         (else
          (display "something went wrong")
          (newline)
          'dont-care))
 (+ 1 (raise 'an-error)))
; PRINTS: something went wrong

(with-input-from-string "(+ 1 2)"
  (lambda ()
    (let ((datum (read))) 
      (check-true (list? datum))
      (check datum => '(+ 1 2)))))

(check (eof-object) => #<eof>)

(check (== (list 1 2) (list 1 2)) => #t)
(check (!= (list 1 2) (list 1 2)) => #f)

(check (== (list 1 2) (list 1 2)) => #t)
(check (!= (list 1 2) (list 1 2)) => #f)

(check
  (with-output-to-string
    (lambda ()
      (display* "hello world" "\n")))
  => "hello world\n")

(check (in? 1 (list )) => #f)
(check (in? 1 (list 3 2 1)) => #t)
(check (in? #\x "texmacs") => #t)
(check (in? 1 (vector )) => #f)
(check (in? 1 (vector 3 2 1)) => #t)
(check-catch 'type-error (in? 1 "123"))

(check (let1 x 1 x) => 1)
(check (let1 x 1 (+ x 1)) => 2)

(check-true ((compose not zero?) 1))
(check-false ((compose not zero?) 0))

(let1 add1/add (lambda* (x (y 1)) (+ x y))
  (check (add1/add 1) => 2)
  (check (add1/add 0) => 1)
  (check (add1/add 1 2)=> 3))

(define add3
  (typed-lambda
    ((i integer?) (x real?) z)
    (+ i x z)))

(check (add3 1 2 3) => 6)
(check-catch 'type-error (add3 1.2 2 3))

(check-report)

(check (make-list 3 #\a) => (list #\a #\a #\a))
(check (make-list 3) => (list #f #f #f))

(check (make-list 0) => (list ))

(check-true (pair? '(a . b)))
(check-true (pair? '(a b c)))

(check-false (pair? '()))
(check-false (pair? '#(a b)))

(check-true (list? '()))
(check-true (list? '(a)))
(check-true (list? '(a b c)))
(check-true (list? '(1 . 2)))
(check-true (list? '(1 2 . 3)))

(check-true (list? '((a) (b) (c))))
(check-true (list? '(a (b) c)))

(check-true (list? (let ((x '(1 2 3))) (set-cdr! (cddr x) x) x)))

(check-false (list? #t))
(check-false (list? #f))
(check-false (list? 123))
(check-false (list? "Hello"))
(check-false (list? '#(1 2 3))) 
(check-false (list? '#()))
(check-false (list? '12345))

(check (null? '()) => #t)
(check (null? '(1)) => #f)
(check (null? '(1 2)) => #f)

(check (car '(a b c . d)) => 'a)
(check (car '(a b c)) => 'a)

(check-catch 'wrong-type-arg (car '()))

(check (cdr '(a b c . d)) => '(b c . d))
(check (cdr '(a b c)) => '(b c))
  
(check-catch 'wrong-type-arg (cdr '()))

(check (caar '((a . b) . c)) => 'a)

(check-catch 'wrong-type-arg (caar '(a b . c)))
(check-catch 'wrong-type-arg (caar '()))

(check (list-ref (cons '(1 2) '(3 4)) 1) => 3)

(check (list-ref '(a b c) 2) => 'c)

(check-catch 'wrong-type-arg (list-ref '() 0))

(check-catch 'out-of-range (list-ref '(a b c) -1))
(check-catch 'out-of-range (list-ref '(a b c) 3))

(check (length ()) => 0)
(check (length '(a b c)) => 3)
(check (length '(a (b) (c d e))) => 3)

(check (length 2) => #f)
(check (length '(a . b)) => -1)

(check (append '(a) '(b c d)) => '(a b c d))
(check (append '(a b) 'c) => '(a b . c))

(check (append () 'c) => 'c)
(check (append) => '())

(check (reverse '()) => '())
(check (reverse '(a)) => '(a))
(check (reverse '(a b)) => '(b a))

(check (map square (list 1 2 3 4 5)) => '(1 4 9 16 25))

(check
  (let ((v (make-vector 5)))
    (for-each (lambda (i) (vector-set! v i (* i i)))
              (iota 5))
    v)
  => #(0 1 4 9 16))

(check
  (let ((v (make-vector 5 #f)))
    (for-each (lambda (i) (vector-set! v i (* i i)))
              (iota 4))
    v)
  => #(0 1 4 9 #f))

(check
  (let ((v (make-vector 5 #f)))
    (for-each (lambda (i) (vector-set! v i (* i i)))
              (iota 0))
    v)
  => #(#f #f #f #f #f))

(check (memq #f '(1 #f 2 3)) => '(#f 2 3))
(check (memq 'a '(1 a 2 3)) => '(a 2 3))
(check (memq 2 '(1 2 3)) => '(2 3))

(check (memq 2.0 '(1 2.0 3)) => #f)
(check (memq 2+0i '(1 2+0i 3)) => #f)

(define num1 3)
(define num2 3)
(check (memq num1 '(3 num2)) => '(3 num2))
(check (memq 3 '(num1 num2)) => #f)
(check (memq 'num1 '(num1 num2)) => '(num1 num2))

(check (memq (+ 1 1) '(1 2 3)) => '(2 3))

(check (memv 2 '(1 2 3)) => '(2 3))
(check (memv 2.0 '(1 2.0 3)) => '(2.0 3))
(check (memv 2+0i '(1 2+0i 3)) => '(2+0i 3))

(check (memv 2 '(1 2.0 3)) => #f)
(check (memv 2 '(1 2+0i 3)) => #f)

(check (member 2 '(1 2 3)) => '(2 3))
(check (member 0 '(1 2 3)) => #f)
(check (member 0 '()) => #f)
 
(check (member "1" '(0 "1" 2 3)) => '("1" 2 3))
(check (member '(1 . 2) '(0 (1 . 2) 3)) => '((1 . 2) 3))
(check (member '(1 2) '(0 (1 2) 3)) => '((1 2) 3))

(check (string? "MathAgape") => #t)
(check (string? "") => #t)

(check (string? 'MathAgape) => #f)
(check (string? #/MathAgape) => #f)
(check (string? 123) => #f)
(check (string? '(1 2 3)) => #f)

(check (string->list "MathAgape")
  => '(#\M #\a #\t #\h #\A #\g #\a #\p #\e))

(check (string->list "") => '())

(check
  (list->string '(#\M #\a #\t #\h #\A #\g #\a #\p #\e))
  => "MathAgape")

(check (list->string '()) => "")

(check (string-length "MathAgape") => 9)
(check (string-length "") => 0)

(check
  (catch 'wrong-type-arg
    (lambda () (string-length 'not-a-string))
    (lambda args #t))
  =>
  #t)

(check (string-ref "MathAgape" 0) => #\M)
(check (string-ref "MathAgape" 2) => #\t)

(check-catch 'out-of-range (string-ref "MathAgape" -1))
(check-catch 'out-of-range (string-ref "MathAgape" 9))
(check-catch 'out-of-range (string-ref "" 0))

(check (string-append "Math" "Agape") => "MathAgape")

(check (string-append) => "")

(check (make-vector 1 1) => (vector 1))
(check (make-vector 3 'a) => (vector 'a 'a 'a))

(check (make-vector 0) => (vector ))
(check (vector-ref (make-vector 1) 0) => #<unspecified>)

(check (vector 'a 'b 'c) => #(a b c))
(check (vector) => #())

(check (vector-append #(0 1 2) #(3 4 5)) => #(0 1 2 3 4 5))

(check (vector? #(1 2 3)) => #t)
(check (vector? #()) => #t)
(check (vector? '(1 2 3)) => #f)

(check (vector-length #(1 2 3)) => 3)
(check (vector-length #()) => 0)

(let1 v #(1 2 3)
  (check (vector-ref v 0) => 1)
  (check (v 0) => 1)
  
  (check (vector-ref v 2) => 3)
  (check (v 2) => 3))

(check-catch 'out-of-range (vector-ref #(1 2 3) 3))
(check-catch 'out-of-range (vector-ref #() 0))
  
(check-catch 'wrong-type-arg (vector-ref #(1 2 3) 2.0))
(check-catch 'wrong-type-arg (vector-ref #(1 2 3) "2"))

(define my-vector #(0 1 2 3))
(check my-vector => #(0 1 2 3))

(check (vector-set! my-vector 2 10) => 10)
(check my-vector => #(0 1 10 3))

(check-catch 'out-of-range (vector-set! my-vector 4 10))

(check (vector->list #()) => '())
(check (vector->list #() 0) => '())

(check-catch 'out-of-range (vector->list #() 1))

(check (vector->list #(0 1 2 3)) => '(0 1 2 3))
(check (vector->list #(0 1 2 3) 1) => '(1 2 3))
(check (vector->list #(0 1 2 3) 1 1) => '())
(check (vector->list #(0 1 2 3) 1 2) => '(1))

(check (list->vector '(0 1 2 3)) => #(0 1 2 3))
(check (list->vector '()) => #())

