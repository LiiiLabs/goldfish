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

(check (let-values (((ret) (+ 1 2))) (+ ret 4)) => 7)

(check (let-values (((a b) (values 3 4))) (+ a b)) => 7)

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

(check (square 2) => 4)

(check (pair? '(a . b)) => #t)
(check (pair? '(a b c)) => #t)
(check (pair? '()) => #f)
(check (pair? '#(a b)) => #f)

(check (car '(a b c . d)) => 'a)

(check (car '(a b c)) => 'a)

(check
  (catch 'wrong-type-arg
    (lambda () (car '()))
    (lambda args #t))
  =>
  #t)

(check (cdr '(a b c . d)) => '(b c . d))

(check (cdr '(a b c)) => '(b c))
  
(check
  (catch 'wrong-type-arg
    (lambda () (cdr '()))
    (lambda args #t))
  =>
  #t)

(check (caar '((a . b) . c)) => 'a)

(check
  (catch 'wrong-type-arg
    (lambda () (caar '(a b . c)))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'wrong-type-arg
    (lambda () (caar '()))
    (lambda args #t))
  =>
  #t)

(check (null? '()) => #t)
(check (null? '(1)) => #f)
(check (null? '(1 2)) => #f)

(check (make-list 3 #\a) => (list #\a #\a #\a))
(check (make-list 3) => (list #f #f #f))

(check (make-list 0) => (list ))

(check (length ()) => 0)
(check (length '(a b c)) => 3)
(check (length '(a (b) (c d e))) => 3)

(check (length 2) => #f)
(check (length '(a . b)) => -1)

(check (append '(a) '(b c d)) => '(a b c d))
(check (append '(a b) 'c) => '(a b . c))

(check (append () 'c) => 'c)
(check (append) => '())

(check (list-ref (cons '(1 2) '(3 4)) 1) => 3)

(check (list-ref '(a b c) 2) => 'c)

(check
  (catch 'wrong-type-arg
    (lambda () (list-ref '() 0))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'out-of-range
    (lambda () (list-ref '(a b c) -1))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'out-of-range
    (lambda () (list-ref '(a b c) 3))
    (lambda args #t))
  =>
  #t)

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

(check (char? #\A) => #t)
(check (char? 1) => #f)

(check (char=? #\A #\A) => #t)
(check (char=? #\A #\A #\A) => #t)
(check (char=? #\A #\a) => #f)

(check (string? "MathAgape") => #t)
(check (string? "") => #t)

(check (string? 'MathAgape) => #f)
(check (string? #/MathAgape) => #f)
(check (string? 123) => #f)
(check (string? '(1 2 3)) => #f)

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

(check
  (catch 'out-of-range
    (lambda () (string-ref "MathAgape" -1))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'out-of-range
    (lambda () (string-ref "MathAgape" 9))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'out-of-range
    (lambda () (string-ref "" 0))
    (lambda args #t))
  =>
  #t)

(check (string-append "Math" "Agape") => "MathAgape")

(check (string-append) => "")

(check
  (string->list "MathAgape")
  =>
  '(#\M #\a #\t #\h #\A #\g #\a #\p #\e))

(check (string->list "") => '())

(check
  (list->string '(#\M #\a #\t #\h #\A #\g #\a #\p #\e))
  =>
  "MathAgape")

(check (list->string '()) => "")

(define original-string "MathAgape")
(define copied-string (string-copy original-string))

(check (equal? original-string copied-string) => #t)
(check (eq? original-string copied-string) => #f)

(check
  (equal? (string-copy "MathAgape" 4)
          (string-copy "MathAgape" 4))
  =>
  #t)

(check
  (eq? (string-copy "MathAgape" 4)
       (string-copy "MathAgape" 4))
  =>
  #f)

(check
  (equal? (string-copy "MathAgape" 4 9)
          (string-copy "MathAgape" 4 9))
  =>
  #t)

(check
  (eq? (string-copy "MathAgape" 4 9)
       (string-copy "MathAgape" 4 9))
  =>
  #f)

(check (vector? #(1 2 3)) => #t)
(check (vector? #()) => #t)
(check (vector? '(1 2 3)) => #f)

(check (make-vector 1 1) => (vector 1))
(check (make-vector 3 'a) => (vector 'a 'a 'a))

(check (make-vector 0) => (vector ))
(check (vector-ref (make-vector 1) 0) => #<unspecified>)

(check (vector 'a 'b 'c) => #(a b c))
(check (vector) => #())

(check (vector-length #(1 2 3)) => 3)
(check (vector-length #()) => 0)

(check (vector-ref #(1 2 3) 0) => 1)
(check (vector-ref #(1 2 3) 2) => 3)

(check
  (catch 'out-of-range
    (lambda () (vector-ref #(1 2 3) 3))
    (lambda args #t))
  =>
  #t)
  
(check
  (catch 'out-of-range
    (lambda () (vector-ref #() 0))
    (lambda args #t))
  =>
  #t)
  
(check
  (catch 'wrong-type-arg
    (lambda () (vector-ref #(1 2 3) 2.0))
    (lambda args #t))
  =>
  #t)
  
(check
  (catch 'wrong-type-arg
    (lambda () (vector-ref #(1 2 3) "2"))
    (lambda args #t))
  =>
  #t)

(define my-vector #(0 1 2 3))
(check my-vector => #(0 1 2 3))

(check (vector-set! my-vector 2 10) => 10)
(check my-vector => #(0 1 10 3))

(check
  (catch 'out-of-range
    (lambda () (vector-set! my-vector 4 10))
    (lambda args #t))
  =>
  #t)

(check (vector->list #()) => '())
(check (vector->list #() 0) => '())

(check
  (catch 'out-of-range
    (lambda () (vector->list #() 1))
    (lambda args #t))
  =>
  #t)

(check (vector->list #(0 1 2 3)) => '(0 1 2 3))
(check (vector->list #(0 1 2 3) 1) => '(1 2 3))
(check (vector->list #(0 1 2 3) 1 1) => '())
(check (vector->list #(0 1 2 3) 1 2) => '(1))

(check (list->vector '(0 1 2 3)) => #(0 1 2 3))
(check (list->vector '()) => #())

(check (vector->string (vector #\0 #\1 #\2 #\3)) => "0123")
(check (vector->string (vector #\a #\b #\c)) => "abc")

(check (vector->string (vector #\0 #\1 #\2 #\3) 0 4) => "0123")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1) => "123")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1 4) => "123")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1 3) => "12")
(check (vector->string (vector #\0 #\1 #\2 #\3) 1 2) => "1")

(check
  (catch 'out-of-range
    (lambda () (vector->string (vector #\0 #\1 #\2 #\3) 2 10))
    (lambda args #t))
  =>
  #t)

(check (vector->string (vector 0 1 #\2 3 4) 2 3) => "2")

(check
  (catch 'wrong-type-arg
    (lambda () (vector->string (vector 0 1 #\2 3 4) 1 3))
    (lambda args #t))
  =>
  #t)

(check (vector-copy #(0 1 2 3)) => #(0 1 2 3))
(check (vector-copy #(0 1 2 3) 1) => #(1 2 3))
(check (vector-copy #(0 1 2 3) 3) => #(3))
(check (vector-copy #(0 1 2 3) 4) => #())

(check
  (catch 'out-of-range
    (lambda () (vector-copy #(0 1 2 3) 5))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'out-of-range
    (lambda () (vector-copy #(0 1 2 3) 1 5))
    (lambda args #t))
  =>
  #t)

(define my-vector #(0 1 2 3))
(check (eqv? my-vector (vector-copy #(0 1 2 3))) => #f)
(check 
  (eqv? (vector-ref my-vector 2)
        (vector-ref (vector-copy #(0 1 2 3)) 2))
  =>
  #t)

(check (vector-copy #(0 1 2 3) 1 1) => #())
(check (vector-copy #(0 1 2 3) 1 2) => #(1))
(check (vector-copy #(0 1 2 3) 1 4) => #(1 2 3))

(define a (vector "a0" "a1" "a2" "a3" "a4"))
(define b (vector "b0" "b1" "b2" "b3" "b4"))

;(< at 0)
(check
  (catch 'out-of-range
    (lambda () (vector-copy! b -1 a))
    (lambda args #t))
  =>
  #t)

;(< start 0)
(check
  (catch 'out-of-range
    (lambda () (vector-copy! b 0 a -1))
    (lambda args #t))
  =>
  #t)

;(> start (vector-length from))
(check
  (catch 'out-of-range
    (lambda () (vector-copy! b 0 a 6))
    (lambda args #t))
  =>
  #t)

;(> end (vector-length from))
(check
  (catch 'out-of-range
    (lambda () (vector-copy! b 0 a 0 6))
    (lambda args #t))
  =>
  #t)

;(> start end)
(check
  (catch 'out-of-range
    (lambda () (vector-copy! b 0 a 2 1))
    (lambda args #t))
  =>
  #t)

;(> (+ at (- end start)) (vector-length to))
(check
  (catch 'out-of-range
    (lambda () (vector-copy! b 6 a))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'out-of-range
    (lambda () (vector-copy! b 1 a))
    (lambda args #t))
  =>
  #t)

(define a (vector "a0" "a1" "a2" "a3" "a4"))
(define b (vector "b0" "b1" "b2" "b3" "b4"))
(vector-copy! b 0 a 1)
(check b => #("a1" "a2" "a3" "a4" "b4"))

(define a (vector "a0" "a1" "a2" "a3" "a4"))
(define b (vector "b0" "b1" "b2" "b3" "b4"))
(vector-copy! b 0 a 0 5)
(check b => #("a0" "a1" "a2" "a3" "a4")) 

(check (vector-append #(0 1 2) #(3 4 5)) => #(0 1 2 3 4 5))

(define my-vector (vector 0 1 2 3 4))
(fill! my-vector #f)
(check my-vector => #(#f #f #f #f #f)) 

(define my-vector (vector 0 1 2 3 4))
(fill! my-vector #f 1 2)
(check my-vector => #(0 #f 2 3 4)) 

(check (apply + (list 3 4)) => 7)
(check (apply + (list 2 3 4)) => 9)

(check (map square (list 1 2 3 4 5)) => '(1 4 9 16 25))

(check
  (string-map
    (lambda (ch) (integer->char (+ 1 (char->integer ch))))
    "HAL")
  => "IBM")

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

(check
  (let ((lst '()))
    (string-for-each
      (lambda (x) (set! lst (cons (char->integer x) lst)))
      "12345")
    lst)
  => '(53 52 51 50 49))

(check
  (let ((lst '()))
    (string-for-each
      (lambda (x) (set! lst (cons (- (char->integer x) (char->integer #\0)) lst)))
      "12345")
    lst)
  => '(5 4 3 2 1))

(check
  (let ((lst '()))
    (string-for-each
      (lambda (x) (set! lst (cons (- (char->integer x) (char->integer #\0)) lst)))
      "123")
    lst)
  => '(3 2 1))

(check
  (let ((lst '()))
    (string-for-each
      (lambda (x) (set! lst (cons (- (char->integer x) (char->integer #\0)) lst)))
      "")
    lst)
  => '())

(check
  (let ((lst (make-list 5)))
    (vector-for-each
      (lambda (i) (list-set! lst i (* i i)))
      #(0 1 2 3 4))
    lst)
  => '(0 1 4 9 16))

(check
  (let ((lst (make-list 5)))
    (vector-for-each
      (lambda (i) (list-set! lst i (* i i)))
      #(0 1 2))
    lst)
  => '(0 1 4 #f #f))

(check
  (let ((lst (make-list 5)))
    (vector-for-each
      (lambda (i) (list-set! lst i (* i i)))
      #())
    lst)
  => '(#f #f #f #f #f))

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

(check (eof-object) => #<eof>)

