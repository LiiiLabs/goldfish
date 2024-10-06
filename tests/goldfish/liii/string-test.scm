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
        (liii string))

(check-set-mode! 'report-failed)

(check (string-join '("a" "b" "c")) => "abc")

(check (string-join '("a" "b" "c") ":") => "a:b:c")
(check (string-join '("a" "b" "c") ":" 'infix) => "a:b:c")
(check (string-join '("a" "b" "c") ":" 'suffix) => "a:b:c:")
(check (string-join '("a" "b" "c") ":" 'prefix) => ":a:b:c")

(check (string-join '() ":") => "")
(check (string-join '() ":" 'infix) => "")
(check (string-join '() ":" 'prefix) => "")
(check (string-join '() ":" 'suffix) => "")

(check-catch 'value-error (string-join '() ":" 'strict-infix))
(check-catch 'type-error (string-join '() ":" 2))
(check-catch 'value-error (string-join '() ":" 'no-such-grammer))
(check-catch 'wrong-number-of-args (string-join '() ":" 1 2 3))

(check-true (string-null? ""))

(check-false (string-null? "MathAgape"))

(check-false (string-null? 'not-a-string))

(check-true (string-every #\x "xxxxxx"))
(check-false (string-every #\x "xxx0xx"))

(check-true (string-every char-numeric? "012345"))
(check-false (string-every char-numeric? "012d45"))

(check-catch 'wrong-type-arg (string-every 1 "012345"))

(check
  (catch 'wrong-type-arg
    (lambda () 
      (string-every
         #\012345
         "012345"))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'wrong-type-arg
    (lambda () 
      (string-every
         "012345"
         "012345"))
    (lambda args #t))
  =>
  #t)

(check-true (string-every char-numeric? "012345"))
(check-false (string-every number? "012345"))

(check-true (string-every char-numeric? "ab2345" 2))
(check-false (string-every char-numeric? "ab2345" 1))
(check-false (string-every  char-numeric? "ab234f" 2))
(check-true (string-every char-numeric? "ab234f" 2 4))
(check-true (string-every char-numeric? "ab234f" 2 2))
(check-false (string-every char-numeric? "ab234f" 1 4))
(check-true (string-every char-numeric? "ab234f" 2 5))
(check-false (string-every char-numeric? "ab234f" 2 6))

(check-catch 'out-of-range (string-every char-numeric? "ab234f" 2 7))
(check-catch 'out-of-range (string-every char-numeric? "ab234f" 2 1))

(check-true (string-any #\0 "xxx0xx"))
(check-false (string-any #\0 "xxxxxx"))
(check-true (string-any char-numeric? "xxx0xx"))
(check-false (string-any char-numeric? "xxxxxx"))

(check
  (catch 'wrong-type-arg
    (lambda () 
      (string-every
         0
         "xxx0xx"))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'wrong-type-arg
    (lambda () 
      (string-any
         (lambda (n) (= n 0))
         "xxx0xx"))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'wrong-type-arg
    (lambda () 
      (string-every
         "0"
         "xxx0xx"))
    (lambda args #t))
  =>
  #t)

(check-true (string-any char-alphabetic? "01c345" 2))

(check-false (string-any char-alphabetic? "01c345" 3))

(check
  (string-any 
    char-alphabetic?
    "01c345"
    2
    4)
  =>
  #t)

(check
  (string-any 
    char-alphabetic?
    "01c345"
    2
    2)
  =>
  #f)

(check
  (string-any 
    char-alphabetic?
    "01c345"
    3
    4)
  =>
  #f)

(check
  (string-any 
    char-alphabetic?
    "01c345"
    2
    6)
  =>
  #t)

(check
  (catch 'out-of-range
    (lambda () 
      (string-any 
         char-alphabetic?
        "01c345"
        2
        7))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'out-of-range
    (lambda () 
      (string-any 
         char-alphabetic?
        "01c345"
       2
       1))
    (lambda args #t))
  =>
  #t)

(define original-string "MathAgape")
(define copied-string (string-copy original-string))

(check-true (equal? original-string copied-string))
(check-false (eq? original-string copied-string))

(check-true
  (equal? (string-copy "MathAgape" 4)
          (string-copy "MathAgape" 4)))

(check-false
  (eq? (string-copy "MathAgape" 4)
       (string-copy "MathAgape" 4)))

(check-true
  (equal? (string-copy "MathAgape" 4 9)
          (string-copy "MathAgape" 4 9)))

(check-false
  (eq? (string-copy "MathAgape" 4 9)
       (string-copy "MathAgape" 4 9)))

(check (string-take "MathAgape" 4) => "Math")

(check-catch 'out-of-range (string-take "MathAgape" 20))

(check (string-take-right "MathAgape" 0) => "")
(check (string-take-right "MathAgape" 1) => "e")
(check (string-take-right "MathAgape" 9) => "MathAgape")

(check-catch 'out-of-range (string-take-right "MathAgape" 20))

(check (string-drop "MathAgape" 8) => "e")
(check (string-drop "MathAgape" 9) => "")
(check (string-drop "MathAgape" 0) => "MathAgape")

(check-catch 'out-of-range (string-drop "MahtAgape" -1))
(check-catch 'out-of-range (string-drop "MathAgape" 20))

(check (string-drop-right "MathAgape" 5) => "Math")
(check (string-drop-right "MathAgape" 9) => "")
(check (string-drop-right "MathAgape" 0) => "MathAgape")

(check-catch 'out-of-range (string-drop-right "MathAgape" -1))
(check-catch 'out-of-range (string-drop-right "MathAgape" 20))

(check
  (string-pad "MathAgape" 15)
  =>
  "      MathAgape")

(check
  (string-pad "MathAgape" 12 #\1)
  =>
  "111MathAgape")

(check
  (string-pad "MathAgape" 6 #\1 0 4)
  =>
  "11Math")

(check
  (string-pad "MathAgape" 9)
  =>
  "MathAgape")

(check
  (string-pad "MathAgape" 5)
  =>
  "Agape")

(check
  (string-pad "MathAgape" 2 #\1 0 4)
  =>
  "th")

(check
  (catch 'out-of-range
    (lambda () (string-pad "MathAgape" -1))
    (lambda args #t))
  =>
  #t)

(check (string-pad-right "MathAgape" 15) => "MathAgape      ")
(check (string-pad-right "MathAgape" 12 #\1) => "MathAgape111")
(check (string-pad-right "MathAgape" 6 #\1 0 4) => "Math11")
(check (string-pad-right "MathAgape" 9) => "MathAgape")
(check (string-pad-right "MathAgape" 9 #\1) => "MathAgape")
(check (string-pad-right "MathAgape" 4) => "Math")
(check (string-pad "MathAgape" 2 #\1 0 4) => "th")

(check-catch 'out-of-range (string-pad-right "MathAgape" -1))

(check (string-trim "  2 4  ") => "2 4  ")
(check (string-trim "  2 4  " 2) => "2 4  ")
(check (string-trim "  2 4  " 3) => "4  ")
(check (string-trim "  2 4  " 4) => "4  ")
(check (string-trim "  2 4  " 5) => "")

(check-catch 'out-of-range (string-trim "  2 4  " 8))

(check
  (string-trim "  2 4  " 0 4)
  =>
  "2 ")

(check
  (string-trim "  2 4  " 0 7)
  =>
  "2 4  ")

(check
  (catch 'out-of-range
    (lambda () (string-trim "  2 4  " 0 8))
    (lambda args #t))
  =>
  #t)

(check
  (string-trim "  2 4  " #\ )
  =>
  "2 4  ")

(check
  (string-trim "-- 2 4 --" #\-)
  =>
  " 2 4 --")

(check
  (string-trim " - 345" #\- 1)
  =>
  " 345")

(check
  (string-trim " - 345" #\- 1 4)
  =>
  " 3")

(check (string-trim-right "  2 4  ") => "  2 4")

(check
  (string-trim-right "  2 4  " 1)
  =>
  " 2 4")

(check
  (string-trim-right "  2 4  " 2)
  =>
  "2 4")

(check
  (string-trim-right "  2 4  " 3)
  =>
  " 4")

(check
  (string-trim-right "  2 4  " 4)
  =>
  "4")

(check
  (string-trim-right "  2 4  " 5)
  =>
  "")

(check
  (string-trim-right "  2 4  " 6)
  =>
  "")

(check
  (string-trim-right "  2 4  " 7)
  =>
  "")

(check
  (catch 'out-of-range
    (lambda () (string-trim-right "  2 4  " 8))
    (lambda args #t))
  =>
  #t)

(check (string-trim-right "  2 4  " 0 4) => "  2")
(check (string-trim-right "  2 4  " 0 7) => "  2 4")

(check
  (catch 'out-of-range
    (lambda () (string-trim-right "  2 4  " 0 8))
    (lambda args #t))
  =>
  #t)

(check (string-trim-right "  2 4  " #\ ) => "  2 4")
(check (string-trim-right "-- 2 4 --" #\-) => "-- 2 4 ")
(check (string-trim-right "012-" #\- 1) => "12")
(check (string-trim-right "012-4" #\- 0 4) => "012")

(check (string-trim-both "  2 4  ") => "2 4")
(check (string-trim-both "--2 4--" #\-) => "2 4")

(check-true (string-prefix? "Ma" "MathAgape"))
(check-true (string-prefix? "" "MathAgape"))
(check-true (string-prefix? "MathAgape" "MathAgape"))

(check-false (string-prefix? "a" "MathAgape"))

(check-true (string-suffix? "e" "MathAgape"))
(check-true (string-suffix? "" "MathAgape"))
(check-true (string-suffix? "MathAgape" "MathAgape"))

(check-false (string-suffix? "p" "MathAgape"))

(check (string-index "0123456789" #\2) => 2)
(check (string-index "0123456789" #\2 2) => 2)
(check (string-index "0123456789" #\2 3) => #f)
(check (string-index "01x3456789" char-alphabetic?) => 2)

(check (string-index-right "0123456789" #\8) => 8)
(check (string-index-right "0123456789" #\8 2) => 8)
(check (string-index-right "0123456789" #\8 9) => #f)
(check (string-index-right "01234567x9" char-alphabetic?) => 8)

(check-true (string-contains "0123456789" "3"))
(check-true (string-contains "0123456789" "34"))
(check-false (string-contains "0123456789" "24"))

(check (string-count "xyz" #\x) => 1)
(check (string-count "xyz" #\x 0 1) => 1)
(check (string-count "xyz" #\y 0 1) => 0)
(check (string-count "xyz" #\x 0 3) => 1)
(check (string-count "xyz" (lambda (x) (char=? x #\x))) => 1)

(check (string-upcase "abc") => "ABC")
(check (string-upcase "abc" 0 1) => "Abc")

(check-catch 'out-of-range (string-upcase "abc" 0 4))

(check (string-downcase "ABC") => "abc")
(check (string-downcase "ABC" 0 1) => "aBC")

(check-catch 'out-of-range (string-downcase "ABC" 0 4))

(check (string-reverse "01234") => "43210")

(check-catch 'out-of-range (string-reverse "01234" -1))

(check (string-reverse "01234" 0) => "43210")
(check (string-reverse "01234" 1) => "04321")
(check (string-reverse "01234" 5) => "01234")

(check-catch 'out-of-range (string-reverse "01234" 6))

(check (string-reverse "01234" 0 2) => "10234")
(check (string-reverse "01234" 1 3) => "02134")
(check (string-reverse "01234" 1 5) => "04321")
(check (string-reverse "01234" 0 5) => "43210")

(check-catch 'out-of-range (string-reverse "01234" 1 6))

(check-catch 'out-of-range (string-reverse "01234" -1 3))

(check
  (string-map
    (lambda (ch) (integer->char (+ 1 (char->integer ch))))
    "HAL")
  => "IBM")

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

(check (string-tokenize "1 22 333") => '("1" "22" "333"))
(check (string-tokenize "1 22 333" #\2) => '("1 " " 333"))
(check (string-tokenize "1 22 333" #\  2) => `("22" "333"))

(check (string-remove-prefix "浙江省杭州市西湖区" "浙江省") => "杭州市西湖区")
(check (string-remove-prefix "aaa" "a") => "aa")
(check (string-remove-prefix "abc" "bc") => "abc")
(check (string-remove-prefix "abc" "") => "abc")

(check (string-remove-suffix "aaa" "a") => "aa")
(check (string-remove-suffix "aaa" "") => "aaa")
(check (string-remove-suffix "Goldfish.tmu" ".tmu") => "Goldfish")

(check-report)

