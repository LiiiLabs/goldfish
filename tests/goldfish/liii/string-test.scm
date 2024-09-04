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

; Copyright (c) 2024 Da, Nian @ Liii Network Inc.
; All right reserved.

(import (liii check)
        (liii string)
        (srfi srfi-8))

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

(check
  (catch 'wrong-type-arg
    (lambda () 
      (string-join '() ":" 'strict-infix))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'wrong-type-arg
    (lambda () 
      (string-join '() ":" 2))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'wrong-type-arg
    (lambda () 
      (string-join '() ":" 'no-such-grammer))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'wrong-number-of-args
    (lambda () 
      (string-join '() ":" 1 2 3))
    (lambda args #t))
  =>
  #t)

(check-true (string-null? ""))

(check-false (string-null? "MathAgape"))

(check-false (string-null? 'not-a-string))

(check
  (string-every 
    #\x
    "xxxxxx")
  =>
  #t)

(check
  (string-every 
    #\x
    "xxx0xx")
  =>
  #f)

(check
  (string-every 
    char-numeric?
    "012345")
  =>
  #t)

(check
  (string-every 
    char-numeric?
    "012d45")
  =>
  #f)

(check
  (catch 'wrong-type-arg
    (lambda () 
      (string-every
         1
         "012345"))
    (lambda args #t))
  =>
  #t)

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

(check
  (string-every 
    char-numeric?
    "012345")
  =>
  #t)

(check
  (string-every 
    number?
    "012345")
  =>
  #f)

(check-true (string-every char-numeric? "ab2345" 2))

(check-false (string-every char-numeric? "ab2345" 1))

(check
  (string-every 
    char-numeric?
    "ab234f"
    2)
  =>
  #f)

(check
  (string-every 
    char-numeric?
    "ab234f"
    2
    4)
  =>
  #t)

(check
  (string-every 
    char-numeric?
    "ab234f"
    2
    2)
  =>
  #t)

(check
  (string-every 
    char-numeric?
    "ab234f"
    1
    4)
  =>
  #f)

(check
  (string-every 
    char-numeric?
    "ab234f"
    2
    5)
  =>
  #t)

(check
  (string-every 
    char-numeric?
    "ab234f"
    2
    6)
  =>
  #f)

(check
  (catch 'out-of-range
    (lambda () 
      (string-every 
        char-numeric?
       "ab234f"
       2
       7))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'out-of-range
    (lambda () 
      (string-every 
        char-numeric?
       "ab234f"
       2
       1))
    (lambda args #t))
  =>
  #t)

(check
  (catch 'wrong-number-of-args
    (lambda () 
      (string-every 
        char-numeric?
       "ab234f"
       2
       7
       1))
    (lambda args #t))
  =>
  #t)

(check
  (string-any 
    #\0
    "xxx0xx")
  =>
  #t)

(check
  (string-any 
    #\0
    "xxxxxx")
  =>
  #f)

(check
  (string-any 
    char-numeric?
    "xxx0xx")
  =>
  #t)

(check
  (string-any
    char-numeric?
    "xxxxxx")
  =>
  #f)

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

(check
  (string-any 
    char-alphabetic?
    "01c345"
    2)
  =>
  #t)

(check
  (string-any 
    char-alphabetic?
    "01c345"
    3)
  =>
  #f)

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

(check
  (catch 'wrong-number-of-args
    (lambda () 
      (string-any 
         char-alphabetic?
        "01c345"
       2
       7
       1))
    (lambda args #t))
  =>
  #t)

(check
  (string-take "MathAgape" 4)
  =>
  "Math")

(check
  (catch 'wrong-type-arg
    (lambda () (string-take "MathAgape" 20))
    (lambda args #t))
  =>
  #t)

(check
  (string-take-right "MathAgape" 1)
  =>
  "e")

(check
  (catch 'wrong-type-arg
    (lambda () (string-take-right "MathAgape" 20))
    (lambda args #t))
  =>
  #t)

(check
  (string-drop "MathAgape" 8)
  =>
  "e")

(check
  (catch 'wrong-type-arg
    (lambda () (string-drop "MathAgape" 20))
    (lambda args #t))
  =>
  #t)

(check
  (string-drop-right "MathAgape" 5)
  =>
  "Math")

(check
  (catch 'wrong-type-arg
    (lambda () (string-drop "MathAgape" 20))
    (lambda args #t))
  =>
  #t)

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
  (catch 'wrong-type-arg
    (lambda () (string-pad "MathAgape" -1))
    (lambda args #t))
  =>
  #t)

(check
  (string-pad-right "MathAgape" 15)
  =>
  "MathAgape      ")

(check
  (string-pad-right "MathAgape" 12 #\1)
  =>
  "MathAgape111")

(check
  (string-pad-right "MathAgape" 6 #\1 0 4)
  =>
  "Math11")

(check
  (string-pad-right "MathAgape" 9)
  =>
  "MathAgape")

(check
  (string-pad-right "MathAgape" 9 #\1)
  =>
  "MathAgape")

(check
  (string-pad-right "MathAgape" 4)
  =>
  "Math")

(check
  (string-pad "MathAgape" 2 #\1 0 4)
  =>
  "th")

(check
  (catch 'wrong-type-arg
    (lambda () (string-pad-right "MathAgape" -1))
    (lambda args #t))
  =>
  #t)


(check
  (string-trim "  2 4  ")
  =>
  "2 4  ")

(check
  (string-trim "  2 4  " 2)
  =>
  "2 4  ")

(check
  (string-trim "  2 4  " 3)
  =>
  "4  ")

(check
  (string-trim "  2 4  " 4)
  =>
  "4  ")

(check
  (string-trim "  2 4  " 5)
  =>
  "")

(check
  (catch 'out-of-range
    (lambda () (string-trim "  2 4  " 8))
    (lambda args #t))
  =>
  #t)

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

(check
  (string-trim-right "  2 4  ")
  =>
  "  2 4")

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

(check
  (string-trim-right "  2 4  " 0 4)
  =>
  "  2")

(check
  (string-trim-right "  2 4  " 0 7)
  =>
  "  2 4")

(check
  (catch 'out-of-range
    (lambda () (string-trim-right "  2 4  " 0 8))
    (lambda args #t))
  =>
  #t)

(check
  (string-trim-right "  2 4  " #\ )
  =>
  "  2 4")

(check
  (string-trim-right "-- 2 4 --" #\-)
  =>
  "-- 2 4 ")

(check
  (string-trim-right "012-" #\- 1)
  =>
  "12")

(check
  (string-trim-right "012-4" #\- 0 4)
  =>
  "012")

(check
  (string-trim-both "  2 4  ")
  =>
  "2 4")

(check
  (string-trim-both "--2 4--" #\-)
  =>
  "2 4")

(check
  (string-prefix? "Ma" "MathAgape")
  =>
  #t)

(check
  (string-prefix? "" "MathAgape")
  =>
  #t)

(check
  (string-prefix? "MathAgape" "MathAgape")
  =>
  #t)

(check
 (string-prefix? "a" "MathAgape")
  =>
  #f)

(check
  (string-suffix? "e" "MathAgape")
  =>
  #t)

(check
  (string-suffix? "" "MathAgape")
  =>
  #t)

(check
  (string-suffix? "MathAgape" "MathAgape")
  =>
  #t)

(check
 (string-suffix? "p" "MathAgape")
  =>
  #f)

(check
  (string-index "0123456789" #\2)
  =>
  2)

(check
  (string-index "0123456789" #\2 2)
  =>
  2)

(check
  (string-index "0123456789" #\2 3)
  =>
  #f)

(check
  (string-index "01x3456789" char-alphabetic?)
  =>
  2)

(check
  (string-index-right "0123456789" #\8)
  =>
  8)

(check
  (string-index-right "0123456789" #\8 2)
  =>
  8)

(check
  (string-index-right "0123456789" #\8 9)
  =>
  #f)

(check
  (string-index-right "01234567x9" char-alphabetic?)
  =>
  8)

(check
  (string-contains "0123456789" "3")
  =>
  #t)

(check
  (string-contains "0123456789" "34")
  =>
  #t)

(check
  (string-contains "0123456789" "24")
  =>
  #f)

(check (string-count "xyz" #\x) => 1)

(check (string-count "xyz" #\x 0 1) => 1)

(check (string-count "xyz" #\y 0 1) => 0)

(check (string-count "xyz" #\x 0 3) => 1)

(check (string-count "xyz" (lambda (x) (char=? x #\x))) => 1)

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
  (string-tokenize "1 22 333")
  => '("1" "22" "333"))

(check
  (string-tokenize "1 22 333" #\2)
  => '("1 " " 333"))

(check
  (string-tokenize "1 22 333" #\  2)
  => `("22" "333"))

(check-report)
(if (check-failed?) (exit -1))

