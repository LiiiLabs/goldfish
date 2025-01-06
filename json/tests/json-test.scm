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

(set! *load-path* (cons "json" *load-path*))

(import (liii check)
        (liii json))

;(check-set-mode! 'report-failed)

(check (json-string-escape "hello") => "\"hello\"")
(check (json-string-escape "hello\"world") => "\"hello\\\"world\"")
(check (json-string-escape "hello\\world") => "\"hello\\\\world\"")
(check (json-string-escape "hello/world") => "\"hello\\/world\"")
(check (json-string-escape "hello\bworld") => "\"hello\\bworld\"")
(check (json-string-escape "hello\fworld") => "\"hello\\fworld\"")
(check (json-string-escape "hello\nworld") => "\"hello\\nworld\"")
(check (json-string-escape "hello\rworld") => "\"hello\\rworld\"")
(check (json-string-escape "hello\tworld") => "\"hello\\tworld\"")

(check (string->json "{\"age\":18}") => `(("age" . 18)))
(check (string->json "{age:18}") => `((age . 18)))
(check (string->json "{\"name\":\"中文\"}") => `(("name" . "中文"))) 

(check (string->json "\"\"") => "")

(check (string->json "{\"name\":\"Alice\\nBob\"}") => '(("name" . "Alice\nBob")))
(check (string->json "{\"name\":\"Alice\\tBob\"}") => '(("name" . "Alice\tBob")))
(check (string->json "{\"name\":\"Alice\\rBob\"}") => '(("name" . "Alice\rBob")))
(check (string->json "{\"name\":\"Alice\\bBob\"}") => '(("name" . "Alice\bBob")))
(check (string->json "{\"name\":\"Alice\\fBob\"}") => '(("name" . "Alice\fBob")))
(check (string->json "{\"name\":\"Alice\\\\Bob\"}") => '(("name" . "Alice\\Bob")))
(check (string->json "{\"name\":\"Alice\\\/Bob\"}") => '(("name" . "Alice/Bob")))
(check (string->json "{\"name\":\"Alice\\\"Bob\"}") => '(("name" . "Alice\"Bob")))

(check (json->string '(("age" . 18))) => "{\"age\":18}")
(check (json->string #(0 1 2 3)) => "[0,1,2,3]")

(check (json-ref '(("age" . 18)) "age") => 18)
(check (json-ref #(0 1 2 3) 0) => 0)

(check (json-ref* `((bob . ((age . 18) (sex . male)))) 'bob 'age) => 18)

(let* ((j0 `((age . 18) (sex . male)))
       (j1 (json-set j0 'age 19))
       (j2 (json-set j0 'age 'null)))
      (check (json-ref j0 'age) => 18)
      (check (json-ref j1 'age) => 19)
      (check (json-ref j2 'age) => '()))

(let1 j `(("age" . 18) ("sex" . male))
      (check (json-ref (json-set j "age" 19) "age") => 19)
      (check (json-ref j "age") => 18))

(let* ((j0 #(red green blue))
       (j1 (json-set j0 0 'black)))
  (check j0 => #(red green blue))
  (check j1 => #(black green blue)))

(let* ((j0 `((bob . 18) (jack . 16)))
       (j1 (json-set j0 #t 3))
       (j2 (json-set j0 #t (lambda (x) (+ x 1)))))
  (check j1 => `((bob . 3) (jack . 3)))
  (check j2 => `((bob . 19) (jack . 17))))

(let* ((json '((name . "Alice") (age . 25)))
       (updated-json (json-set* json 'age 26)))
  (check (json-ref updated-json 'age) => 26))

(let* ((json '((person . ((name . "Alice")
                          (age . 25)))))
       (updated-json (json-set* json 'person 'age 26)))
  (check (json-ref* updated-json 'person 'age) => 26))

(let* ((json '((person . ((name . "Alice")
                          (age . 25)
                          (address . ((city . "Wonderland")
                                      (zip . "12345")))))))
       (updated-json (json-set* json 'person 'address 'city "Newland")))
  (check (json-ref* updated-json 'person 'address 'city) => "Newland"))

(let* ((json '((name . "Alice") (age . 25)))
       (updated-json (json-set* json 'age (lambda (x) (+ x 1)))))
  (check (json-ref updated-json 'age) => 26))

(let* ((json '((person . ((name . "Alice") (age . 25)))))
       (updated-json (json-set* json 'person 'age (lambda (x) (+ x 1)))))
  (check (json-ref* updated-json 'person 'age) => 26))

(let ((json '((person . ((name . "Alice") (age . 25))))))
  (let ((updated-json (json-push* json 'person 'city "Wonderland")))
    (check (json-ref* updated-json 'person 'city) => "Wonderland")))

(let ((json '(("person" . (("name" . "Alice") ("age" . 25))))))
  (let ((updated-json (json-push* json "person" "city" "Wonderland")))
    (check (json-ref* updated-json "person" "city") => "Wonderland")))

(let ((json '((person . ((name . "Alice") (age . 25) (address . ((city . "Oldland") (zip . "12345"))))))))
  (let ((updated-json (json-push* json 'person 'address 'street "Main St")))
    (check (json-ref* updated-json 'person 'address 'street) => "Main St")))

(let ((json '((data . #(1 2 3)))))
  (let ((updated-json (json-push* json 'data 3 4)))
    (check updated-json => '((data . #(1 2 3 4))))))

(let ((json '((data . #(#(1 2) #(3 4))))))
  (let ((updated-json (json-push* json 'data 1 2 5)))
    (check updated-json => '((data . #(#(1 2) #(3 4 5)))))))

(let ((json '((data . ((0 . "zero") (1 . "one"))))))
  (let ((updated-json (json-push* json 'data 2 "two")))
    (check (json-ref* updated-json 'data 2) => "two")))

(let ((json '((flags . ((#t . "true") (#f . "false"))))))
  (let ((updated-json (json-push* json 'flags #t "yes")))
    (check (json-ref* updated-json 'flags #t) => "yes")))

(let* ((json '((name . "Alice") (age . 25))))
  (let ((updated-json (json-drop json 'age)))
    (check (json-ref updated-json 'age) => '())))

(let* ((json '((name . "Alice")
               (age . 25)
               (address . ((city . "Wonderland")
                           (zip . "12345"))))))
  (let ((updated-json (json-drop* json 'address 'city)))
    (check (json-ref* updated-json 'address 'city) => '())))

(let* ((json '((name . "Alice")
               (age . 25)
               (address . ((city . "Wonderland")
                           (zip . "12345"))))))
  (let1 j1 (json-drop json (lambda (k) (equal? k 'city)))
    (check (json-ref* j1 'address 'city) => "Wonderland"))
  (let1 j2 (json-drop json (lambda (k) (equal? k 'name)))
    (check (json-ref* j2 'name) => '()))
  (let1 j3 (json-drop* json 'address (lambda (k) (equal? k 'city)))
    (check (json-ref* j3 'address 'city) => '())))

(let ((json '((name . "Alice") (age . 25))))
  (check (json-reduce json 'name (lambda (k v) (string-upcase v)))
         => '((name . "ALICE") (age . 25))))

(let ((json '((person . ((name . "Alice") (age . 25))))))
  (check (json-reduce json 'person (lambda (k v) v))
         => '((person . ((name . "Alice") (age . 25))))))

(let ((json '((name . "Alice") (age . 25))))
  (check (json-reduce json (lambda (k) (equal? k 'age)) (lambda (k v) (+ v 1)))
         => '((name . "Alice") (age . 26))))

(let ((json '((person . ((name . "Alice") (age . 25))))))
  (check (json-reduce json (lambda (k) (equal? k 'person)) (lambda (k v) v))
         => '((person . ((name . "Alice") (age . 25))))))

(let ((json '((name . "Alice") (age . 25))))
  (check (json-reduce json #t (lambda (k v) (if (string? v) (string-upcase v) v)))
         => '((name . "ALICE") (age . 25))))

(let ((json '((name . "Alice") (age . 25))))
  (check (json-reduce json #f (lambda (k v) v))
         => '((name . "Alice") (age . 25))))

(let ((json '()))
  (check (json-reduce json 'name (lambda (k v) v))
         => '()))

(let ((json #()))
  (check (json-reduce json 'name (lambda (k v) v))
         => #()))

(let ((json '((person . ((name . "Alice") (age . 25))))))
  (check (json-reduce* json 'person 'name (lambda (k v) (string-upcase v)))
         => '((person . ((name . "ALICE") (age . 25))))))

(let1 json '((person . ((name . "Alice")
                        (age . 25)
                        (address . ((city . "Wonderland")
                                    (zip . "12345"))))))
  (let ((updated-json (json-reduce* json 'person 'address 'city (lambda (x y) (string-upcase y)))))
    (check (json-ref* updated-json 'person 'address 'city) => "WONDERLAND")))

(check-report)

