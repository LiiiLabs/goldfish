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
        (liii json))

(check (string->json "{\"age\":18}") => `(("age" . 18)))
(check (string->json "{age:18}") => `((age . 18)))

(check (string->json "{\"name\":\"中文\"}") => `(("name" . "中文"))) 

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

(check-report)

