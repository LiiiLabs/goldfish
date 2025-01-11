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
        (liii scala)
        (liii cut))

(check-set-mode! 'report-failed)

(let ((opt1 (option 42))
      (opt2 (option '())))

  (check (opt1 :map (lambda (x) (+ x 1))
               :map (lambda (x) (* x 2))
               :get) => 86)
  (check (opt2 :map (lambda (x) (+ x 1))
               :map (lambda (x) (* x 2))
               :empty?) => #t)

  (check (opt1 :flat-map (lambda (x) (option (+ x 1)))
               :flat-map (lambda (x) (option (* x 2)))
               :get) => 86)
  (check (opt2 :flat-map (lambda (x) (option (+ x 1)))
               :flat-map (lambda (x) (option (* x 2)))
               :empty?) => #t)

  (check (opt1 :filter (lambda (x) (> x 40))
               :filter (lambda (x) (< x 50))
               :get) => 42)
  (check (opt1 :filter (lambda (x) (> x 50))
               :filter (lambda (x) (< x 60))
               :empty?) => #t)
  (check (opt2 :filter (lambda (x) (> x 40))
               :filter (lambda (x) (< x 50))
               :empty?) => #t)

  (check (opt1 :defined?) => #t)
  (check (opt1 :empty?) => #f)
  (check (opt2 :defined?) => #f)
  (check (opt2 :empty?) => #t)

  (check (opt1 :get) => 42)
  (check-catch 'value-error (opt2 :get))

  (check (opt1 :get-or-else 0) => 42)
  (check (opt2 :get-or-else 0) => 0)

  (check (opt1 :get-or-else (lambda () 0)) => 42)
  (check (opt2 :get-or-else (lambda () 0)) => 0)
)

(check (((box 1) :to 2) :collect) => (list 1 2))
(check (((box 1) :to 1) :collect) => (list 1))
(check (((box 2) :to 1) :collect) => (list ))

(check (((box 1) :until 3) :collect) => (list 1 2))
(check (((box 1) :until 2) :collect) => (list 1))
(check (((box 2) :until 2) :collect) => (list ))

(check ((case-string "abc") :map char-upcase :unbox) => "ABC")

(check-true ((case-string "") :empty?))
(check-false ((case-string "abc") :empty?))

(check ((case-string "abc") :length) => 3)

(let ((lst (case-list '(1 2 3 4 5))))
  (check (lst :take -1 :collect) => '())
  (check (lst :take 0 :collect) => '())
  (check (lst :take 3 :collect) => '(1 2 3))
  (check (lst :take 5 :collect) => '(1 2 3 4 5))
  (check (lst :take 10 :collect) => '(1 2 3 4 5))
)

(let ((lst (case-list '(1 2 3 4 5))))
  (check (lst :take-right -1 :collect) => '())
  (check (lst :take-right 0 :collect) => '())
  (check (lst :take-right 3 :collect) => '(3 4 5))
  (check (lst :take-right 5 :collect) => '(1 2 3 4 5))
  (check (lst :take-right 10 :collect) => '(1 2 3 4 5))
)

(let1 l (case-list '(1 2 3))
  (check-true (l :forall positive?)))

(let1 l (case-list '(1 2 3))
  (check-true (l :exists even?)))

(let1 l (case-list '(1 2 3))
  (check-true (l :contains 1))
  (check-false (l :contains 4)))

(let1 lst (case-list '(1 2 3 4 5))
  (check ((lst :find (lambda (x) (= x 3))) :get) => 3)
  (check ((lst :find (lambda (x) (> x 2))) :get) => 3)

  (check ((lst :find (lambda (x) (> x 10))) :empty?) => #t)

  (check ((lst :find even?) :get) => 2)

  (check ((lst :find (lambda (x) (< x 0))) :empty?) => #t)
)

(check ((case-list (list 1 2 3)) :count) => 3)
(check ((case-list (list 1 2 3)) :count (cut > <> 1)) => 2)

(let ((lst (case-list '(1 2 3 4 5))))
  (check (lst :forall (lambda (x) (> x 0))) => #t)
  (check (lst :forall (lambda (x) (> x 3))) => #f)
)

(let ((empty-lst (case-list '())))
    (check (empty-lst :forall (lambda (x) (> x 0))) => #t))

(let ((lst (case-list '(1 2 3 4 5))))
  (check (lst :fold 0 +) => 15)
  (check (lst :fold '() (lambda (x acc) (cons x acc))) => '(5 4 3 2 1))

  (check (lst :fold-right 0 +) => 15)
  (check (lst :fold-right '() (lambda (x acc) (cons x acc))) => '(1 2 3 4 5))
)

(let1 l (case-list (list 1 2 3))
  (check (l :make-string) => "123")
  (check (l :make-string " ") => "1 2 3")
  (check (l :make-string "[" "," "]") => "[1,2,3]")
  
  (check-catch 'wrong-number-of-args (l :make-string "[" ","))
  (check-catch 'type-error (l :make-string 123 "," "]"))
  (check-catch 'type-error (l :make-string "[" 123 "]"))
  (check-catch 'type-error (l :make-string "[" "," 123))
)

(let ((vec (case-vector #(1 2 3 4 5))))
  (check (vec :take -1 :collect) => #())
  (check (vec :take 0 :collect) => #())
  (check (vec :take 3 :collect) => #(1 2 3))
  (check (vec :take 5 :collect) => #(1 2 3 4 5))
  (check (vec :take 10 :collect) => #(1 2 3 4 5))
)

(let ((vec (case-vector #(1 2 3 4 5))))
  (check (vec :take-right -1 :collect) => #())
  (check (vec :take-right 0 :collect) => #())
  (check (vec :take-right 3 :collect) => #(3 4 5))
  (check (vec :take-right 5 :collect) => #(1 2 3 4 5))
  (check (vec :take-right 10 :collect) => #(1 2 3 4 5))
)

(let ((vec (case-vector #(1 2 3 4 5))))
  (check ((vec :find (lambda (x) (= x 3))) :get) => 3)
  (check ((vec :find (lambda (x) (> x 2))) :get) => 3)

  (check ((vec :find (lambda (x) (> x 10))) :empty?) => #t)

  (check ((vec :find even?) :get) => 2)

  (check ((vec :find (lambda (x) (< x 0))) :empty?) => #t)
)

(let ((vec (case-vector #(1 2 3 4 5))))
  (check (vec :forall (lambda (x) (> x 0))) => #t)
  (check (vec :forall (lambda (x) (> x 3))) => #f))

(let ((empty-vec (case-vector #())))
  (check (empty-vec :forall (lambda (x) (> x 0))) => #t))

(let ((vec (case-vector #(1 2 3 4 5))))
  (check (vec :fold 0 +) => 15)
  (check (vec :fold '() (lambda (x acc) (cons x acc))) => '(5 4 3 2 1))

  (check (vec :fold-right 0 +) => 15)
  (check (vec :fold-right '() (lambda (x acc) (cons x acc))) => '(1 2 3 4 5))
)

(let1 v (box #(1 2 3))
  (check (v :count) => 3)
  (check (v :count (cut > <> 1)) => 2)
  (check (v :make-string) => "123")
  (check (v :make-string " ") => "1 2 3")
  (check (v :make-string "[" "," "]") => "[1,2,3]")
  
  (check-catch 'wrong-number-of-args (v :make-string "[" ","))
  (check-catch 'type-error (v :make-string 123 "," "]"))
  (check-catch 'type-error (v :make-string "[" 123 "]"))
  (check-catch 'type-error (v :make-string "[" "," 123))
)

(let1 ht (box (hash-table 'a 1 'b 2 'c 3))
  (let1 r (ht :map (lambda (k v) (values k (+ v 1)))
              :collect)
    (check (r 'a) => 2)
    (check (r 'b) => 3)
    (check (r 'c) => 4)))
      
(let1 ht (box (hash-table 'a 1 'b 2 'c 3))
  (check ((ht :get 'a) :get) => 1)
  (check ((ht :get 'd) :empty?) => #t))

(let1 ht (box (hash-table 'a 1 'b 2 'c 3))
  (check-true (ht :contains 'a))
  (check-false (ht :contains 'd)))

(check-report)

