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
        (liii lang)
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
)

(let ((opt1 (option 42)) (opt2 (option '())))
  (check (opt1 :get) => 42)
  (check-catch 'value-error (opt2 :get)))

(let ((opt1 (option 42)) (opt2 (option '())))
  (check (opt1 :get-or-else 0) => 42)
  (check (opt2 :get-or-else 0) => 0)

  (check (opt1 :get-or-else (lambda () 0)) => 42)
  (check (opt2 :get-or-else (lambda () 0)) => 0)
)

(check ((none) :get-or-else (box 1)) => (box 1))

(let ((opt1 (option 42)) (opt2 (option '())))
  (check (opt1 :or-else (option 0)) => (option 42))
  (check (opt2 :or-else (option 0)) => (option 0))
  (check-catch 'type-error (opt1 :or-else 0))
)

(check-true ((option "str") :equals (option "str")))

(let ((opt1 (option 42)) (opt2 (option '())))
  (check-true (opt1 :forall (lambda (x) (== x 42))))
  (check-false (opt2 :forall (lambda (x) (== x 42)))))

(let ((opt1 (option 42)) (opt2 (option '())))
  (check-true (opt1 :exists (lambda (x) (== x 42))))
  (check-false (opt2 :exists (lambda (x) (== x 42)))))

(check-true ((box 42) :equals (box 42)))
(check-false ((box 41) :equals (box 42)))

(check (((box 1) :to 2) :collect) => (list 1 2))
(check (((box 1) :to 1) :collect) => (list 1))
(check (((box 2) :to 1) :collect) => (list ))

(check (((box 1) :until 3) :collect) => (list 1 2))
(check (((box 1) :until 2) :collect) => (list 1))
(check (((box 2) :until 2) :collect) => (list ))

(check-catch 'value-error ((box #x110000) :to-char))

(check ((box 1) :to-string) => "1")

(check-true ((rich-char #x30) :equals (rich-char #x30)))
(check-false ((rich-char #x31) :equals (rich-char #x30)))

(let ((char1 (rich-char 48))  ;; ASCII '0'
      (char2 (rich-char #xFF10))  ;; 全角 '０'
      (char3 (rich-char #x0660))  ;; 阿拉伯数字 '٠'
      (char4 (rich-char #x06F0))  ;; 扩展阿拉伯数字 '۰'
      (char5 (rich-char #x0966))  ;; 印度数字
      (char6 (rich-char #x09E6))  ;; 孟加拉数字
      (char7 (rich-char #x0A66))  ;; 古尔穆奇数字
      (char8 (rich-char #x0AE6))  ;; 古吉拉特数字
      (char9 (rich-char #x0B66))  ;; 奥里亚数字
      (char10 (rich-char #x0BE6))  ;; 泰米尔数字
      (char11 (rich-char #x0C66))  ;; 泰卢固数字
      (char12 (rich-char #x0CE6))  ;; 卡纳达数字 
      (char13 (rich-char #x0D66))  ;; 马拉雅拉姆数字
      (char14 (rich-char #x0E50))  ;; 泰文数字 '๐'
      (char15 (rich-char #x0ED0))  ;; 老挝数字
      (char16 (rich-char #x0F20))  ;; 藏文数字
      (char17 (rich-char #x1040))  ;; 缅甸数字 '၀'
      (char18 (rich-char #x17E0))  ;; 高棉数字 '០'
      (char19 (rich-char #x1810))  ;; 蒙古数字 '᠐'
      (char20 (rich-char 65)))  ;; ASCII 'A'

  ;; 测试 %digit?
  (check (char1 :digit?) => #t)  ;; ASCII 数字
  (check (char2 :digit?) => #t)  ;; 全角数字
  (check (char3 :digit?) => #t)  ;; 阿拉伯数字
  (check (char4 :digit?) => #t)  ;; 扩展阿拉伯数字
  (check (char5 :digit?) => #t)  ;; 印度数字
  (check (char6 :digit?) => #t)  ;; 孟加拉数字
  (check (char7 :digit?) => #t)  ;; 古尔穆奇数字
  (check (char8 :digit?) => #t)  ;; 古吉拉特数字
  (check (char9 :digit?) => #t)  ;; 奥里亚数字
  (check (char10 :digit?) => #t)  ;; 泰米尔数字
  (check (char11 :digit?) => #t)  ;; 泰卢固数字
  (check (char12 :digit?) => #t)  ;; 卡纳达数字
  (check (char13 :digit?) => #t)  ;; 马拉雅拉姆数字
  (check (char14 :digit?) => #t)  ;; 泰文数字
  (check (char15 :digit?) => #t)  ;; 老挝数字
  (check (char16 :digit?) => #t)  ;; 藏文数字
  (check (char17 :digit?) => #t)  ;; 缅甸数字
  (check (char18 :digit?) => #t)  ;; 高棉数字
  (check (char19 :digit?) => #t)  ;; 蒙古数字
  (check (char20 :digit?) => #f))  ;; 非数字字符

(check ((rich-char #x41) :to-string) => "A")
(check-true ((box #\A) :equals (rich-char #x41)))

(check ((rich-char #xA3) :to-string) => "£")

(check ((rich-char #x4E2D) :to-string) => "中")
(check (object->string (rich-char #x4E2D)) => "中")

(check ((rich-char #x1F600) :to-string) => "😀")

(check ((box "abc") :get) => "abc")
(check ((box "") :get) => "")

(check ((rich-string "abc") :length) => 3)
(check ((rich-string "中文") :length) => 2)

(let1 str (box "你好，世界")  
  (check (str :char-at 0) => (rich-char #x4F60))  ;; "你" 的 Unicode 码点
  (check (str :char-at 1) => (rich-char #x597D))  ;; "好" 的 Unicode 码点
  (check (str :char-at 2) => (rich-char #xFF0C))  ;; "，" 的 Unicode 码点
  (check (str :char-at 3) => (rich-char #x4E16))  ;; "世" 的 Unicode 码点
  (check (str :char-at 4) => (rich-char #x754C))  ;; "界" 的 Unicode 码点
  (check-catch 'out-of-range (str :char-at 10)))

(let1 str (box "Hello，世界")
   (check (str 0) => (box #\H))
   (check (str 7) => (rich-char "界")))

(check (box "42") => (box "42"))
(check-false ((box "41") :equals (box "42")))

(check-true ((rich-string "") :empty?))
(check-false ((rich-string "abc") :empty?))

(let1 str (rich-string "Hello, World!")
  (check-true (str :contains #\W))
  (check-true (str :contains "Hello"))
  (check-true (str :contains "")))

(check ((rich-string "abc") :map char-upcase :get) => "ABC")

(let1 str (rich-string "Hello, World!")
  (check (str :forall char-alphabetic?) => #f)
  (check (str :exists char-alphabetic?) => #t)
  (check (str :count char-alphabetic?) => 10)
)

(check ((rich-string "hello") :to-string) => "hello")

(check ((box '(1 2 3)) :apply 0) => 1)
(check ((box '(1 2 3)) 0) => 1)

(let1 lst (rich-list '(1 2 3 4 5))
  (check ((lst :find (lambda (x) (= x 3))) :get) => 3)
  (check ((lst :find (lambda (x) (> x 2))) :get) => 3)

  (check ((lst :find (lambda (x) (> x 10))) :empty?) => #t)

  (check ((lst :find even?) :get) => 2)

  (check ((lst :find (lambda (x) (< x 0))) :empty?) => #t)
)

(check (box (list (box 1) (box 2) (box 3)))
  => (((box 1) :to 3) :map box))

(let1 lst (box '(1 2 3 4 5))
  (check (lst :forall (lambda (x) (> x 0))) => #t)
  (check (lst :forall (lambda (x) (> x 3))) => #f)
)

(let1 empty-lst (rich-list '())
  (check (empty-lst :forall (lambda (x) (> x 0))) => #t))

(let1 l (rich-list '(1 2 3))
  (check-true (l :exists even?)))

(let1 l (rich-list '(1 2 3))
  (check-true (l :contains 1))
  (check-false (l :contains 4)))

(let ((lst (rich-list '(1 2 3 4 5))))
  (check (lst :take -1 :collect) => '())
  (check (lst :take 0 :collect) => '())
  (check (lst :take 3 :collect) => '(1 2 3))
  (check (lst :take 5 :collect) => '(1 2 3 4 5))
  (check (lst :take 10 :collect) => '(1 2 3 4 5))
)

(let ((lst (rich-list '(1 2 3 4 5))))
  (check (lst :take-right -1 :collect) => '())
  (check (lst :take-right 0 :collect) => '())
  (check (lst :take-right 3 :collect) => '(3 4 5))
  (check (lst :take-right 5 :collect) => '(1 2 3 4 5))
  (check (lst :take-right 10 :collect) => '(1 2 3 4 5))
)

(check ((rich-list (list 1 2 3)) :count) => 3)
(check ((rich-list (list 1 2 3)) :count (cut > <> 1)) => 2)

(let ((lst (rich-list '(1 2 3 4 5))))
  (check (lst :fold 0 +) => 15)
  (check (lst :fold '() (lambda (x acc) (cons x acc))) => '(5 4 3 2 1))

  (check (lst :fold-right 0 +) => 15)
  (check (lst :fold-right '() (lambda (x acc) (cons x acc))) => '(1 2 3 4 5))
)

(check (object->string (box '(1 2 3))) => "(1 2 3)")

(let1 l (rich-list (list 1 2 3))
  (check (l :make-string) => "123")
  (check (l :make-string " ") => "1 2 3")
  (check (l :make-string "[" "," "]") => "[1,2,3]")
  
  (check-catch 'wrong-number-of-args (l :make-string "[" ","))
  (check-catch 'type-error (l :make-string 123 "," "]"))
  (check-catch 'type-error (l :make-string "[" 123 "]"))
  (check-catch 'type-error (l :make-string "[" "," 123))
)

(check ((box #(1 2 3)) :apply 1) => 2)
(check ((box #(1 2 3)) 1) => 2)

(let ((vec (rich-vector #(1 2 3 4 5))))
  (check ((vec :find (lambda (x) (= x 3))) :get) => 3)
  (check ((vec :find (lambda (x) (> x 2))) :get) => 3)

  (check ((vec :find (lambda (x) (> x 10))) :empty?) => #t)

  (check ((vec :find even?) :get) => 2)

  (check ((vec :find (lambda (x) (< x 0))) :empty?) => #t)
)

(check-true ((box #(1 2 3)) :equals (box #(1 2 3))))

(let ((vec (rich-vector #(1 2 3 4 5))))
  (check (vec :forall (lambda (x) (> x 0))) => #t)
  (check (vec :forall (lambda (x) (> x 3))) => #f))

(let ((empty-vec (rich-vector #())))
  (check (empty-vec :forall (lambda (x) (> x 0))) => #t))

(let ((vec (rich-vector #(1 2 3 4 5))))
  (check (vec :take -1 :collect) => #())
  (check (vec :take 0 :collect) => #())
  (check (vec :take 3 :collect) => #(1 2 3))
  (check (vec :take 5 :collect) => #(1 2 3 4 5))
  (check (vec :take 10 :collect) => #(1 2 3 4 5))
)

(let ((vec (rich-vector #(1 2 3 4 5))))
  (check (vec :take-right -1 :collect) => #())
  (check (vec :take-right 0 :collect) => #())
  (check (vec :take-right 3 :collect) => #(3 4 5))
  (check (vec :take-right 5 :collect) => #(1 2 3 4 5))
  (check (vec :take-right 10 :collect) => #(1 2 3 4 5))
)

(let ((vec (rich-vector #(1 2 3 4 5))))
  (check (vec :fold 0 +) => 15)
  (check (vec :fold '() (lambda (x acc) (cons x acc))) => '(5 4 3 2 1))

  (check (vec :fold-right 0 +) => 15)
  (check (vec :fold-right '() (lambda (x acc) (cons x acc))) => '(1 2 3 4 5))
)

(check (object->string (box #(1 2 3))) => "#(1 2 3)")

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

