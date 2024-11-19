(import (liii check)
        (liii argparse))

;; Test string type with required type
(let ((parser (make-argparser)))
  (parser 'add-argument "name" 'string
          '((short . "n") (default . "anonymous")))
  (check (parser 'get-argument "name") => "anonymous")
  (parser 'parse-args '("--name" "john"))
  (check (parser 'get-argument "name") => "john"))

;; Test number type with long form
(let ((parser (make-argparser)))
  (parser 'add-argument "width" 'number
          '((short . "width") (default . 80)))
  (check (parser 'get-argument "width") => 80)
  (parser 'parse-args '("--width" "100"))
  (check (parser 'get-argument "width") => 100)
  (check (parser 'width) => 100)
  (parser 'parse-args '("-width" "60"))
  (check (parser 'get-argument "width") => 60))

;; Test number type with short form
(let ((parser (make-argparser)))
  (parser 'add-argument "height" 'number
          '((default . 60)))  ; without short name
  (parser 'parse-args '("--height" "120"))
  (check (parser 'get-argument "height") => 120))

;; Test mixed types
(let ((parser (make-argparser)))
  (parser 'add-argument "width" 'number
          '((short . "w") (default . 80)))
  (parser 'add-argument "title" 'string
          '((default . "Untitled")))
  (parser 'parse-args '("-w" "100" "--title" "My Document"))
  (check (parser 'get-argument "width") => 100)
  (check (parser 'get-argument "title") => "My Document"))

