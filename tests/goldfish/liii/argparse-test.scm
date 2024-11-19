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
        (liii argparse))

(let ((parser (make-argparser)))
  (parser 'add-argument
    '((name . "name") (type . string) (short . "n") (default . "anonymous")))
  (check (parser 'name) => "anonymous")
  (parser 'parse-args '("--name" "john"))
  (check (parser 'name) => "john"))

;; Test number type with long form
(let ((parser (make-argparser)))
  (parser 'add-argument
    '((name . "width") (type . number) (short . "width") (default . 80)))
  (check (parser 'get-argument "width") => 80)
  (parser 'parse-args '("--width" "100"))
  (check (parser 'get-argument "width") => 100)
  (check (parser 'width) => 100)
  (parser 'parse-args '("-width" "60"))
  (check (parser 'get-argument "width") => 60))

;; Test number type with short form
(let ((parser (make-argparser)))
  (parser 'add-argument
    '((name "height") (type number) (default . 60)))  ; without short name
  (parser 'parse-args '("--height" "120"))
  (check (parser 'get-argument "height") => 120))

;; Test mixed types
(let ((parser (make-argparser)))
  (parser 'add-argument
    '((name . "width") (type . number) (short . "w") (default . 80)))
  (parser 'add-argument 
    '((name . "title") (type . string) (default . "Untitled")))
  (parser 'parse-args '("-w" "100" "--title" "My Document"))
  (check (parser 'get-argument "width") => 100)
  (check (parser 'get-argument "title") => "My Document"))

