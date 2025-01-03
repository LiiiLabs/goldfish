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

(define-library (liii path)
(export
  make-path path-parts path-absolute?
  path->string
  path-dir? path-file? path-exists? path-getsize
)
(import (liii error) (liii vector) (liii string))
(begin

(define-record-type :path
  (%make-path parts type drive)
  path?
  (parts path-parts)
  (type path-type)
  (drive path-drive))

(define* (make-path parts (type 'posix) (drive ""))
  (when (not (vector? parts))
    (type-error "make-path: parts must be a vector"))
  (when (and (vector-empty? parts) (== type 'posix))
    (value-error "make-path: parts must not be emtpy for posix path"))
  (case type
    ((posix)
     (%make-path parts type drive))
    (else (value-error "make-path: invalid type" type))))

(define path-absolute?
  (typed-lambda ((path path?))
    (case (path-type path)
      ((posix)
       (string-starts? ((path-parts path) 0) "/"))
      (else (value-error "path-absolute?: invalid type of path" (path-type path))))))

(define path->string
  (typed-lambda ((path path?))
    (case (path-type path)
      ((posix)
       (string-append (string #\/)
         (string-trim (string-join (vector->list (path-parts path)) (string #\/)) #\/)))
      (else (value-error "path->string: invalid type of path" (path-type path))))))

(define (path-dir? path)
  (g_isdir path))

(define (path-file? path)
  (g_isfile path))

(define (path-exists? path)
  (file-exists? path))

(define path-getsize
  (typed-lambda ((path string?))
    (if (not (file-exists? path))
      (file-not-found-error
        (string-append "No such file or directory: '" path "'"))
      (g_path-getsize path))))

) ; end of begin
) ; end of define-library

