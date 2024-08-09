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

(define-library (scheme file)
(export open-binary-input-file open-binary-output-file)
(import (liii os))
(begin
  
(define open-binary-input-file open-input-file)
(define open-binary-output-file open-output-file)

(define (file-exists? path)
  (if (string? path)
    (if (not (access path 'F_OK))
      #f
      (if (access path 'R_OK)
          #t
          (error 'permission-error (string-append "No permission: " path))))
    (error 'type-error "(file-exists? path): path should be string")))

(define (delete-file path)
  (if (not (string? path))
    (error 'type-error "(delete-file path): path should be string")
    (if (not (file-exists? path))
      (error 'read-error (string-append path " does not exist"))
      (g_delete-file path))))


) ; end of begin
) ; end of define-library

