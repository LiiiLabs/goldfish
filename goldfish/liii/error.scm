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

(define-library (liii error)
(export error-file-not-found error-os error-not-a-directory
  error-file-exists)
(import (scheme process-context))
(begin

(define (error-file-not-found msg)
  (error 'file-not-found-error msg))

(define (error-os msg)
  (error 'os-error msg))

(define (error-not-a-directory msg)
  (error 'not-a-directory-error msg))

(define (error-file-exists msg)
  (error 'file-exists-error msg))

) ; begin
) ; define-library
