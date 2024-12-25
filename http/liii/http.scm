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

(define-library (liii http)
(begin
(export http-head http-ok?)

(define (http-ok? r)
  (let1 status-code (r 'status-code)
    (cond ((and (>= status-code 400) (< status-code 500))
           (error 'http-error
                  (string-append (integer->string status-code)
                                 " Client Error: {reason} for url: {self.url}")))
          ((and (>= status-code 500) (< status-code 600))
           (error 'http-error
                  (string-append (integer->string status-code)
                                 " Server Error: {reason} for url: {self.url}")))
          (else #t))))

(define* (http-head url)
  (let1 r (g_http-head url)
        (hash-table-set! r 'url url)
        r))

) ; end of begin
) ; end of define-library

