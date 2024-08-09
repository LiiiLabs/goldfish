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

(import (srfi srfi-78)
        (srfi srfi-1)
        (liii os))

(check-set-mode! 'report-failed)

(when (not (os-windows?))
  (check (file-exists? "/tmp") => #t)
  (check (file-exists? "/not_exists") => #f))

(when (os-linux?)
  (check (file-exists? "/root") => #f)
  (check (file-exists? "/root/.bashrc") => #f))

(when (os-windows?)
  (check (file-exists? "C:") => #t))

(when (os-linux?)
  (check
    (catch 'read-error
      (lambda () (delete-file "/root"))
      (lambda args #t))
    => #t))

(when (not (os-windows?))
  (with-output-to-file "/tmp/test_delete_file"
    (lambda ()
      (display "Hello, World!")))
  (check (file-exists? "/tmp/test_delete_file") => #t)
  (delete-file "/tmp/test_delete_file")
  (check (file-exists? "/tmp/test_delete_file") => #f))

(check-report)
(if (check-failed?) (exit -1))
