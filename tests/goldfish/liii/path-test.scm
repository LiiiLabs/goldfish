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

(import (liii path)
        (liii check)
        (liii os))

(check-set-mode! 'report-failed)

(check-true (path-absolute? (make-path #("/"))))
(check-true (path-absolute? (make-path #("/" "tmp"))))
(check-false (path-absolute? (make-path #("tmp"))))

(check (path->string (make-path #("/" "etc" "passwd"))) => "/etc/passwd")
(check (path->string (make-path #("/" "tmp" ""))) => "/tmp/")

(check (path-dir? ".") => #t)
(check (path-dir? "..") => #t)

(when (not (os-windows?))
  (check (path-dir? "/") => #t)
  (check (path-dir? "/tmp") => #t)
  (check (path-dir? "/no_such_dir") => #f))

(when (os-windows?)
  (check (path-dir? "C:/") => #t)
  (check (path-dir? "C:/no_such_dir/") => #f))

(check (path-file? ".") => #f)
(check (path-file? "..") => #f)

(when (os-linux?)
  (check (path-file? "/etc/passwd") => #t))

(when (not (os-windows?))
  (check-true (> (path-getsize "/") 0))
  (check-true (> (path-getsize "/etc/hosts") 0)))

(when (os-windows?)
  (check-true (> (path-getsize "C:") 0))
  (check-true (> (path-getsize "C:/Windows") 0))
  (check-true (> (path-getsize "C:\\Windows\\System32\\drivers\\etc\\hosts") 0)))

(let ((file-name "中文文件名.txt")
      (file-content "你好，世界！"))
  (define temp-dir (os-temp-dir))
  (define file-path (string-append temp-dir (string (os-sep)) file-name))
  (path-write-text file-path file-content)
  (check (path-read-text file-path) => file-content)
  (delete-file file-path))

(check-report)

