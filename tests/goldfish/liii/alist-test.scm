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
        (liii alist))

(check-set-mode! 'report-failed)

(check-true (alist? '()))

(check-true (alist? '((a 1))))
(check-true (alist? '((a . 1))))
(check-true (alist? '((a . 1) (b . 2))))

(check-false (alist? '(1 2 3)))

(check (alist-ref '((a 1)) 'a) => '(1))
(check (alist-ref '((a . 1)) 'a) => 1)
(check-catch 'key-error (alist-ref '(("a" . 1)) "a"))
(check-catch 'key-error (alist-ref '((a . 1)) 'b))

(check (alist-ref '((a . 1)) 'b (lambda () 2)) => 2)

(check (alist-ref/default '((a . 1)) 'b 2) => 2)

(check-report)

