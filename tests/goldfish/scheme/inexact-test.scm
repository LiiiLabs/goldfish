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
        (scheme inexact))

(check-set-mode! 'report-failed)
(check (nan? +nan.0) => #t)
(check (nan? +nan.0+5.0i) => #t)
       
(check (nan? 32) => #f)
(check (nan? 3.14) => #f)
(check (nan? 1+2i) => #f)
(check (nan? +inf.0) => #f)
(check (nan? -inf.0) => #f)
       
(check-report)
