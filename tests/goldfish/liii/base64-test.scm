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
        (liii base64))

(check (string-base64-encode "") => "")
(check (string-base64-encode "a") => "YQ==")
(check (string-base64-encode "z") => "eg==")
(check (string-base64-encode "f") => "Zg==")
(check (string-base64-encode "fo") => "Zm8=")
(check (string-base64-encode "foo") => "Zm9v")
(check (string-base64-encode "foob") => "Zm9vYg==")
(check (string-base64-encode "fooba") => "Zm9vYmE=")
(check (string-base64-encode "foobar") => "Zm9vYmFy")

(check-report)

