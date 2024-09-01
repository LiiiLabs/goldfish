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

(import (liii queue)
        (liii base)
        (liii check))

(check-set-mode! 'report-failed)

(let1 q1 (queue)
  (check-true (queue-empty? q1))
  (check (queue-size q1) => 0)
  (check-catch 'value-error (queue-pop! q1))

  (queue-push! q1 1)
  (check (queue-size q1) => 1)
  (check (queue-front q1) => 1)
  (check (queue-back q1) => 1)
  (check (queue-pop! q1) => 1)
  (check-true (queue-empty? q1))
)

(let1 q2 (queue 1 2 3)
  (check (queue-size q2) => 3)
  (check (queue-front q2) => 1)
  (check (queue-back q2) => 3)
  (check (queue-pop! q2) => 1)
  (check (queue-pop! q2) => 2)
  (check (queue-pop! q2) => 3)
)

(check-report)

