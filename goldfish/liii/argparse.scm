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

(define-library (liii argparse)
(import (liii base)
        (liii error)
        (liii list)
        (liii string)
        (liii hash-table)
        (liii alist))
(export make-argparser)
(begin

(define (make-arg-record name type short-name default)
  (list name type short-name default default))

;; Convert value based on type
(define (convert-value value type)
  (case type
    ((number) 
     (if (number? value)
         value
         (let ((num (string->number value)))
           (if num 
               num
               (error "Invalid number format" value)))))
    ((string) 
     (if (string? value)
         value
         (error "Value is not a string")))
    (else (error "Unsupported type" type))))

(define (%add-argument args-ht args)
  (let* ((options (car args))
         (name (alist-ref options 'name
           (lambda () (value-error "name is required for an option"))))
         (type (alist-ref/default options 'type 'string))
         (short-name (alist-ref/default options 'short #f))
         (default (alist-ref/default options 'default #f))
         (arg-record (make-arg-record name type short-name default)))
    (unless (memq type '(number string))
             (error "Type must be either 'number or 'string" type))
    (hash-table-set! args-ht name arg-record)
    (when short-name
          (hash-table-set! args-ht short-name arg-record))))

(define (%get-argument args-ht args)
  (let ((found (hash-table-ref/default args-ht (car args) #f)))
    (if found
        (car (cddddr found))
        (error "Argument not found" (car args)))))

(define (%parse-args args-ht args)
  (let loop ((args (car args))
             (current-arg #f))
    (if (null? args)
        args-ht
        (let ((arg (car args)))
          (cond
            ;; Long form --name
            ((and (string? arg) 
                  (>= (string-length arg) 3)
                  (string-starts? arg "--"))
             (let* ((name (substring arg 2))
                    (found (hash-table-ref args-ht name)))
               (if found
                   (if (null? (cdr args))
                       (error "Missing value for argument" name)
                       (begin
                         (let ((value (convert-value (cadr args) (cadr found))))
                           (set-car! (cddddr found) value))
                         (loop (cddr args) #f)))
                   (error "Unknown argument" name))))
            
            ;; Short form -n
            ((and (string? arg)
                  (>= (string-length arg) 2)
                  (char=? (string-ref arg 0) #\-))
             (let* ((name (substring arg 1))
                    (found (hash-table-ref args-ht name)))
               (if found
                   (if (null? (cdr args))
                       (error "Missing value for argument" name)
                       (begin
                         (let ((value (convert-value (cadr args) (cadr found))))
                           (set-car! (cddddr found) value))
                         (loop (cddr args) #f)))
                   (error "Unknown argument" name))))
            
            (else (loop (cdr args) current-arg)))))))

(define (make-argparser)
  (let ((args-ht (make-hash-table)))
    (lambda (command . args)
      (case command
        ((add-argument) (%add-argument args-ht args))
        ((get-argument) (%get-argument args-ht args))
        ((parse-args) (%parse-args args-ht args))
        (else
          (if (and (null? args) (symbol? command))
              (%get-argument args-ht (list (symbol->string command)))
              (error "Unknown parser command" command)))))))

) ; end of begin
) ; end of define-library

