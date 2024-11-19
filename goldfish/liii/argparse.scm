(define-library (liii argparse)
  (import (liii base)
          (liii error)
          (liii list)
          (liii hash-table))
  
  (export make-parser)

  (begin
    ;; Internal argument record structure
    ;; (name type short-name default current-value)
    (define (make-arg-record name type short-name default)
      (list name type short-name default default))
    
    (define (get-option options key)
      (let ((pair (assoc key options)))
        (if pair (cdr pair) #f)))
    
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
    
    ;; Main parser constructor
    (define (make-parser)
      (let ((args-table (make-hash-table)))
        (lambda (command . args)
          (case command
            ;; Add new argument
            ((add-argument)
             (let* ((name (car args))
                    (type (cadr args))
                    (options (caddr args))
                    (short-name (get-option options 'short))
                    (default (get-option options 'default))
                    (arg-record (make-arg-record name type short-name default)))
               (unless (memq type '(number string))
                 (error "Type must be either 'number or 'string" type))
               (hash-table-set! args-table name arg-record)
               (when short-name
                 (hash-table-set! args-table short-name arg-record))))
            
            ;; Parse command line arguments
            ((parse-args)
             (let loop ((args (car args))
                       (current-arg #f))
               (if (null? args)
                   args-table
                   (let ((arg (car args)))
                     (cond
                       ;; Long form --name
                       ((and (string? arg) 
                             (>= (string-length arg) 3)
                             (string=? (substring arg 0 2) "--"))
                        (let* ((name (substring arg 2))
                               (found (hash-table-ref args-table name #f)))
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
                               (found (hash-table-ref args-table name #f)))
                          (if found
                              (if (null? (cdr args))
                                  (error "Missing value for argument" name)
                                  (begin
                                    (let ((value (convert-value (cadr args) (cadr found))))
                                      (set-car! (cddddr found) value))
                                    (loop (cddr args) #f)))
                              (error "Unknown argument" name))))
                       
                       (else (loop (cdr args) current-arg)))))))
            
            ;; Get argument value
            ((get-argument)
             (let ((found (hash-table-ref args-table (car args) #f)))
               (if found
                   (car (cddddr found))
                   (error "Argument not found" (car args)))))
            
            (else (error "Unknown parser command" command)))))))

  ;; End library definition
  )
